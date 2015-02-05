#! /bin/env Rscript

#Load the libraries that we are going to use
library(data.table)
library(dplyr)
library(magrittr)
library(truncnorm)
library(lubridate)

#check if files exist
files=c("dist.all.last","dist.male.first","dist.female.first")
if(!all(files %in% list.files())){
  stop("Please download the data files from http://www.census.gov/topics/population/genealogy/data/1990_census/1990_census_namefiles.html\n")
}

#When does stuff happen        
getDate <- function(year,offset){
        marchDays = paste(year,3,1:31,sep="-") %>% as.POSIXct
        startofT1 = marchDays[marchDays %>% weekdays %>% `==`("Monday")][1]
        startofT1 + (as.numeric(offset) * 60*60*24) # add days
}

toCRN <- function(year, baseCRN) paste0(year - 2000, as.integer(baseCRN))

#The data files don't have colum names, but this is what they should be 
#Column names and data from http://www.census.gov/topics/population/genealogy/data/1990_census/1990_census_namefiles.html
nameDataColumns = c("Name", "Percent Frequency", "Percent Cumulative Frequency", "Rank")

#Read in the data files
surnames = read.table("dist.all.last",stringsAsFactors=FALSE) %>% 
        as.data.table %>% 
        setnames(nameDataColumns) %>% 
        mutate(prop = `Percent Frequency` / 100 + 0.0001) %>%
        mutate(prop = prop/sum(prop)) %>%
        mutate(sumProp = cumsum(prop)) %>%
        select(Name,sumProp)

maleNames = read.table("dist.male.first",stringsAsFactors=FALSE) %>% 
        as.data.table %>% 
        setnames(nameDataColumns) %>% 
        mutate(prop = `Percent Frequency` / 100 + 0.0001) %>%
        mutate(prop = prop/sum(prop)) %>%
        mutate(sumProp = cumsum(prop)) %>%
        select(Name,sumProp)

femaleNames = read.table("dist.female.first",stringsAsFactors=FALSE) %>% 
        as.data.table %>% 
        setnames(nameDataColumns) %>% 
        mutate(prop = `Percent Frequency` / 100 + 0.0001) %>%
        mutate(prop = prop/sum(prop)) %>%
        mutate(sumProp = cumsum(prop)) %>%
        select(Name,sumProp)


courseMajors = c("COMP","ENGR","MATH","STAT","PHYS")
studentMajors = c("COMP","SWEN","NWEN","ECEN","ELCO","BIOL","LAWS")


#Helper function to pick a random name from a list
rname <- function(names){
  prob = runif(1,0,1)
  (names %>%
   mutate(isMe = prob <= sumProp & prob > lag(sumProp,default=0)) %>%
   filter(isMe == TRUE))$Name
}

ryear <- function(n){
  sample(c(rep(1,4),rep(2,2),3),n,replace=T)
}

#At this point we need to generate a cohort of students to do our papers for us.
numStudents = 1000 #How many students do we want to have
pMale = 0.8        #Most students in Engineering are male
startYear = 2011:2014  #Our fake students are recent


students = data.table(male = replicate(numStudents, runif(1,0,1) < pMale)) %>%
        mutate(
               yearGroup = ryear(length(male)),
               studentID = 300000000 + 1:length(male),
               startYear = sample(startYear,length(male),replace=T),
               major = sample(studentMajors,length(male),replace=T),
               ability = rtruncnorm(n = length(male), a = 0)   #Each student has an inherent acadmic ability
                ) %>%
        rowwise %>%
        mutate(
                lastName = rname(surnames),
                firstName = ifelse(male == TRUE, rname(maleNames),rname(femaleNames)),
                email = paste0(firstName,".",lastName,"@podunk.edu")
                )

#Generate some courses
meanCoursesPerMajorPerYear = 3 
semesters = 1:2
meanAssignments = 7
meanTests = 1

makeCourses <- function(year, courseMajors){
lapply(courseMajors, function(x) {
       numCourses = rpois(1,meanCoursesPerMajorPerYear)
       if(numCourses == 0)  return(NULL)
       courses = data.table(courseNumber = (year* 100) + 1:numCourses,
                            yearLevel = year,
                            course = x, 
                            title = "AAA",
                            trimester = sample(semesters, numCourses, replace = T),
                            numberAssignments = rpois(1, meanAssignments),
                            numberTests = rpois(1, meanTests)
                            ) %>% mutate(courseCode = paste0(course,courseNumber))

                }) %>% 
                rbindlist
}

courses = lapply(1:3, function(year) makeCourses(year, courseMajors)) %>% 
        rbindlist %>%

        mutate(baseCRN = 1:length(course))


# These courses have assignments, tests and exams. All have a 40% or more exam. 

assignments = apply(courses, 1, function(course){
 courseCode = course[["courseCode"]]
 baseCRN = course[["baseCRN"]]
 yearLevel = course[["yearLevel"]]
 trimester = course[["trimester"]]
 as = course[["numberAssignments"]] %>% as.integer
 tests = course[["numberTests"]] %>% as.integer
 exams = 1

 availableMarks = 60 # Each course grade is comprised from 100% 
 examMarks = 40          #of these at least 40% are reserved for the exam

 #then we reserve at least 1% for each assignment
 availableMarks = availableMarks - as

 meanTestWeight = 15 #A good test can be worth some 15% or so.
 meanAssWeight = 8


 failCount = 0
 maxFails = 10 #give up on the randomness
 #Some of these remaining marks are distributed amoung the tests.
 testMarks = rep(100, tests)    #Initial marks distribution is obviously crazy
 while(sum(testMarks) > availableMarks & failCount < maxFails){
    testMarks = rpois(tests, meanTestWeight)   
    failCount = failCount + 1
 }

 if(sum(testMarks) > availableMarks) testMarks = rep(1,tests)

 availableMarks = availableMarks + as - sum(testMarks)  # return the reserved assignment marks and remove the test marks


 failCount = 0
 assMarks = rep(100,as)
 while(sum(assMarks) > availableMarks & failCount < maxFails){
   assMarks = rpois(as,meanAssWeight)
   failCount = failCount + 1
 }

 if(sum(assMarks) > availableMarks) assMarks = rep(1,as)

 availableMarks = availableMarks - sum(assMarks)  #Remove assignment marks form avaliable

 examMarks = examMarks + availableMarks  #All the remaining marks go the the exams


 #Create the assessment names
 assNames = NULL
 testNames = NULL
 examName = "exam"

 if(as > 0)    assNames  = sapply(1:as,    function(x) paste("Assignment",x))
 if(tests > 0) testNames = sapply(1:tests, function(x) paste("Test"      ,x))

 #Create the assessment times
 internalTimes = runif(as + tests, 0,14 * 7) %>% sort  #Any where in the teaching weeks
 examTime = 14 * 7 + runif(1,0,4*7) #Sometime in the exam period
 
 #Now we need to create the data.table which contains this information. 
 data.table(
            courseCode = courseCode,
            yearLevel = yearLevel,
            baseCRN = baseCRN,
            assessment = c(assNames, testNames, examName),
            times = c(internalTimes,examTime) + ifelse(trimester ==2, 20*7,0), #add offset for T2 start 
            marks = c(assMarks,testMarks, examMarks),
            difficulties = rtruncnorm(as+tests+1, 0)
            )
                }) %>% 
rbindlist

#Students enrol in courses and sit assessments

#This function computes a students score for an assement
sitAssessment <- function(skill, difficulty){
      e = exp(skill - difficulty)
      pRasch =  e / (1 + e)
      rtruncnorm(length(difficulty), 0, 100, pRasch * 100, 15)
}


# Each students sits some number of courses

assessmentMarks = apply(students, 1, function(student,courses,assignments){
 skill = student[["ability"]] %>% as.numeric
 id = student[["studentID"]] %>% as.numeric
 yearGroup = student[["yearGroup"]] %>% as.numeric
 startYear = student[["startYear"]] %>% as.numeric



 #Each student only does a subset of the courses, and only up to their current year level 
 coursesTaken = courses %>% 
        filter(yearLevel <= yearGroup) %>%
        group_by(yearLevel) %>%
        do(sample_n(.,rbinom(1,min(8,length(courseCode)),0.9))) %>%
        `[[`("courseCode")

 
 assignmentsTaken = assignments %>%
        filter(courseCode %in% coursesTaken) %>%
        mutate(mark = sitAssessment(skill, difficulties),
               studentID = id,
               yearTaken = as.integer(yearLevel) + startYear - 1,
               CRN = toCRN(yearTaken,baseCRN)
               )

return(assignmentsTaken)

}, courses,assignments) %>% 
        rbindlist %>%
        filter(yearTaken <= 2015) %>%
        mutate(mark = ifelse(yearTaken == 2015, as.numeric(NA), mark))




# Now we need to work out final grades for each student

asGrade <- function(mark){
  if(is.na(mark)) return(as.numeric(NA))
 if(mark>=90) return(95)
 if(mark>=85) return(87)
 if(mark>=80) return(82)

 if(mark>=75) return(77)
 if(mark>=70) return(72)
 if(mark>=65) return(67)

 if(mark>=60) return(62)
 if(mark>=55) return(57)
 if(mark>=50) return(52) 

 if(mark>=30) return(45)
 return(20)
}

finalMarks = assessmentMarks %>%
        group_by(courseCode, studentID) %>% 
        summarise(final = asGrade(sum(mark * marks/ 100)), Year = yearTaken) %>%
        merge(courses,by="courseCode") %>%
        mutate(Date = getDate(Year, ifelse(trimester == 1, 20*7,38*7)))



##### Formatting to match database 
titleCase <- function(s) paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "")


#Table 1: Course

dbTableCourse = assessmentMarks %>%
        unique(by=c("CRN")) %>%
        merge(courses, by=c("courseCode")) %>%
        mutate(Title= courseCode, 
               Year = yearTaken) %>%
        mutate(Trimester = trimester) %>%
        select(CRN,Title,Year,Trimester)

#Table 2: Students        
dbTableStudent = students %>%
        group_by(studentID) %>%
        summarise(FirstName = titleCase(firstName),
               LastName = titleCase(lastName),
               Exclude = FALSE, 
               Comment = NA,
               EmailAddress = email,
               Major = major)

#Table 3: Class List
dbTableClassList = assessmentMarks %>% 
        select(CRN, studentID) %>% 
        unique(by =c("CRN","studentID")) %>%
        mutate(RepeatStatus = FALSE, 
               Withdrawn = runif(length(CRN), 0 ,1) < 0.01, #Widthraw 1% of students from their courses
               ClassListID = 1:length(CRN)
        )

#Table 4: Assessment
dbTableAssessment = assessmentMarks %>% 
        group_by(CRN,assessment) %>%
        summarise(AssessmentTitle = unique(assessment), 
               Date = unique(getDate(yearTaken,times)),
               Weight = unique(marks),
               MaxMarks = 100,
               FailMark = 50,
               MarginalMark = 60,
               Mandatory = runif(1,0,1) < 0.2 #20% of assignments are mandatory
               ) %>%
        ungroup %>%
        select(-assessment)
        
dbTableAssessment = finalMarks %>%
        group_by(courseCode,Year) %>%
        summarise(AssessmentTitle = "Final",
               Mandatory = FALSE,
               MaxMarks = 100,
               FailMark = 50,
               MarginalMark = 60,
               Weight = 100,
               CRN =  unique(toCRN(Year,baseCRN)),
               Date = unique(Date)
               ) %>%
        ungroup %>%
        select(-courseCode, -Year) %>%
        rbind(dbTableAssessment,use.names=TRUE) %>%
        mutate(AssessmentID= 1:length(Weight))

#Table 5: Result
dbTableResult = assessmentMarks %>%
        mutate(AssessmentTitle = assessment) %>%
        merge(dbTableAssessment, by=c("CRN","AssessmentTitle")) %>%
        mutate(Result = ifelse(mark < FailMark,"F","P"),
               Mark = mark) %>%
        select(studentID, Mark,Result,AssessmentID) %>%
        filter(!is.na(Mark))

dbTableResult = finalMarks %>%
        mutate(AssessmentTitle = "Final",
               CRN = toCRN(Year,baseCRN),
              Mark = final 
               ) %>%
        merge(dbTableAssessment, by=c("AssessmentTitle","CRN")) %>%
        mutate(Result =  ifelse(final < FailMark,"F","P")) %>%
        select(Mark,Result,studentID,AssessmentID) %>%
        filter(!is.na(Mark)) %>%
        rbind(dbTableResult, use.names=TRUE) %>%
        mutate(ResultID = 1:length(Mark)) 



sink("sqlCommands.txt")       

#Write out the course table
cat("DELETE FROM Course;\n")
cat("DELETE FROM Student;\n")
cat("DELETE FROM ClassList;\n")
cat("DELETE FROM Assessment;\n")
cat("DELETE FROM Result;\n")

apply(dbTableCourse, 1, function(row){
         sprintf("INSERT INTO Course (CRN,Title,Year,Trimester) VALUES (%s,\"%s\",%s,%s);\n", 
                 row[["CRN"]], 
                 row[["Title"]],
                 row[["Year"]],
                 row[["Trimester"]])
        }) %>% cat

apply(dbTableStudent,1, function(row){
         sprintf("INSERT INTO Student (StudentID,FirstName,LastName,Exclude,Comment,EmailAddress,Major) VALUES (%s,\"%s\",\"%s\",%s,\"%s\",\"%s\",\"%s\");\n", 
                 row[["studentID"]], 
                 row[["FirstName"]],
                 row[["LastName"]],
                 ifelse(row[["Exclude"]]==TRUE,1,0),
                 row[["Comment"]],
                 row[["EmailAddress"]],
                 row[["Major"]])
        }) %>% cat

apply(dbTableClassList, 1, function(row){
         sprintf("INSERT INTO ClassList (StudentID,CRN,ClassListID,Withdrawn,RepeatStatus) VALUES (%s,%s,%s,%s,%s);\n", 
                 row[["studentID"]], 
                 row[["CRN"]],
                 row[["ClassListID"]],
                 ifelse(row[["Withdrawn"]]==TRUE,1,0) ,
                 ifelse(row[["RepeatStatus"]] == TRUE, 1,0)                 
                 )
        }) %>% cat


apply(dbTableResult, 1, function(row){
         sprintf("INSERT INTO Result (StudentID,AssessmentID,ResultID,Result,Mark) VALUES (%s,%s,%s,\"%s\",%s);\n", 
                 row[["studentID"]], 
                 row[["AssessmentID"]],
                 row[["ResultID"]],
                 row[["Result"]],
                 round(as.numeric(row[["Mark"]]),0)                 
                 )
        }) %>% cat


apply(dbTableAssessment, 1, function(row) {
         sprintf("INSERT INTO Assessment (CRN,AssessmentID,AssessmentTitle,Date,Weight,Mandatory,MarginalMark,MaxMarks,FailMark) VALUES (%s,%s,\"%s\",\"%s\",%s,%s,%s,%s,%s);\n", 
                 row[["CRN"]], 
                 row[["AssessmentID"]], 
                 row[["AssessmentTitle"]],
                 row[["Date"]],
                 row[["Weight"]],
                 ifelse(row[["Mandatory"]]==TRUE,1,0),
                 row[["MarginalMark"]],
                 row[["MaxMarks"]],
                 row[["FailMark"]])
        }) %>% cat

sink()
