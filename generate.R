#! /bin/env Rscript

#Load the libraries that we are going to use
library(data.table)
library(dplyr)
library(magrittr)
library(truncnorm)

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
               major = min(sample(studentMajors,length(male),replace=T),2011+yearGroup),
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

 #Now we need to create the data.table which contains this information. 
 data.table(
            courseCode = courseCode,
            yearLevel = yearLevel,
            baseCRN = baseCRN,
            assessment = c(assNames, testNames, examName),
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
        do(sample_n(.,rbinom(1,8,0.9))) %>%
        `[[`("courseCode")

 
 assignmentsTaken = assignments %>%
        filter(courseCode %in% coursesTaken) %>%
        mutate(mark = sitAssessment(skill, difficulties),
               studentID = id,
               yearTaken = as.integer(yearLevel) + startYear - 1,
               CRN = paste0(yearTaken - 2000, baseCRN)
               )

return(assignmentsTaken)

}, courses,assignments) %>% 
        rbindlist %>%
        filter(yearTaken <= 2015) %>%
        mutate(mark = ifelse(yearTaken == 2015, as.numeric(NA), mark))




# Now we need to work out final grades for each student

asGrade <- function(mark){
  if(is.na(mark)) return(as.character(NA))
 if(mark>=90) return("A+")
 if(mark>=85) return("A")
 if(mark>=80) return("A-")

 if(mark>=75) return("B+")
 if(mark>=70) return("B")
 if(mark>=65) return("B-")

 if(mark>=60) return("C+")
 if(mark>=55) return("C")
 if(mark>=50) return("C-") 

 if(mark>=30) return("D")
 return("E")
}

finalMarks = assessmentMarks %>%
        group_by(courseCode, studentID) %>% 
        summarise(final = asGrade(sum(mark * marks/ 100)))


