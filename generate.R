#! /bin/env Rscript

#Load the libraries that we are going to use
library(data.table)
library(dplyr)
library(magrittr)

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
               year = ryear(length(male)),
               id = 300000000 + 1:length(male),
               startYear = sample(startYear,length(male),replace=T),
               major = sample(studentMajors,length(male),replace=T)
                ) %>%
        rowwise %>%
        mutate(
                lastName = rname(surnames),
                firstName = ifelse(male == TRUE, rname(maleNames),rname(femaleNames)),
                email = paste0(firstName,".",lastName,"@podunk.edu")
                )

 

