install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

setwd("C:/Users/liuch/Desktop/assignment/DS6502 assignment2/DS6502-Assignment-2/Excel File")
data <- read.csv("Infectious Disease 2001-2014.csv")

######################### Q3 (done) ##############################
#subset the data for Malaria cases in the year 2010
malaria_2010 <- subset(data, Disease == "Malaria" & Year == 2010)

#get the info when count >= 10
malaria_2010_more_than_10cases <- subset(malaria_2010, Count >= 10)
malaria_2010_more_than_10cases

#only print the counties whose count is over 10
malaria_2010_county_10<- unique(malaria_2010_more_than_10cases$County)
malaria_2010_county_10

#only print the info of county, sex is total, and the number of count
subset_malaria_2010 <- subset(malaria_2010_more_than_10cases, Sex == "Total", select = c("County", "Sex", "Count"))
subset_malaria_2010 

