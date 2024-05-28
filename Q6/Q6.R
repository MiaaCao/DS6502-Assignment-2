# Set working directory
setwd("C:/Users/miaca/OneDrive/桌面/DS6502/DS6502-Assignment-2/Excel File")

# Install package dplyr that used for the manipulation and display of data within a data frame
install.packages("dplyr") 
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Read dataset
data <- read.csv("Infectious Disease 2001-2014.csv")
data

# Filter the dataset for Dengue cases in San Diego and San Francisco
DengueData <- data %>%
  filter(Disease == "Dengue" & (County == "San Diego" | 
                                  County == "San Francisco"))
DengueData

# Perform a two-sample t-test
t_TestResult <- t.test(Rate ~ County, data = DengueData)
t_TestResult

# Count the number of samples for each county
SamplesCount <- DengueData %>%
  group_by(County) %>%
  summarise(count = n())
SamplesCount

# The samples are not paired, as they are independently collected data 
# points from two different counties.