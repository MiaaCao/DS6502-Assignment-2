setwd("C:/Users/miaca/OneDrive/桌面/DS6502/DS6502-Assignment-2/Excel File")

# Install package dplyr that used for the manipulation and display of data within a data frame
install.packages("dplyr") 
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Read dataset
data <- read.csv("Infectious Disease 2001-2014.csv")
data

# Filter the dataset for HIV data, 2005, and females
HIVFemaleData_2005 <- filter(data, data$Disease == "HIV" & 
                               data$Year == "2005" & data$Sex == "Female")

# Select County and Rate columns from the HIVFemaleData_2005 table
CountyRate <- HIVFemaleData_2005 %>% select(County, Rate)

# Plot the rates for each county
ggplot(CountyRate, aes(x = County, y = Rate, label = round(Rate, 0.5))) +
  geom_bar(stat = "identity") + 
  geom_text(vjust = -0.5, size = 2) +  # Adjust the vertical position of the labels
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "HIV Infection Rates Among Females by County (2005)",
       x = "County", y = "Infection Rate")