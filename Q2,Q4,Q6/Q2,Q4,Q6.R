################## Q2 #################

getwd()
setwd("C:/Users/miaca/OneDrive/桌面/DS6502/DS6502-Assignment-2/Excel File")

# Install package dplyr that used for the manipulation and display of data within a data frame
install.packages("dplyr") 
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

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


################## Q4 #################
 
# Filter the dataset for Tuberculosis cases from 2010 to 2012
TB_Data2010_2012 <- data %>%
  filter(Disease == "Tuberculosis" & Year >= 2010 & Year <= 2012 & data$Sex == "Total")

# Identify counties with no infected cases (Count == 0) for total category
NoCaseCounties <- TB_Data2010_2012 %>%
  filter(Count == 0) %>%
  select(County) %>%
  distinct()
NoCaseCounties 

# Identify counties with at least one case (Count > 0)
AtLeastOneCaseCounties <- TB_Data2010_2012 %>%
  filter(Count > 0) %>%
  select(County) %>%
  distinct()
AtLeastOneCaseCounties 

# Calculate the ratio
NoCaseCountyCount <- nrow(NoCaseCounties)
AtLeastOneCaseCountiesCount <- nrow(AtLeastOneCaseCounties)
TotalCountyCount <- length(unique(TB_Data2010_2012$County))

# Calculate ratio
RatioNoCaseToOthers <- NoCaseCountyCount / (TotalCountyCount - NoCaseCountyCount)
RatioNoCaseToOthers
# The ratio of counties with no Tuberculosis cases to those with at least one case is: 0.59

# Calculate correlation between the number of infected cases and population size
Correlation <- cor(TB_Data2010_2012$Count, TB_Data2010_2012$Population)
Correlation

# The correlation between the number of infected cases and the population size is:0.99

# Plot the data with zero cases highlighted
ggplot(TB_Data2010_2012, aes(x = County, y = Count, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count == 0, "0", "")), vjust = -0.3) +  # Highlight zero cases
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Tuberculosis Cases by County and Year (2010-2012)",
       x = "County",
       y = "Count of Cases",
       fill = "Sex")

################## Q6 (a) #################

# Filter the dataset for Dengue cases in San Diego and San Francisco
DengueData <- data %>%
  filter(Disease == "Dengue" & (County == "San Diego" | 
                                  County == "San Francisco"))
DengueData

# Perform a two-sample t-test
t_TestResult <- t.test(Rate ~ County, data = dengue_data)
t_TestResult

# Count the number of samples for each county
SamplesCount <- DengueData %>%
  group_by(County) %>%
  summarise(count = n())
SamplesCount

# The samples are not paired, as they are independently collected data 
# points from two different counties.