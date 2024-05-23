######### Q2 #######
getwd()
setwd("C:/Users/miaca/OneDrive/桌面/DS6502/DS6502-Assignment-2/Excel File")

# Install package dplyr that used for the manipulation and display of data within a data frame
install.packages("dplyr") 
library(dplyr)
library(ggplot2)

data <- read.csv("Infectious Disease 2001-2014.csv")
data

# Filter the dataset dor HIV data, 2005, and females
HIVFemaleData_2005 <- filter(data, data$Disease == "HIV" & 
                               data$Year == "2005" & data$Sex == "Female")

# Select County and Rate columns
CountyRate <- HIVFemaleData_2005 %>% select(County, Rate)

# Plot the rates for each county
ggplot(CountyRate, aes(x = County, y = Rate, label = round(Rate, 0.5))) +
  geom_bar(stat = "identity") + 
  geom_text(vjust = -0.5, size = 2) +  # Adjust the vertical position of the labels
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "HIV Infection Rates Among Females by County (2005)",
       x = "County", y = "Infection Rate")

# In conclusion, in the year of 2005, San Francisco had the 
# highest rate of infected females for HIV, which is 11.2.


##################     Q4 #################
# Filter the dataset for Tuberculosis data, 2010 ~ 2012, and total
#TBTotaleData_2010_2012 <- filter(data, data$Disease == "Tuberculosis" & 
                                             #Year >= 2010 & Year <= 2012 & data$Sex == "Total")
#TBTotaleData_2010_2012 

# Filter the dataset for Tuberculosis cases from 2010 to 2012
tb_data_2010_2012 <- data %>%
  filter(Disease == "Tuberculosis" & Year >= 2010 & Year <= 2012 & data$Sex == "Total")

# Identify counties with no infected cases (Count == 0) for total category
no_case_counties <- tb_data_2010_2012 %>%
  filter(Count == 0) %>%
  select(County) %>%
  distinct()
no_case_counties #22
# In conclusion, Alpine, Amador, Butte, Calaveras, Colusa, Del Norte, Glenn,
# Inyo, Lassen, Mariposa, Mendocino, Modoc, Mono, Nevada, Plumas, San Benito,
# Shasta, Sierra, Siskiyou, Tehama, Trinity, Tuolumne had no case.

# Identify counties with at least one case (Count > 0)
at_least_one_case_counties <- tb_data_2010_2012 %>%
  filter(Count > 0) %>%
  select(County) %>%
  distinct()
at_least_one_case_counties #49

# Calculate the ratio
no_case_county_count <- nrow(no_case_counties)
at_least_one_case_county_count <- nrow(at_least_one_case_counties)
total_county_count <- length(unique(tb_data_2010_2012$County))

# Calculate ratio
ratio_no_case_to_others <- no_case_county_count / (total_county_count - no_case_county_count)
ratio_no_case_to_others
# The ratio of counties with no Tuberculosis cases to those with at least one case is: 0.59

# Calculate correlation between the number of infected cases and population size
correlation <- cor(tb_data_2010_2012$Count, tb_data_2010_2012$Population)
correlation
# The correlation between the number of infected cases and the population size is:0.99

# Plot the data with zero cases highlighted
ggplot(tb_data_2010_2012, aes(x = County, y = Count, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count == 0, "0", "")), vjust = -0.3) +  # Highlight zero cases
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Tuberculosis Cases by County and Year (2010-2012)",
       x = "County",
       y = "Count of Cases",
       fill = "Sex")

######################## Q6 (a) ##############################
# Q6: Over the whole period,
# (a) Are the rates of Dengue disease in San Diego and in San Francisco statistically 
# different?
#  - Which test should be used to investigate the statistical difference?
#  - How many samples in each set? 
#  - Are these sample paired?

# Filter the dataset for Dengue cases in San Diego and San Francisco
dengue_data <- data %>%
  filter(Disease == "Dengue" & (County == "San Diego" | 
                                  County == "San Francisco"))
dengue_data
# Use a two-sample t-test to compare the means of the rates between 
# San Diego and San Francisco.

# Perform a two-sample t-test
t_test_result <- t.test(Rate ~ County, data = dengue_data)
t_test_result
# By counting the number of observations for each county, we can determine 
# the sample size for each set.


# Count the number of samples for each county
samples_count <- dengue_data %>%
  group_by(County) %>%
  summarise(count = n())
samples_count

# The samples are not paired, as they are independently collected data 
# points from two different counties.