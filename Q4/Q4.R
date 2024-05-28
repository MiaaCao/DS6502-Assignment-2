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