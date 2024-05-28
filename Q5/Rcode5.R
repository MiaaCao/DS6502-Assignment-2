install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

getwd()
setwd("C:/Users/liuch/Desktop/assignment/DS6502 assignment2/DS6502-Assignment-2/Excel File")
data <- read.csv("Infectious Disease 2001-2014.csv")


######################### Q5 (done) ##############################
# Correlation between two variables
# Pearson’s r is correlation coefficient which indicates how strong the correlation is
# use the cor() function to calculate the Pearson correlation coefficient, and create a scatter plot

# subset the dataset for Chlamydia and Salmonellosis in California
chlamydia_data <- subset(data, Disease == "Chlamydia" & County == "California" & Sex == "Total")
salmonellosis_data <- subset(data, Disease == "Salmonellosis" & County == "California"& Sex == "Total")

# Calculate the correlation between Chlamydia and Salmonellosis rates
correlation <- cor(chlamydia_data$Rate, salmonellosis_data$Rate)

# Print the correlation coefficient
cat("Correlation between Chlamydia and Salmonellosis rates in California:", correlation)

# Create a scatter plot to visualize the relationship
ggplot() +
  geom_point(data = chlamydia_data, aes(x = Year, y = Rate, color = "Chlamydia")) +
  geom_point(data = salmonellosis_data, aes(x = Year, y = Rate, color = "Salmonellosis")) +
  labs(x = "Year", y = "Rate", 
       title = "Correlation between Chlamydia and Salmonellosis Rates in California") 


# Calculate the standard deviation to check how scattering 
chlamydia_std_dev <- sd(chlamydia_data$Rate)
cat("Standard Deviation of Chlamydia Rates:", chlamydia_std_dev)

salmonellosis_std_dev <- sd(salmonellosis_data$Rate)
cat("Standard Deviation of Salmonellosis Rates:", salmonellosis_std_dev)




