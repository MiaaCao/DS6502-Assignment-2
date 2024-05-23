install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
setwd("C:\\Users\\liuch\\Desktop\\assignment\\SD6502 assignment2\\DS6502-Assignment-2\\Excel File")

data <- read.csv("Infectious Disease 2001-2014.csv")

######################### Q1 (done) ##############################
#upward trend: the end point of 2014 is larger than the beginning point of 2001
#subset and group data, filter for Amebiasis cases
amebiasis_data <- subset(data, Disease == "Amebiasis" & Sex=="Total")

# Aggregate data by County and Year, summing the counts regardless of gender
aggregated_data <- aggregate(Count ~ County + Year, data = amebiasis_data, sum)

# Extract the counts for 2001 and 2012
data_2001 <- subset(aggregated_data, Year == 2001)
data_2014 <- subset(aggregated_data, Year == 2014)

# Merge the 2001 and 2012 data
trend_data <- merge(data_2001, data_2014, by = "County", suffixes = c("_2001", "_2014"))

# Identify counties with an upward trend
upward_trend_counties <- subset(trend_data, Count_2014 > Count_2001)

# Print the counties with an upward trend
print(upward_trend_counties$County)

# Plot the data for upward trend counties
upward_trend_aggregated_data <- subset(aggregated_data, County %in% upward_trend_counties$County)

ggplot(upward_trend_aggregated_data, aes(x = Year, y = Count, color = County)) +
  geom_line() +
  labs(title = "Amebiasis Disease Trend in Counties with Upward Trend (2001-2012)",
       x = "Year",
       y = "Total Count of Amebiasis Cases") +
  theme_minimal()


# plot a table
# Create a table plot using ggplot
table_data <- upward_trend_counties[, c("County", "Count_2001", "Count_2014")]
# Convert the table data to long format for ggplot
table_data_long <- reshape2::melt(table_data, id.vars = "County")
# Create the table plot
table_plot <- ggplot(table_data_long, aes(x = variable, y = County, label = value)) +
  geom_text(size = 5) +
  labs(title = "Counts for Counties with Upward Trend (2001 vs. 2014)", x = "Count", y = "County") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(table_plot)


####check the trend of male and female of these upward-trend  counties, compared to the total of that county
#San Diego
SanDiego_data <- data %>% filter(Disease == "Amebiasis", County == "San Diego")
ggplot(SanDiego_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
      geom_line() +
      geom_point() +
      labs(title = "Amebiasis Disease Count in San Diego County",
       x = "Year",
       y = "Count",
       color = "Sex") +
      theme_minimal()

#Kings
Kings_data <- data %>% filter(Disease == "Amebiasis", County == "Kings")
ggplot(Kings_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Kings County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#Madera
Madera_data <- data %>% filter(Disease == "Amebiasis", County == "Madera")
ggplot(Madera_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Madera County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#Mendocino
Mendocino_data <- data %>% filter(Disease == "Amebiasis", County == "Mendocino")
ggplot(Mendocino_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Mendocino County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#Merced
Merced_data <- data %>% filter(Disease == "Amebiasis", County == "Merced")
ggplot(Merced_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Merced County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#Riverside
Riverside_data <- data %>% filter(Disease == "Amebiasis", County == "Riverside")
ggplot(Riverside_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Riverside County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#San Benito
SanBenito_data <- data %>% filter(Disease == "Amebiasis", County == "San Benito")
ggplot(SanBenito_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in San Benito County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()

#Solano
Solano_data <- data %>% filter(Disease == "Amebiasis", County == "Solano")
ggplot(Solano_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Solano County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()


#Ventura
Ventura_data <- data %>% filter(Disease == "Amebiasis", County == "Ventura")
ggplot(Ventura_data, aes(x = Year, y = Count, color = Sex, group = Sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Amebiasis Disease Count in Ventura County",
       x = "Year",
       y = "Count",
       color = "Sex") +
  theme_minimal()




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




