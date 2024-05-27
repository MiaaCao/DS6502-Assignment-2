#Reading the csv file
data<- read.csv("./Infectious Disease 2001-2014.csv")

#installing the required packages
install.packages("dplyr")
install.packages("ggplot2")

#loading the packages in the library
library(dplyr)
library(ggplot2)

#Q7 (a)Are the rates of Cryptosporidiosis in California, Lake, San Diego, and San Francisco statistically different from each other (ignoring the year)? 

#To investigate whether the rates are statistically different, we can use an Analysis of Variance (ANOVA) test. ANOVA is suitable for comparing the means of more than two groups.
#filtering data for Cryptosporidiosis and the specified counties
crypto_data <- data %>%
  filter(Disease == "Cryptosporidiosis" & County %in% c("California", "Lake", "San Diego", "San Francisco"))

#performing ANOVA
anova_result <- aov(Rate ~ County, data = crypto_data)
summary(anova_result)

################################

#Q7 (b) Which of these countries are exactly different from each other? Which test did you use to determine this? 

#To find out which specific counties are different from each other, we can use a post-hoc test such as Tukey's Honest Significant Difference (HSD) test.
tukey_result <- TukeyHSD(anova_result)
tukey_result

################################

#Q7 (c)Create a suitable plot to indicate the changes in the rate ofCryptosporidiosis in these four counties.

#A line plot or a box plot can be used to visualize the changes in the rates over the years.
#creating a line plot
ggplot(crypto_data, aes(x = Year, y = Rate, color = County)) +
  geom_line() +
  geom_point() +
  labs(title = "Rates of Cryptosporidiosis in Selected Counties",
       x = "Year",
       y = "Rate") +
  theme_minimal()

