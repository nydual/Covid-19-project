rm(list = ls()) # removes all variables stored previous
library(Hmisc) #import packages


data <- read.csv("~/Desktop/R projects/COVID19_line_list_data.csv")
describe(data) # Hmisc command to describe the data available

# Cleaning up death  column
data$death_dummy <- as.integer(data$death != 0)

# death rate 

sum(data$death_dummy) / nrow(data)
# Proofing that older people are more vulnerable to covid-19 than the youth
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE) # na.rm removes Na where age is unknown
mean(alive$age, na.rm = TRUE) # na.rm removes Na where age is unknown

# Is it statistically significant that older people are more vulnerable to Covid-19
#if the p-value is < 0.5, we reject the null hypothesis
#The results is -0 so we reject the null hypothesis and conclude that it is statistically significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)


#Checking if gender has effect on Covid-19

men = subset(data, gender == "male")  # the result show that 8.5%
women = subset(data, gender == "female") # the result show that 3.7%
mean(men$death_dummy, na.rm = TRUE) # na.rm removes Na where age is unknown
mean(women$death_dummy, na.rm = TRUE) # na.rm removes Na where age is unknown

t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)

# 99% confidence men has 0.8% higher fatality rate than women
#it's statistaclly significant that men have higher covid death rate than women according to the data
