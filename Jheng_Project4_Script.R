# Pei-Yu Jheng, 10/23/23, ALY6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(pacman)

data <- read.csv("C:\\Users\\user\\Desktop\\2023 Fall\\ALY6000\\Module 4\\Project 4\\train_jRxnrHD.csv")
class(data)
glimpse(data)


#Part I – Exploring

library("janitor")
data <- clean_names(data) #turn variables name to lower case
glimpse(data) #this function is used to see every column in a data frame.

#Renaming columns
colnames(data)[9] ="total_paid_premiums" #change variable's name
glimpse(data)

#Remove any rows that contain NAs
df <- data %>% drop_na()

#Correcting data types
str(df)
#The data types are all correct, so no need to change

#Removing columns
df = subset(df, select = -c(id, application_underwriting_score, target))

#Reorganizing the data
age <- function(x){
  y = floor(x/365)
  return(y)
} #define a function called 'age' to convert age in days to years old

df$age_in_days <- age(df$age_in_days)
colnames(df)[2] ="age" #change variable name to age

#Determine descriptive statistics for interesting variables
#max, min, mean, median, mode. standard deviation, variance, and range.
summary(df) #returns the minimum, maximum, mean, median, and 1st and 3rd quartiles for a numerical vector.
by(df, df$residence_area_type, summary) #print summary by group

mode(df$age)
sd(df$age)
var(df$age)
range(df$age)

mode(df$income)
sd(df$income)
var(df$income)
range(df$income)

mode(df$total_paid_premiums)
sd(df$total_paid_premiums)
var(df$total_paid_premiums)
range(df$total_paid_premiums)

median(df$residence_area_type)
mode(df$residence_area_type)
range(df$residence_area_type)

# the raw data visualization
library(ggplot2) 

ggplot(df) +
  aes(x = age) +
  geom_histogram(bins=30) #histogram of age range
#most of the people enrolled in this insurance premium are 40-60 years old.

ggplot(df) +
  aes(x = age) +
  geom_density() #Density plot of age
#most of the people enrolled in this insurance premium are around 50 year-old.

library(hrbrthemes)

ggplot(df, aes(x=income, y=count_3_6_months_late)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9)+
  theme_ipsum() +
  labs(title = "Income-related overdue premiums", x="Income(in thousands)", y="Number of late Payment")+
  theme(plot.title = element_text(size = 14))+
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3))
  #line chart of late premiums affected by income
#use raw data of variable: 3-6 months late to see how the income affects late premiums,
#it shows that the lower the income, the higher the number of late payments; 
#the higher the income, almost 0 late payments.
#It leads to the result that income is a factor that has a correlation.

ggplot(df) +
  aes(x = income, y = residence_area_type) +
  geom_point()+
  scale_color_hue() #scatter plot of income and region
#It demonstrates that Urban area are more likely to has the highest income group.

ggplot(df) +
  aes(x = sourcing_channel) +
  geom_bar() #bar chart of sourcing channel
#A is the sourcing channel have more than half of people use.


#Part II – Expanding
library(ggthemes)
library(ggeasy)
df$total_late <- df$count_3_6_months_late + df$count_6_12_months_late + df$count_more_than_12_months_late
#sum these three variables' integers I will get the total numbers of late pay premiums.
df$total_enrolled <- df$total_paid_premiums + df$total_late
#sum the total numbers of late pay premiums and the number of paid premiums I will get the total number of their enrolled premiums,
#so that I can calculate the percentage of non-pay premium below

df$perc_late <- floor((df$total_late / df$total_enrolled)*100)


#Question 1
#How does age affect the total times of overdue premiums? 
#I need a graph that is able to show me each case that falls into the age, 
#as well as the total number of late premiums, which makes the scatter plot the right fit.

ggplot(df,aes(x=perc_late,y=age,col=age))+geom_point()+ theme_tufte()+
  labs(title = "Overdue Payment Caused by Age", x = 'Percentage of Late Premiums', y ='Age')+
  scale_x_continuous(labels = scales::percent_format(scale = 1))


#Question 2
#What income level is more likely to have an overdue premium? 
#I use a line chart to present how the late premiums are affected by income

ggplot(df, aes(x=income, y=perc_late)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9)+
  theme_ipsum() +
  labs(title = "Overdue Premiums Caused by Income", x="Income(in thousands)", y="Percentage of late Payment")+
  theme(plot.title = element_text(size = 13))+
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3))+
  scale_y_continuous(labels = scales::percent_format(scale = 1))

df_10_perc <- filter(df, perc_late >= 10)#filter the percentage of late premiums bigger than or equal to 10%
#to see what is the income range of the percentage of late premiums bigger than 10%
max(df_10_perc$income)
#the highest income in the percentage of late premiums higher than 10% is $7,038,040. 
#People whose income level is between $24K and $7,100K are more likely not to pay on time.

df_50_perc <- filter(df, perc_late >= 50)#filter the percentage of late premiums bigger than or equal to 50%
#to see what is the income range of the percentage of late premiums bigger than or equal to 50%
max(df_50_perc$income)
#the highest income in the percentage of late premiums higher than 50% is $1,395,100. 
#People whose income level is between $24K and $1,400K have a 50% not paid on time history for their premiums purchased. 

#Question 3
#How significant is the region's impact on the premiums? 
#I created a bar chart to compare the difference in the premiums paid status by the area where people live,
#as well as a scatter plot of income and region.

ggplot(df, aes(x = residence_area_type, y = perc_late, fill = perc_late)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Overdue Premiums Caused by Region", x = "Region", y = "Percentage of late Payment", fill = "Payment Status") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()#Grouped Bar Chart

ggplot(df) +
  aes(x = perc_late, y = residence_area_type) +
  geom_point()+
  scale_color_hue()+
  labs(title = "Overdue Premiums Caused by Region", x = "Percentage of late premiums", y = "Region")+
  scale_x_continuous(labels = scales::percent_format(scale = 1))#scatter plot of income and region

#I have tried to use a Stacked Bar Chart, but the X-axis appears percentage larger than 100.
df_region <- df %>% select(c('residence_area_type','total_late','total_paid_premiums'))
df_region$total_late <- round(df$total_late/13*100,1)
df_region$total_paid_premiums <- round(df$total_paid_premiums/13*100,1)

new_df <- df_region %>% gather(key = "Category", value = "Percentage", -residence_area_type)
ggplot(new_df, aes(x = residence_area_type, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Overdue Premiums Caused by Region", x = "Region", y = "Percentage", fill = "Payment Status") +
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
