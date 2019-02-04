### Statistics and Predictive Analytics

# Tutorial topic:  Clean and organize nasty dataset

# Load packages
library(dplyr)
library(ggplot2)

# Download and inspect "Salt_Lake_MSA_Occupational_Projections_2012-2022.csv"

wage <- read.csv("Salt_Lake_MSA_Occupational_Projections_2012-2022.csv",
                 stringsAsFactors = F)

# Inspect data
head(wage)
glimpse(wage)
View(wage)

# Note in Excel I transformed all non-text columns to numeric.

# Goal: clean and tidy and analyze dataset to answer question.

# Question:  Which occupations should you study now to have
# the best employment prospects in 10 years?  (What do we mean by
# "best"?)

# Problems, both with tidiness and cleanliness:
# 1. SOC code was brought in as a date.
# 2. The wage variables seem to be both hourly and annual.
# 3. Missing observations.
# 4. The grain of the observations is incompatible since there are summary rows.
#    The dataset is untidy.  


# Here’s a brief primer on our star rating methodology:
#   
# The star rating is based on both employment outlook and wages. Only
# occupations with employment of at least 100 in the base year are ranked. 
# In addition, residual occupational groups, which combine similar, small
# occupations, are not ranked. 

names(wage)
table(wage$Star.Rating)
subset(wage, Employment.Estimates.2012 < 100)

# Employment outlook is based 90 percent on the
# number of total Utah job openings projected between 2010 and 2020 and 10
# percent on how fast the occupation is expected to grow over that time period.
# The most recent surveyed median annual wages are used in the wage computation.
# There may be slight adjustments to the ratings based on an economist’s
# personal knowledge of the labor market. Ranked occupations are rated from zero
# to five stars.
# 
# Five-stars — strongest employment outlook and high wages.
# Four-stars — good employment outlook and relatively high wages.
# Three-stars — moderate-to-strong employment outlook and low-tomoderate wages.
# Two-stars — relatively high wage but a limited employment outlook.
# One-star — relatively low wage and strong employment outlook.
# Zero-stars — limited employment outlook and low wages.

subset(wage, Star.Rating=="5") %>%
select(Occupation.Title)

# 1. Remove summary rows.  
# Claim:  all rows without a star rating are summary rows.  
# Summary rows also have SOC codes that end in 00.

wage <- wage %>%
  dplyr::filter(Star.Rating != "")

subset(wage, substr(SOC.Code, 6, 7)=="00") 


# 2.  Turn hourly pay into annual salary. Transfrom Hourly.Wages.Median.

table(wage$Hourly.Wages.Median)
names(wage)
wage <- wage %>%
  mutate(new_wage =  ifelse(Hourly.Wages.Median < 100,
                            Hourly.Wages.Median*40*50,
                            Hourly.Wages.Median))

summary(wage$new_wage)
hist(wage$new_wage)
hist(log(wage$new_wage))


# 3. Summarize wages by occupation

wage %>%
  group_by(Occupation.Title) %>%
  summarize(mean_wage = mean(new_wage)) %>%
  arrange(desc(mean_wage))
  head

# 4. Summarize job growth rate (Annual.Growth.Rate) by occupation and salary
# (In addition:  consider only salary > 80k)
  
  wage %>%
    arrange(desc(Annual.Growth.Rate)) %>%
    select(Occupation.Title, Annual.Growth.Rate, new_wage) %>%
    head
  
  wage %>%
    arrange(desc(Annual.Growth.Rate)) %>%
    select(Occupation.Title, Annual.Growth.Rate, new_wage) %>%
    filter(new_wage > 80000) %>%
    head

# 5. Summarize and plot salary by education
  names(wage)

wage %>%
  group_by(Education) %>%
    summarize(salary = mean(new_wage, na.rm= T)) %>%
    ggplot(aes(reorder(Education, salary), salary)) +
    geom_bar(stat= "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Education",
       title = "Salary by Education")


