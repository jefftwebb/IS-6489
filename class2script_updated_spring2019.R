###########################################
### Statistics and Predictive Analytics ###
###########################################

# Class 2 Script

# Topics:
# 1. Tidy data
# 2. EDA workflow
# 3. Practice EDA workflow

library(tidyverse)

#################
### Tidy data ###
#################

# Illustrations of kinds of messiness and some practice
# using dplyr code, along with some additional functions
# from the tidyr package.

# Load example datasets (included in tidyverse package).

data(table1)
data(table2)
data(table3)
data(table4a)
data(table4b)

# Take a look

table1
table2
table3
table4a
table4b

### PROBLEM ###
# Table 1 is tidy and is easy to work with. Use dplyr's
# mutate() verb to calculate the rate per 10,000 of
# tuberculosis for each country (cases/population) in table
# 1. 2-3 minutes.

table1 %>%
  mutate(rate = cases/ population *10000)


# Now create a new table that computes the average rate
# per country.

table1 %>%
  mutate(rate = cases/ population *10000) %>%
  group_by(country) %>%
  summarize(mean_rate = mean(rate)) %>%
  arrange(mean_rate)

# Create a new table that  computes the average rate per
# year.

table1 %>%
  mutate(rate = cases/ population *10000) %>%
  group_by(year) %>%
  summarize(mean_rate = mean(rate))

### PROBLEM ###
# Table 2 is messy and is not easy to work with. The problem
# is that variables are stored in rows. The grain of the
# data (what constitutes an observation) is a country-year
# combination.  Work with people sitting around you to
# calculate the rate per 10,000 of tuberculosis for each
# country.  Give it your best shot for 5 minutes.

table2 %>%
  group_by(country, year) %>%
  mutate(cases = first(count),
         population = last(count),
         rate = cases/population *10000) %>%
  group_by(country, year) %>%
  slice(1) %>%
  dplyr::select(-type, -count)

# Or more simply, using the spread() function from the tidyr
# package:

table2 %>%
  spread(key = type, value = count) %>%
  mutate(rate = cases/population *10000)


### DEMONSTRATION ###
# Table 3 is problematic because the rate is unevaluated and
# is stored as a character.  This violates the principle of
# one value per cell. What to do? Use the separate() function
# from the tidyr package.

str_split(table3$rate, pattern = "/", simplify = T)[, 1]

table3 %>%
  mutate(cases = str_split(rate, pattern = "/", simplify = T)[,1],
         population = str_split(rate, pattern = "/", simplify = T)[,2],
         cases = as.numeric(cases),
         population = as.numeric(population))

### DEMONSTRATION ###
# Tables 4a and 4b are problematic because cases and
# populations are stored in different tables and
# observations of the year variable appear as columns.
# Use the gather() function from tidyr.

####################
### EDA Workflow ###
####################

# Example EDA workflow with simpson's data.  From the slides:

# 1. Formulate a question 
# 2. Read in your data 
# 3. Check the packaging 
# 4. Inspect dataset:  str(), glimpse(), View()
# 5. Look at the top and the bottom of your data 
# 6. Summarize the data 
# 7. Try the easy solution first 
# 8. Challenge your solution
# 9. Follow up questions

### DEMONSTRATION ###

# We'll use the simpsons data for this demonstration, which
# is available at Canvas.

# 1. Formulate a question:  What is the relationship between
# x and y?  (In a real analysis we would think of a more
# interesting question!)

# 2. Read in your data 

s <- read.csv("simpsons_data.csv", stringsAsFactors = F)[, -1]

# 3. Check the packaging 

dim(s)
nrow(s)
ncol(s)

# 4. Inspect the dataset: str(), glimpse(), View()

str(s)
glimpse(s)
View(s)

# 5. Look at the top and the bottom of your data 

head(s)
tail(s)

# 6. Summarize the data

summary(s)
table(s$group)

hist(s$y)
hist(s$x)

s %>%
  ggplot(aes(y, col= group)) +
  geom_density()

s %>%
  ggplot(aes(x, col= group)) +
  geom_density()

# 7. Try the easy solution first 

ggplot(s, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se= F, col = 2)

# 8. Challenge your solution

ggplot(s, aes(x, y, col = group, group = group)) +
  geom_point() +
  geom_smooth(method = "lm", se= F, col = 1) +
  labs(title = "simpson's paradox:  y ~ x, varying by group")

# 9. Follow up questions

####################
### Practice EDA ###
####################

# Download the bikeshare data from Canvas (day.csv).  With 2
# or 3 people  sitting around you, practice the EDA
# workflow, starting with formulating an interesting
# question. For example, perhaps you are interested in the
# difference between perceived temperature (atemp) and
# measured temperature (temp). Are there variables in the
# data set that predict that difference? Or perhaps you
# would like to find out whether bike usage differs on
# weekends vs. weekdays and whether that difference is
# influenced by temperature or weather. Create a
# visualization that summarizes the answer to your question.
# Each group should email me, through Canvas, 1. your code
# for the visualization, 2. at least one follow up question,
# and 3. a screenshot of the resulting plot. If we have
# time, I will pull up the plots, the code, and the question
# for discussion.

d <- read.csv("day.csv")

# Question 1.
# temperaure difference predicted by ...

# 1. humidity

d %>%
  mutate(temp_diff = temp - atemp) %>%
  filter(temp_diff < .4) %>%
  ggplot(aes(hum, temp_diff)) +
  geom_point()

# 2. weathersit

d %>%
  mutate(temp_diff = temp - atemp) %>%
  filter(temp_diff < .4) %>%
  ggplot(aes(factor(weathersit), temp_diff)) +
  geom_boxplot()

# 3. windspeed

d %>%
  mutate(temp_diff = temp - atemp) %>%
  filter(temp_diff < .4) %>%
  ggplot(aes(windspeed, temp_diff)) +
  geom_point() +
  geom_smooth(se = F)

# Question 2
# Does ridership differ by weekday vs. weekend?

d %>%
  ggplot(aes(factor(workingday), cnt)) +
  geom_boxplot()

d %>%
  ggplot(aes(factor(workingday), cnt)) +
  geom_boxplot() + 
  facet_wrap(~weathersit)
