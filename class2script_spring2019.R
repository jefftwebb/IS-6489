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

# Take a look

table1
table2

# First, dplyr verb review with examples:

# Create fake data for practicing: 100 people, each belonging to one of four groups, having 
# undergone two tests, with a recorded result.

set.seed(123)
df <- data.frame(ID = c(seq(1:100), seq(1:100)),
                 Group = rep(c(rep("a", 25),
                           rep("b", 25),
                           rep("c", 25),
                           rep("d", 25)),2),
                 Test = c(rep(1, 100), rep(2, 100)),
                 Result = runif(200, min = 10, max = 100))

head(df)

# Mutate:  creates a new column (preserves the original dataset)
df %>%
  mutate(rounded_result = round(Result)) %>%
  head

# Summarize:  creates a summary table (creates a new table)
df %>%
  summarize(mean = mean(Result),
            median = median(Result),
            sd = sd(Result),
            n = n())

# Arrange: sorts the dataset.
df %>%
  arrange(Result) %>%
  head

# Filter:  subsets rows.
df %>%
  filter(Result > 50) %>%
  head

# Select:  subsets columns
df %>%
  dplyr::select(ID, Test, Result) %>%
  head

# Group by:  perform verb operations by a grouping variable.
df %>%
  group_by(Group) %>%
  summarize(mean = mean(Result),
            median = median(Result),
            sd = sd(Result),
            n = n())

### PRACTICE ###
# Table 1 is tidy and is easy to work with. 

# TASK 1: Use dplyr's mutate() verb to calculate the rate per 10,000 of
# tuberculosis for each country (cases/population) in table 1. 2-3 minutes.


# TASK 2: Now create a new table that computes the average rate
# per country, sorted by average rate. 2-3 minutes.

# TASK 3: Create a new table that  computes the average rate per
# year. 2-3 minutes.

### PROBLEM ###
# Table 2 is messy and is not easy to work with. The problem
# is that variables are stored in rows. The grain of the
# data (what constitutes an observation) is a country-year
# combination.  Work with people sitting around you to
# calculate the rate per 10,000 of tuberculosis for each
# country.  Give it your best shot for 5 minutes.

# Additional dplyr commands that may be useful (wth examples):
# We'll use this filtered dataset for illustration:

df %>%
  filter(ID == 1)

# first() and last()

df %>%
  filter(ID == 1) %>%
  mutate(first_result = first(Result),
         last_result = last(Result))

# slice()
df %>%
  filter(ID == 1) %>%
  mutate(first_result = first(Result),
         last_result = last(Result)) %>%
  slice(1) %>%
  dplyr::select(-Result)


####################
### EDA Workflow ###
####################

# Example EDA workflow with simpson's data.  From the slides:

# After understanding the business context and the motivating business problem for the analysis:
  
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
