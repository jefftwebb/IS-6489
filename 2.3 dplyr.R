### Statistics and Predictive Analytics

# Tutorial topic:  dplyr

# Load packages
library(dplyr)
library(ggplot2)

# Get cheat sheets for both dplyr and ggplot2 at Rstudio:  
# https://www.rstudio.com/resources/cheatsheets/ 

# Let's make up some meaningless data for illustration purposes.
set.seed(123)
df <- data.frame(y = x, # use the x we already created as the outcome variable
                x1 = rpois(n = 1000, lambda = 5), # poisson distribution
                x2 = sample(LETTERS[1:10], size = 1000, replace = T), # categorical
                x3 = rnorm(n = 1000, mean = x, sd = 3)) # normal

head(df)

# dplyr select() creates a new dataframe by selecting columns.

# EXERCISE: use select() to create a dataframe with y, x1 and x2.
# Also reorder and rename, and save to a different object:
# first_p = x1, second_p = x3, outcome = y

# dplyr filter() subsets a dataframe by selecting rows according to 
# logical conditions.

# EXERCISE: use filter() to subset the data frame only for x2 = A, B or C
# Remember that == indicates logical identity, which is what you want here.
# And remember also that x2 is a character vector, so use quotation marks 
# when identifying elements.

# check that it worked

# dplyr mutate() creates a new variable (column).

# EXERCISE:  use mutate to create a new variable, x4, that is defined as x1 * x1

# check that it worked

# dplyr summarize() produces summary statistics.

# EXERCISE:  use summarize() to find the mean and standard deviation of x4.

# dplyr arrange() sorts a data frame.

# EXERCISE:  use arrange() to sort the dataframe by x2 then x3.
# Note:  use sort() for vectors.


# dplyr group_by() is essentially the same as SQL group by.  It will apply a
# function, such as mean or standard deviation, to a group, defined by a
# categorical variable. 

# EXERCISE:  Use summarize() and group_by() to find the
# mean of y for each group in x2.

