### Live coding script
### IS 6489, class 2

# Topics:
# 1. Functions for basic statistics 
# 2. Magrittr piping syntax
# 3. dplyr
# 4. ggplot 
# 5. Clean and organize nasty dataset

# Simulate data from normal distribution and calculate statistics

set.seed(123)
x <- rnorm(1000, mean = 0, sd = 1)
head(x)

# Use R to calculate for x: 
# mean: mean()
# median: median()
# variance: var()
# standard deviation: sd()
# mininimum: min()
# maximum: max()
# range: range() 
# quartiles:  default setting for quantile()

# Note: these functions don't like missing values (NA)!

x_missing <- x
x_missing[c(1,10,20)] <- NA # replace some values with NAs

x_missing[1:20]

mean(x_missing) 
sd(x_missing)
median(x_missing)

# How would we fix this behavior?
# na.rm=T, na.omit()

### Magrittr piping (included in dplyr):  %>% 
library(dplyr)
library(ggplot2)

# The piping syntax is easier to read and understand than conventional syntax.
# It says:  Take the data before the pipe and do something:  
# 1. select, 
# 2. filter, 
# 3. mutate, 
# 4. summarize,
# 5. arrange,
# 6. group_by

# But we can use piping for any function.
# EXERCISE 1: Find the mean of x using pipes.


### Quick (and very basic) intro to dplyr and ggplot

# Get cheat sheets for both dplyr and ggplot2 at Rstudio:  
# https://www.rstudio.com/resources/cheatsheets/ 

# Let's make up some meaningless data for illustration purposes.
set.seed(123)
df <- data.frame(y = x, # use the x we already created as the outcome variable
                x1 = rpois(n = 1000, lambda = 5), # poisson distribution
                x2 = sample(LETTERS[1:10], size = 1000, replace = T), # categorical
                x3 = rnorm(n = 1000, mean = x, sd = 3)) # normal

head(df)

### dplyr (using piping syntax)

# dplyr select() creates a new dataframe by selecting columns.
# EXERCISE 2: use select() to create a dataframe with y, x1 and x2.

# reorder and rename, and save to a different object
# first_p = x1, second_p = x3, outcome = y


# dplyr filter() subsets a dataframe by selecting rows according to logical conditions.
# EXERCISE 3: use filter() to subset the data frame only for x2 = A, B or C
# Remember that == indicates logical identity, which is what you want here.
# And remember also that x2 is a character vector, so use quotation marks when identifying elements.

# check that it worked

# dplyr mutate() creates a new variable (column).
# EXERCISE 4:  use mutate to create a new variable, x4, that is defined as x1 * x1

#check that it worked

# dplyr summarize() produces summary statistics.
# EXERCISE 5:  use summarize() to find the mean and standard deviation of x4.

# dplyr arrange() sorts a data frame.
# EXERCISE 6:  use arrange() to sort the dataframe by x2 then x3.
# Note:  use sort() for vectors.


# dplyr group_by() is essentially the same as SQL group by.  It will apply a function,
# such as mean or standard deviation, to a group, defined by a categorical variable.
# EXERCISE 7:  Use summarize() and group_by() to find the mean of y for each group in x2.

### ggplot2

# ggplot syntax:
# 1. base layer:  ggplot(data, aes(x, y)) +
# 2. geom layer: geom_point()

# EXERCISE 8:  Make a scatter plot of the relationship between x3 and y.

# some other plots: density, histogram

ggplot(df, aes(y)) +
  geom_density()

ggplot(df, aes(x3)) +
  geom_histogram(bins=100)


# EXERCISE 9:  Make a boxplot of the relationship between x2 and y.

# EXERCISE 10: Facet the scatter plot you made above by the 
# groups in x2.

### Practice cleaning a dataset and asking questions.

# Download and inspect a real dataset:  "Salt_Lake_MSA_Occupational_Projections_2012-2022.csv"

wage <- read.csv("Salt_Lake_MSA_Occupational_Projections_2012-2022.csv")

# source:  https://opendata.utah.gov/Jobs/Salt-Lake-MSA-Occupational-Projections-2012-2022/xx3t-5qka/data

# Open this dataset in a spreadsheet and inspect.

# DISCUSSION:
# 1.  What problems do you see with this dataset?
# 2.  Assuming we can solve those problems what interesting questions could we investigate?

# Try to answer them.
