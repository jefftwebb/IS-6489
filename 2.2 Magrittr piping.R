### Statistics and Predictive Analytics

# Tutorial topic:  Magrittr piping syntax

# Load packages
library(dplyr) # dplyr includes magrittr
library(ggplot2)

# Magrittr pipe:  %>% 

# The piping syntax is easier to read and understand than conventional syntax.
# It says:  Take the data before the pipe and do something

# But we can use piping for any function.

# Simulate data to work with

set.seed(123)
x <- rnorm(1000, mean = 0, sd = 1)
head(x)

# EXERCISE: Find the mean of x using pipes.

# EXERCISE: add NAs to a dataset and then remove them, using pipes.

