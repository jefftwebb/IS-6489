### Statistics and Predictive Analytics

# Tutorial topic:  ggplot2 

# Load packages
library(dplyr)
library(ggplot2)

# Get cheat sheets for both dplyr and ggplot2 at Rstudio:  
# https://www.rstudio.com/resources/cheatsheets/ 

# Let's make up some meaningless data for illustration purposes.
set.seed(123)
df <- data.frame(y = rnorm(1000, mean = 0, sd = 1), 
                 x1 = rpois(n = 1000, lambda = 5), # poisson distribution
                 x2 = sample(LETTERS[1:10], size = 1000, replace = T), # categorical
                 x3 = rnorm(n = 1000, mean = rnorm(1000, mean = 0, sd = 1), sd = 3)) # normal

head(df)


# ggplot syntax:
# 1. base layer:  ggplot(data, aes(x, y)) +
# 2. geom layer: geom_point()
# 3. summary layer: stat_smooth()
# 4. label layer:  labs()

# EXERCISE:  Make a scatter plot of the relationship between x3 and y.
# Add a summary regression line and title.

# some other plots: density, histogram

ggplot(df, aes(y)) +
  geom_density()

ggplot(df, aes(x3)) +
  geom_histogram(bins=10)


# EXERCISE: Make a boxplot of the relationship between x2 and y.

# EXERCISE: Facet the scatter plot you made above by the 
# groups in x2.

