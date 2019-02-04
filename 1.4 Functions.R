### Statistics and Predictive Analytics

# Tutorial topic: Functions

# Read in the bike ridership dataset available at Canvas: "day.csv."
# Assign it to an object, d.

d <- read.csv("day.csv")

head(d)

# R is a functional language.  Functions offer prebuilt ways of accomplishing a
# task. In x + x, the "+" is actually a function. So is sum() in sum(x, x).

# For example, if we want to look at the first or last rows of a dataframe, we
# can use functions such as head() or tail(), which return the top and bottom 6
# observations respectively.

head(d)  
tail(d)

# How do you find information about a function?

?head

# Functions have arguments. In the case of head() there are two: x (the data)
# and n (the number or rows to select).  n has a default value of 6, meaning
# that if we don't include that argument then the function will use the default
# value. If we do supply a value for n, then that value will override the
# default.

head(d, n = 10)

# If you supply the argument in the order expected by the function then
# you don't have to name it:

head(d, 10)

# We can supply the arguments out of order, but only if we name them.

head(n = 10, x = d)

# Question:  Why doesn't the following code work?

head(10, d)

# We can use a function to look at the structure of a dataframe: str()

str(d) 

# The function, names(), returns the column names of a dataframe.

### PRACTICE
# Find the column names of d

