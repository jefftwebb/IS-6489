### IS 6489, class 1 tutorial script

# Topics:
# 1. A look around the Rstudio IDE 
# 2. Objects
# 3. Getting data into R
# 4. Functions
# 5. R Data types
# 6. R Data structures 
# 7. Subsetting
# 8. Summarizing data using descriptive statistics and plotting 

# Note:  We are jumping into the deep end in this class.  The homework at
# datacamp this week will provide a more systematic introduction to R.

###############################################################
### A look around Rstudio
###############################################################

# Download and install R and Rstudio if you have not already done so.

    # https://www.rstudio.com
    # https://www.r-project.org

# Rstudio environment:  starting in the upper right, clockwise:
    # Environment pane:  shows which object are in memory.
    # Plot pane:  displays plots created with code.
    # Console: allows you to run code by pressing return; code is not saved.
    # Source pane:  contains documents you are working on like this script.

# Install dplyr and ggplot2 packages:  Tools -> Install Packages

# Cheatsheets:  Help -> Cheatsheets

# Document types: scripts (like this), notebooks, RMarkdown
    # Scripts are just text files.  Email them to collaborators.
    # The hashtag indicates a comment that R should ignore.  
    # Run individual lines in the script by pressing control-return.
    # You can also select a portion of a script to run it.

# Projects!  (for file management).  Create IS-6489 project.
    # File -> New Project

###############################################################
### Objects in R
###############################################################

# The basis of R programming consists in assigning values to objects and
# manipulating those objects. Most simply, R is just a calculator

2 + 3

# That calculation runs in the console. But we can save the value in an object
# which is stored in memory. To do so we use the assignment operator, <-.  
# (= works too).

x <- 2 + 3

x

x + x

# There is always more than one way to do things in R! We can use a built in
# function also.

sum(x, x)

x * x

x^2 

y <- x + x

# Question:  What is the value of y?

y

###############################################################
### Getting data into R
###############################################################


# 1. Built in data

    data(mtcars)
    
    # mtcars is a multidimensional data structure known as a dataframe.
    # A dataframe is similar to a spreadsheet, with rows and columns,
    # where each column has the same number of 

# 2. Import with code

    # Load csv into your local folder (supply the path to the file if the file
    # is not in your working directory)
    
    d <- read.csv("day.csv")
    
    # Here we have read in day.csv with the read.csv() command, and have stored
    # it as an object, d, which is a dataframe.
    
    # How do you know what your working directory is?
    
    getwd()
    setwd()

# 3. Import with Rstudio.
    
    # Use Rstudio's "import dataset" button to browse for data on your computer.
    # This is a tab in the environment pane, upper right.
    
# 4. Retrieve from the web.
    
    a <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                  header = F)
    
    # To read from a webpage we simply supply the url.  The object, a, is now 
    # stored in memory as a dataframe.

### PRACTICE
# Import the "day.csv" dataset from Canvas:  Files => Class 1. Place it in your
# working directory: the IS 6489 project you created above. Then use read.csv to
# read that file from your working directory. Save the dataframe as an object, d
# (for "data").
    
###############################################################
### Functions
###############################################################
    
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

###############################################################
### Data types
###############################################################

# Each column in d is a vector:  a collection of values with the same type. Rows
# are not vectors; they have potentially different data types. A vector is
# unidimensional, whereas a data frame is 2 dimensional:  rows and columns.

# So, vectors are 1) same data type, and 2) unidimensional.

# Let's look again at the structure of d:

str(d)

# d contains columns three different data types: integer (int), Factor, numeric
# (num). In addition, R has logical and character data types.

# If we want to import the date column as a character, not a factor, then use 
# stringsAsFactors = F

d <- read.csv("day.csv", stringsAsFactors = F)

str(d) # That fixed it: date is now a string (or character) variable

# A factor variable is just a character variable with an encoded order.
# The season variable, 1-4, is encoded as an integer but should be a factor.

d$season <- factor(d$season)

str(d$season)

# The $ syntax picks out a single column from a table.  Here we are simply
# overwriting the old variable with the new factor. We could alternatively make
# it into a character variable (winter, spring, summer, fall) without any
# intrinsic order. Notice that the levels of season, though integers, are
# represented as characters.

### DISCUSSION
# How do we decide whether a variable like season should be character, factor,
# or integer?

# There is also a logical data type consisting in T and F (or TRUE and FALSE).
# Let's use the c() function to create a vector of logical values:

logical_vector <- c(T, F, F, T, T)
str(logical_vector)

# We can sum a logical vector
sum(logical_vector) # Counts the Ts.  Very handy.

###############################################################
### Data structures
###############################################################

# The workhorse data structure in R is the data frame,
# which is essentially just a table (think of an excel spreadsheet with 
# columns and rows).  Dataframes are very flexible and handy structures.

# Another key data structure is a vector.  A single column in a dataframe, as
# we've seen, is a vector:

str(d$cnt) 

# In addition to data frames and vectors R also has the following data structures:
# matrix, list, array. We'll skip these for now.

###############################################################
### Subsetting
###############################################################

# A key skill in working with data frames and vectors is indexing and
# subsetting. Square bracket notation, [ ], returns the vector values at the
# positions specified in the brackets:

logical_vector[2:3]

# This code says:  give me the second and third elements of the vector object,
# logical_vector.

# We use square bracket notation with a dataframe by adding a comma: [ , ]. The
# first value, before the comma, indicates the row position. The second value,
# after the comma, indicates the column position.

d[1:10,17] #is the same as
d$cnt[1:10] #is the same as
head(d$cnt, 10)

# All of these say the same thing:  return the first 10 observations of the 17th
# column (cnt) of d.

### PRACTICE
# Write code to return the last 20 values of the month and year variables.
# What is the resulting data structure?

# subset() is a handy function for filtering a data frame according to logical
# criteria. Here is how would use it to return the same subset as d[1:10, 17]

subset(d, select = "cnt", instant < 11)

# Note that one difference is that subset() returns a one column dataframe
# rather than a vector

### PRACTICE Subset d to include only season == 1 and cnt > 1000.  We use the
# double equal sign to indicate identity.

### PRACTICE
#Subset d to include only year = 0 (2011) and temp > .9

###############################################################
### Summarizing data
###############################################################

# Let's do some basic data exploration using built-in functions.  

# dim() summarizes the dimensions of the dataset
dim(d)
nrow(d)
ncol(d)

# summary() summarizes each variable in the dataset at once.
summary(d)

# min(), max(), median(), quantile()
min(d$cnt)
max(d$cnt)
median(d$cnt)
mean(d$cnt)
quantile(d$cnt) # The default setting in quantile() is quartiles
quantile(d$cnt, probs=c(0, .25,.5,.75, 1))

### DISCUSSION
# What are quartiles? quintiles? deciles?

# table() summarizes the counts in a distribution.
table(d$mnth)

# We can plot the results of table() with barplot().
# Always label axes and add a title!

barplot(table(d$mnth), main = "Observations in each Month")

# A histogram summarizes a numeric distribution by grouping similar values into
# "bins" and adjusting the height of the bars to reflect "frequency"--the
# number of observations in that bin.

hist(x = d$cnt, main = "histogram of riders")

# The "breaks" argument to the histogram function controls the number of bins.

hist(x = d$cnt, breaks = 100)
hist(x = d$cnt, breaks = 5)

# A boxplot summarizes a single continuous variable or the relationship 
# between a categorical (factor) variable and a continuous variable.

boxplot(x = d$cnt, main="Boxplot of riders", xlab="riders", ylab="count")

# A boxplot represents the same information as a histogram, with the box
# indicating the middle 50% of the distribution:
par(mfrow=c(2,1))
boxplot(d$cnt, horizontal = T)
hist(d$cnt, main="")
abline(v = quantile(d$cnt, probs = c(.25, .5, .75)), col = 2)
par(mfrow=c(1,1))

# Here is a boxplot organized by factor variable:
boxplot(d$cnt ~ d$season, 
        main = "Boxplot of riders by season", 
        xlab = "season", 
        ylab = "riders")

# A scatterplot summarizes the relationship between 2 continuous variables.
plot(x = d$temp, 
     y = d$cnt, 
     main = "Relationship between riders and temp")

# We can add more information by coloring the points by season.
plot(d$temp, 
     d$cnt, 
     main="Relationship between riders \n and temp by season", 
     col = d$season)

#which color = which variable??
str(d$season) #four level factor:  first level = 1, etc.  Maps to R's color scheme.
palette() #Hence:  "black"   "red"     "green3" "blue"

### DISCUSSION
# Imagine that cnt is our outcome variable--the variable we are trying to explain or
# predict.  

# 1. How would you describe the relationship between riders and season? (look at
# the boxplot). 
# 2. What prediction would you make for ridership in future seasons? 
# 3. How confident are you in your prediction?


