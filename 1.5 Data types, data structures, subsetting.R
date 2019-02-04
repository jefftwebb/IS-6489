### Statistics and Predictive Analytics

# Tutorial topic:   Data types, data structures, subsetting

# Read in the bike ridership dataset available at Canvas: "day.csv."
# Assign it to an object, d.

d <- read.csv("day.csv")

head(d)

###############################################################
### Data types
###############################################################

# Each column in d is a vector:  a collection of values with the same type. 
# Rows are not vectors because they have potentially different data types. 
# A vector is unidimensional.

# So, vectors are: 1) same data type, and 2) unidimensional.

# Let's look at the structure of d:

str(d)

# This dataset contains columns of three different data types: 
# 1. integer (int), 
# 2. factor, 
# 3. numeric (num). 

# In addition, R has logical and character data types.

# If we want to import the date column as a character, not a factor, then use 
# the following additional argument to read.csv(): stringsAsFactors = F.

d <- read.csv("day.csv", stringsAsFactors = F)

str(d) # That fixed it: date is now a string (or character) variable

# A factor variable is just a character variable with an encoded order.
# The season variable, 1-4, is encoded as an integer but should be a character 
# variable. Why?  There is no intrinsic order to seasons; they are all 
# just different.

# To pick out a single column from a table use $.

head(d$season)

# Now change season into a character variable.

d$season <- as.character(d$season)

str(d$season)

# Here we have simply overwritten the old variable with the new one.
# Notice that season is now integers 1-4 represented as characters.
# We could alternatively assign strings to season: "winter", "spring", 
# "summer", "fall". 

### QUESTION
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

# Another key data structure, discussed above, is a vector.  A single 
# column in a dataframe is a vector:

str(d$cnt) 

# In addition to data frames and vectors R also has the following data 
# structures: matrix, list, array. We'll skip these for now.

# A data frame consists of columns that are vectors and each column
# vector has the same number of observations.

length(d$season)==length(d$temp)

# The rows of a data frame are not vectors.  Why?  Because columns
# can have different data types, rows can also have different 
# data types.  As we've seen, vectors must have the same data type.

# QUESTION: 
# If we were to select just one row of a data frame,
# what would the resulting data structure be?  

head(d, n = 1)

str(head(d, n = 1)) # A data frame with one row!

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

d[1:10, 17] #is the same as
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

### PRACTICE 
# Subset d to include only season == 1 and cnt > 1000.  We use the
# double equal sign to indicate identity.

### PRACTICE
#Subset d to include only year = 0 (2011) and temp > .9

