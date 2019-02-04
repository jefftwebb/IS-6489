### Statistics and Predictive Analytics

# Tutorial topic: Getting data into R

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
