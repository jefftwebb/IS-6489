### Statistics and Predictive Analytics

# Tutorial topics: Summarizing data using descriptive statistics and plotting 

# Read in the bike ridership dataset available at Canvas: "day.csv."
# Assign it to an object, d.

# As we import, let's remove the extraneous first column using  
# square bracket subsetting

d <- read.csv("day.csv")[, -1] 

head(d)

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


