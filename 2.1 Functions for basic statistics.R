### Statistics and Predictive Analytics

# Tutorial topic:  Functions for basic statistics 

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

