### Statistics and Predictive Analytics

# Tutorial topic:  Interpreting regression output and 
# using the model to predict

library(tidyverse)
library(arm) 

d <- read.csv("day.csv")

# Model

display(lm2 <- lm(cnt ~ hum + holiday + temp, data = d))


# Use the model to predict ridership for each row (the fitted values)
predict(lm2) %>% head #or
fitted(lm2) %>% head

# We can make a prediction for specific values of the predictors.



# Three methods. 1. Manual, 2. Index coefficients, 3. data.frame

# 1. Manual
# predicted ridership for avg temp and avg hum on non-holiday
summary(d$hum)
summary(d$temp)
table(d$holiday)

# 2689 - 2502*hum - 611*holiday + 6872*temp

2689 - 2502*.63 - 611*0 + 6872*.5

# predicted ridership for low temp and high humidity on non-holiday

2689 - 2502*.73 - 611*0 + 6872*.34

#2. Index coefficients
# model coefficients using coef(): (better--avoids rounding error)

coef(lm2)[1] + coef(lm2)[2]*.73 + coef(lm2)[3]*0 + coef(lm2)[4]*.34


# 3. Data frame method (easiest, and most precise):

predict(lm2, newdata = data.frame(hum = mean(d$hum),
                                  holiday = 0,
                                  temp = mean(d$temp)))

