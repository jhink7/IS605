setwd("C:/CUNY/IS605/Wk11")

#### Part 1 ####

age = c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37)
maxHR = c(202, 186, 187, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178)

reg1 <- lm(maxHR~age)
reg1
# The resulting equation is maxHR = -0.7977*age + 210.0485

summary(reg1)

# The effect of age on maxHR is significant.  As seen in the above summary, it is significant to the 
# "0" level.  '***' in the lm significance code schema.

# plot
par(cex=.8)
plot(age,maxHR)
abline(reg1)

#### Part 2 ####

# read data in
rawdata = read.table("auto-mpg.data")
colnames(rawdata) <- c("disp", "hp", "weight", "acc", "mpg")

# get 40 random rows
subsample = rawdata[sample(nrow(rawdata), 40), ]

reg2 = lm(subsample$mpg~subsample$disp + subsample$hp + subsample$weight + subsample$acc)
reg2

summary(reg2)

# the standard errors can be viewed in the above summary.  
# Here, only the weight of the car seems to have a significant effect on mpg.  That is at the 0.01 (*) significance level
# Note that these (and the resulting fit equation variables) will change based on the the sample chosen.  Since a new subsample
# is chosen each time the code is run, the numbers you get will vary from those in the comments for this section.
#
# the resulting equation is:
# mpg = 0.009605 * disp - 0.028684 * hp - 0.006576 * weight + 0.417358 * acc  + 36.885116
#
# Confidence Intervals:
confint(reg2, level=0.95)

reg3 = lm(rawdata$mpg~rawdata$disp + rawdata$hp + rawdata$weight + rawdata$acc)
reg3

summary(reg3)

# the standard errors can be viewed in the above summary.  
# Here, the weight of the car AND the horsepower have a significant effect on mpg.  
# Weight is significant at the 0 (***) level while hp is at the 0.001 (**) level.
#
# the resulting equation is:
# mpg = -0.0060009 * disp -0.0436077 * hp -0.0052805 * weight -0.0231480 * acc  + 45.2511397
#
# Confidence Intervals:
confint(reg3, level=0.95)
