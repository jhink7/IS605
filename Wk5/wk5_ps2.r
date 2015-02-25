setwd("C:/CUNY/IS605/Wk5")

#### Problem Set 2 ####

# read data in
rawdata = read.table("auto-mpg.data")

#rename columns if we need them later
colnames(rawdata) <- c("disp", "hp", "weight", "acc", "mpg")

A = subset(rawdata, select = c(disp, hp, weight, acc))
b = subset(rawdata, select = c(mpg))

# add intercept column
A$I  = 1

A = data.matrix(A)
b = data.matrix(b)

ata = t(A) %*% A
atb = t(A) %*% b

x = solve(ata, atb)
x
# the best fit line is: mpg = -0.006 * disp - 0.043 * hp - 0.005*weight - 0.02 * acc + 45.25

e = b - A %*% x

# calculate out the equation manually 
rawdata$predMPG = rawdata$disp * x[1] + rawdata$hp *x[2] + rawdata$weight *x[3]+ rawdata$acc * x[4] + x[5]

# manually calculate residuals
rawdata$res = rawdata$mpg - rawdata$predMPG 

# residuals as per matrix calc above
rawdata$res2 <- e

# ensure the 2 methods of calculating residuals are equivalent 
rawdata$resequal <- rawdata$res2 == rawdata$res
rawdata

#checking with R's built in functions shows we are correct
fit <- with(rawdata, lm(mpg~disp+hp+weight+acc))
summary(fit)
