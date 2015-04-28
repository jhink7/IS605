library(stats)
library(boot)

setwd("C:/CUNY/IS605/Wk12")

# read data in
rawdata = read.table("auto-mpg.data")
colnames(rawdata) <- c("disp", "hp", "wt", "acc", "mpg")

cv.err5 <- vector()

degree = 1:8

for (i in degree ) {

  glm.fit=glm(mpg~poly(disp+hp+wt+acc,i), data=rawdata)
  set.seed(i*12223)
  cv.err5[i]=cv.glm(rawdata,glm.fit,K=5)$delta[1]
}

plot(degree,cv.err5,type='b')

# repeat for higher degree polynomials
# 11 degree polynomial

cv.err5 <- vector()

degree = 1:11

for (i in degree ) {
  
  glm.fit=glm(mpg~poly(disp+hp+wt+acc,i), data=rawdata)
  set.seed(i*12223)
  cv.err5[i]=cv.glm(rawdata,glm.fit,K=5)$delta[1]
}

plot(degree,cv.err5,type='b')

# repeat for higher degree polynomials
# 11 degree polynomial

cv.err5 <- vector()

# 15 degree polynomial
degree = 1:15

for (i in degree ) {
  
  glm.fit=glm(mpg~poly(disp+hp+wt+acc,i), data=rawdata)
  set.seed(i*12223)
  cv.err5[i]=cv.glm(rawdata,glm.fit,K=5)$delta[1]
}

plot(degree,cv.err5,type='b')


