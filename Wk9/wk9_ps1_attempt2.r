library(ggplot2)

# First sampling function
sampleFn1 <- function(y)
{
  if(y >= 0 && y <=2)
  {
    # F(x) = x^2/2 - x + 1, x E (0, 1)
    #      = 2x-x^2/2-1, x E [1, 2)
    return <- ifelse(y < 0.5,sqrt(2*y),2-sqrt(2*(1-y)))
  }
}

# First sampling function
sampleFn2 <- function(y)
{
  if(y >= 0 && y <=2)
  {
    # F(x) = x^2/2 - x + 1, x E (0, 1)
    #      = -x^2/2+x-3/2, x E [1, 2)
    return <- ifelse(y < 0.5,1+sqrt(2*y +3),1-sqrt(2)*sqrt(-y-1))
  }
}


sample1 <- sapply(runif(10000),sampleFn1)
sample2 <- sapply(runif(10000),sampleFn2)


data <- data.frame(f1 = sample1, f2 = sample2)

## Plot pdfs
p1 <- ggplot(data, aes(x=f1)) + geom_histogram(binwidth=.05)
p1
p2 <- ggplot(data, aes(x=f2)) + geom_histogram(binwidth=.05)
p2

#### Q4 and Q5####

## function calculates the mean of the samples from a function
getSampleMeans <- function(n, pdf) {

  samples <- sapply(runif(n),pdf)
  
  #samples<-complete.cases(samples)
  
  return <- mean(samples, na.rm=TRUE)
}


# function used to verify the central limit theorem.
# n= number of pdf means, pdf = the pdf of the funciton we want to test
# numIterations = number of iterations to generate means for
verifyCLM <- function(n,pdf, numIterations)
{
  meanSamples <- vector(mode="numeric", length=0)
  
  #n = 10
  for(i in 1:numIterations)
  {
    meanSamples <- append(meanSamples,getSampleMeans(n, pdf))
  }
  
  meanData <- data.frame(m1 = meanSamples)
  
  return <- meanData
}

# Test first sampling function
clmData <- verifyCLM(20, sampleFn1, 1000)

p3 <- ggplot(clmData, aes(x=m1)) + geom_histogram(binwidth=.05)
p3

# Test second sampling function
clmData <- verifyCLM(20, sampleFn2, 1000)

p4 <- ggplot(clmData, aes(x=m1)) + geom_histogram(binwidth=.05)
p4
