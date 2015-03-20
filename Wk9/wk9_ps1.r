library(ggplot2)

# First sampling function
sampleFn1 <- function()
{
  x <- runif(1,0,2)
  
   if(x > 1)
   {
     x = 2 - x
   }
  
  return <- x
}

# Second sampling function
sampleFn2 <- function()
{
  x <- runif(1,0,2)
  
  if(x > 1)
  {
    x = x - 1
  }
  else
  {
    x = 1 - x
  }
  
  return <- x
}

fn1Samples <- vector(mode="numeric", length=0)
fn2Samples <- vector(mode="numeric", length=0)

# generate 1000 samples from each function
for(i in 1:1000)
{
  fn1Samples <- append(fn1Samples,sampleFn1())
  fn2Samples <- append(fn2Samples,sampleFn2())
}

data <- data.frame(f1 = fn1Samples, f2 = fn2Samples)

## Plot pdfs
p1 <- ggplot(data, aes(x=f1)) + geom_histogram(binwidth=.05)
p1
p2 <- ggplot(data, aes(x=f2)) + geom_histogram(binwidth=.05)
p2

#### Q4 and Q5####

## function calculates the mean of the samples from a function
getSampleMeans <- function(n, pdf) {
  samples <- vector(mode="numeric", length=0)
  for(i in 1:n)
  {
    samples <- append(samples,pdf())
  }
  
  return <- mean(samples)
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
