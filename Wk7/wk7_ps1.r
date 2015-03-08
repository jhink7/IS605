##### Problem Set 1 #####

#### Basic Scenario, Static input data, all fitting into memory ####
getMean <- function(x)
{
  sum = 0
  for(i in x)
  {
    sum = sum + i
    sum
  }
  
  mean = sum / length(x)
  return <- mean
}

getBasicStats <- function(x)
{
  u = getMean(x)
  
  devSqSum = 0
  
  for(i in x)
  {
    devSqSum = devSqSum + (i - u) ^ 2
  }
  
  # note, remove the -1 from the denominator if we are considering the input vector x
  # as the entire population and not a sample
  var = devSqSum / (length(x) - 1)
  
  std = sqrt(var)
  
  return <- list(mean = u, std = std)
}

input <- c(2,2,6,2,9,17)
stats = getBasicStats(input)

#compare to built in r functions
stats$mean == mean(input)
all.equal(stats$std, sd(input), 0.001)


#### Infinite Stream of Numbers ####
MAXNumSamples = .Machine$integer.max
numSamples = 0
rMean = 0
rDev = 0
getBasicStatsofStream <- function(x)
{

  # calculate a weighted average
  rMean <<- (numSamples * rMean + x) / (numSamples + 1)
  
  # keep a weighted average of the squared deviations from the mean
  # this is also the running variance
  rDev <<- (numSamples * rDev + (x-rMean)^2) / (numSamples + 1)
  
  # get the standard deviation
  stdDev = sqrt(rDev)
  
  # the maximum weight the current mean can have is the max integer as defined
  # by the system
  if(numSamples < MAXNumSamples)
  {
    numSamples <<- numSamples + 1
  }
  
  return <- list(mean = rMean, std = stdDev)
}

# get a stream of 10000 random numbers
stream <- runif(10000, 0, 10000)

# pass each number to our function and print out the running mean and sd
for(i in stream)
{
  print(getBasicStatsofStream(i))
}

mean(stream)

sd(stream)
