
getderivative <- function(x)
{
  #make h small
  h = 0.000000001
  
  # with h small, the below is an approximation to the limit
  # below equation comes from [f(x+h) - f(x)] / h
  retval = ((x + h)^3 + 2*(x+h)^2 - x^3 - 2*x^2) / h
  
  return (retval)
}

ans1 <- getderivative(2)
ans1

# check
# analytical form is 3x^2 + 4x
# with x = 2, that equals 20
# we are close

getIntegral <- function(min, max)
{
  stepsize = 1e-6
  sum = 0
  
  start = min
  
  while(start < max)
  {
    x = start + stepsize/2
    
    pointVal = 3*x^2 + 4*x
    
    area = pointVal * stepsize
    
    sum = sum + area
    
    start = start+stepsize
  }
  
  return (sum)
}

ans2 = getIntegral(1,3)

# check
# antiderivatice is x^3 + 2x^2
# when evaluated over (1,3) answer is 42
# we are close

