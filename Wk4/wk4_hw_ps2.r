# function gets determinant of minor matrix
minor <- function(A, i, j) { det( A[-i,-j] ) }

# function gets cofactor of A based on indices i and j
cofactor <- function(A, i, j) { (-1)^(i+j) * minor(A,i,j) }

myinverse <- function(A)
{
  n <- nrow(A)
  C <- matrix(NA, n, n)
  
  # populate our cofactor matrix
  # note our "C" here is the transpose of the cofactor matrix
  # done to prevent another call to the t() function at the end
  for( i in 1:n )
    for( j in 1:n )
    {
      C[j,i] <- cofactor(A, i, j)
    }
  
  B <- (C) / det(A) 
  
  return(B)
}

A = c(4,2,5,2,2,2,1,3,4)
dim(A) <- c(3,3)

B = myinverse(A)
I = A %*% B

B2 = solve(A)

#check work
all.equal(B, B2, 0.00001)
