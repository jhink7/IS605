GetLU <- function(A){
  
  n = ncol(A)
  
  L = matrix(data=0, nrow=n, ncol=n)
  U = matrix(data=0, nrow=n, ncol=n)
  
  for(j in 1:n){
    L[j,j] = 1.0
    
    for(i in 1:j)
    {
      s1 = 0
      for(k in 1:i)
      {
        s1 = s1 + U[k,j] * L[i,k]
      }
      U[i,j] = A[i,j] - s1
    }
    
    for(i in j:n)
    {
      s2 = 0
      for(k in 1:j)
      {
        s2 = s2 +U[k,j] * L[i,k]
      }
      
      L[i,j] = (A[i,j] - s2) / U[j,j]
    }
    
    L[j,j] = 1.0
  }

  return <- list(L = L, U = U)
}



A <- c(1, 2, 1, 3, 4, 1, 5, 7, 0)
dim(A) <- c(3, 3)

result = GetLU(A)

result$L
result$U

result$L %*% result$U


manL = matrix(c(1,2,1,0,1,1,0,0,1), nrow=3)

manU = matrix(c(1,0,0,3,-2,0,5,-3,-2), nrow=3)
manL%*%manU

