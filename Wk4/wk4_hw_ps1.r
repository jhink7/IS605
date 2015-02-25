##### Problem Set 1 #####

# Setup the matrix
A = c(1,-1,2,0,3,4)
dim(A) <- c(2,3)  

# calculate X and Y as defined in the assignment spec

X = A %*% t(A)
Y = t(A) %*% A

# calculate eigen vectors
eigVecX = eigen(X)$vectors
eigVecY = eigen(Y)$vectors

# calculate eigen values
eigenValX = eigen(X)$values
eigenValY = eigen(Y)$values

svdA = svd(A)

# calculate singular values, left and right singular vectors
singValues = svdA$d
leftSingVectors = svdA$u
rightSingVectors = svdA$v

# From looking at the resulting calculations, we can see that eigVecX and leftSingVectors are equivalent 
# (proportional by a constant of either 1 or -1)

# From looking at the resulting calculations, we can see that eigVecY and rightSingVectors are equivalent 
# (proportional by a constant of either 1 or -1)

# the eigenValX and eigenValY are equivalent (26.6, 4.39).  Note that the 3rd eigen value for Y is very close
# to 0 and will be considered as such
trimmedEigenValY = head(eigenValY,2)
check <- all.equal(eigenValX, trimmedEigenValY, 0.00001)

# compare the square of the singular values of A to our eigen values of X and Y 
singValuesSquared = singValues^2

check <- all.equal(eigenValX, singValuesSquared, 0.00001)
check
check <- all.equal(trimmedEigenValY, singValuesSquared, 0.00001)
check




      