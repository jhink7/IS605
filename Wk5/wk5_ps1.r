#### Problem Set 1 ####

# set up our data as per assignment spec
A = c(1,1,1,1,0,1,3,4)
dim(A) <- c(4,2)

b = c(0,8,8,19)
dim(b) <- c(4,1)

p = c(1,5,13,17)
dim(p) <- c(4,1)

ata = t(A) %*% A

atb = t(A) %*% b

# solve modified equation (A^t)Ax = (A^t)b
x1 = solve(ata,atb)
x1

# compute the error vector
e1 = b - A %*% x1

# now using p instead of b
atp = t(A) %*% p

# solve modified equation (A^t)Ax = (A^t)p
x2 = solve(ata,atp)
x2

# compute the error vector (should be 0)
e2 = p - A %*% x2 
e2

# show that b - p = [-1; 3;-5; 2]
e = b - p
e

# show orthagonality, ie dot product = 0
te2 = t(e2)
isOrth = (te2 %*% p == 0) & (te2 %*% A[,1] == 0) & (te2 %*% A[,2] == 0)
isOrth
