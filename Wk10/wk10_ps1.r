
# Part 1
A <- c(0,0.5,0.5,0,0,0, 0,0,1,0,0,0, 0.25,0.25,0,0,0.25,0.25, 0,0,0,0,0.5,0.5, 0,0,0,0.5,0,0.5, 0,0,0.5,0.5,0,0)


dim(A) <- c(6, 6)

B = 0.85 * A + 0.15/6

# Part 2
r = data.matrix(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

k = 100
for(i in 1:k)
{
  r = B %*% r
}

# Part 3

# calculate eigen values
eigenValB = eigen(B)$values

# note the max of 1 here
eigenValB 

# calculate eigen vectors
eigVecB = eigen(B)$vectors[,1]

# scale the corresponding eigen vector by scalar value.  Also truncate the 0
# magnitude complex components
eigVecB = Re(eigVecB / -2.307201787)

# sum 
sum(eigVecB)

# check equivalence to r
check <- all.equal(data.matrix(eigVecB), r, 0.00001)
check

library(igraph)

pages <- data.frame(name=c("1", "2", "3", "4", "5", "6"))

relations <- data.frame(from =c("1", "1", "2", "3","3","3","3","4","4","5","5","6","6"), to=c("2","3", "3", "1","2","5","6","6","5","6","4","3","4") )

g <- graph.data.frame(relations, directed=TRUE, vertices=pages)

plot(g)

pr = page.rank(g)

r2 = pr$vector


