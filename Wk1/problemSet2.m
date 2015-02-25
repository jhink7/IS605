A = [1,1,3;2,-1,5;-1,-2,4]
b = [1;2;6]

X = LinearSystemSolve(A,b)

X2 = linsolve(A,b)

A2 = [0,2,1;1,1,1;1,2,0]
b2 = [4;7;6]

X3 = LinearSystemSolve(A2,b2)
X4 = linsolve(A2,b2)

X5 = A2\b2