setwd("C:/CUNY/IS605/Wk5")

#### Problem Set 2 ####

# read data in
rawdata = read.table("auto-mpg.data")

#rename columns if we need them later
colnames(rawdata) <- c("disp", "hp", "weight", "acc", "mpg")

cormatrix <- cor(rawdata)
covmatrix <- cov(rawdata)