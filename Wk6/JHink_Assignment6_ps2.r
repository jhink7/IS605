setwd("C:/CUNY/IS605/Wk6")

library(data.table)
library(dplyr)

# Read in the data
fileName <- 'assign6.sample.txt'
singleString = readChar(fileName, file.info(fileName)$size)

#clean string of special characters, non ASCII characters etc
Encoding(singleString) <- "latin1" 
singleString = iconv(singleString, "latin1", "ASCII", sub="")
singleString = gsub("[^[:alnum:][:space:]]", "", singleString)
singleString = tolower(singleString)
# note, concscious decision to leave in wholely numerical words

# split based on whitespace
y <- strsplit(singleString, "[[:space:]]+")

df <- data.frame(y)
colnames(df) <- c("word")

# get the number of words in a document
numwords = nrow(df)

# sum up instances of each word
wordcounts <- as.data.frame(table(df))

# get probability of each word
wordcounts$prob <- wordcounts$Freq / numwords

# extend program to calculate adjacent words

dt <- data.table(df)

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

alpha <- function(bigram){

  l = strsplit(bigram, "[[:space:]]+")
  l = unlist(l)
  l = sort(l)
  
  sortbigram = paste(l[1], l[2])

  return <- sortbigram
}

# since direction doesn't matter, we have 4 bigrams per row.
dt = dt[, bigram1 := paste(paste(word, ""),rowShift(word,-1))]
dt = dt[, bigram2 := paste(paste(word, ""),rowShift(word,1))]

#construct a 1 d list of bigrams
bigrams = c(dt$bigram1, dt$bigram2)

# filter out 4 bigrams with our NAs
bigramsDF = data.frame(bigrams)
bigramsDF <-filter(bigramsDF, !grepl("NA",bigrams))
bigramsDF <- arrange(bigramsDF, desc(bigrams))

bigramsDF$bigrams = as.character(bigramsDF$bigrams)

# get bigrams in alphabetical order.  This gets rid of the 
# ordering problem
# "the cat" and "cat the" will now be treated the same
bigramsDF$alpha <- unlist(lapply(bigramsDF$bigrams, alpha))

bigramsDF$bigrams <- NULL

# number of bigrams
numbigrams <- nrow(bigramsDF)

# sum up instances of each bigram
bigramcounts <- as.data.frame(table(bigramsDF))

# get probability of each bigram
bigramcounts$prob <- bigramcounts$Freq / numbigrams

bigramcounts <- arrange(bigramcounts, desc(prob))

bigramcounts$bigramsDF = as.character(bigramcounts$bigramsDF)

