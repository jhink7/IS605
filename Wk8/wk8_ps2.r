library(gRain)

# setup network
ny <- c("no", "yes")
lh <- c("low", "high")

diff <- cptable(~difficulty, values=c(30,70),levels=ny)
intel <- cptable(~intelligence, values=c(80,20),levels=lh)
sat <- cptable(~sat|intelligence, values=c(90,10,20,80),levels=lh)
grade <- cptable(~grade|difficulty:intelligence,values=c(60,40,1,99,80,20,10,90),levels=lh)
letter <- cptable(~letter|grade, values=c(90,10,5,95), levels=lh)

plist <- compileCPT(list(diff, intel, sat, grade, letter))
plist

plist$grade

net1 <- grain(plist)

# what happens to the probability ofDifficulty of Course when you present the evidence that the received recommendation letter
# was good?
# Answer: Difficulty (No,Yes) moves from (0.3, 0.7) to (0.16, 0.84)
net12 <- setEvidence(net1, nslist=list(letter="high"))

querygrain(net12, nodes=c("difficulty"), type="marginal")

#  In addition, now present the evidence that both SAT scores were good and
#  the letter of recommendation was good, What is the probability of the Difficulty of Course
#  now?
# Answer: Difficulty (No,Yes) moves from (0.16, 0.84) to (0.14, 0.86)
net13 <- setEvidence(net12, nslist=list(letter="high", sat="high"))
querygrain(net13, nodes=c("difficulty"), type="marginal")
