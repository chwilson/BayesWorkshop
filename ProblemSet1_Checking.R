#Code for probabilities
D1 <- 1:6
D2 <- 1:12
Poss <- vector() #computational vector
Poss2 <- vector() #vector containting the list of all values obtained

for (i in D1) {
  for (j in D1) {
    p <- i+j
    Poss[j] <- p
  }
  Poss2 <- append(Poss2,Poss)  
}
for (i in D2) {
  for (j in D2) {
    p <- i+j
    Poss[j] <- p  
  }
  Poss2 <- append(Poss2,Poss)
}
for (i in D1) {
  for (j in D2) {
    p <- i+j
    Poss[j] <- p  
  }
  Poss2 <- append(Poss2,Poss)
}
for (i in D1) {
  for (j in D2) {
    p <- i+j
    Poss[j] <- p  
  }
  Poss2 <- append(Poss2,Poss)
}

x <- Poss2
hist(x,
     breaks= 24, 
     freq=FALSE, 
     main= "", 
     xlab="Sum of Dice",ylab="Probability",
     xlim=c(1,24), ylim=c(0,0.08))


## Simon 
dices = c(1:6,1:12)
hist(dices,probability=T,breaks=13)
?hist
length(2:11)


expand.grid(D1,D2)
expand.grid(D1,D1)
expand.grid(D2,D1)
expand.grid(D2,D2)

length(which(rowSums(expand.grid(D1,D1))==6))/36


vals <- rowSums(rbind(expand.grid(D1,D1),expand.grid(D2,D2),expand.grid(D1,D2),expand.grid(D2,D1)))
length(which(vals==2))/length(vals)
# AHA! This approach ends up under-counting because the sample space in (D1,D2) is 
# half the size of (D2,D2), so simply adding these things together does not correctly 
# weight the probability of the 'event' that 1 D6 + 1 D12 are drawn. These are the correct
# weighs to get probability of different sums, conditional on knowing the dice! 

## Monte Carlo approach 1
D1 <- c(1:12)
D2 <- c(1:6)
possible <- c(D1,D2)
experiment <- rep(0,10^7)
for(i in 1:length(experiment)){
experiment[i] <- sum(sample(possible,2,replace=T))
}


# Checking 
length(which(experiment==2))/length(experiment)
length(which(x==2))/length(x)
0.25*(1/36) + 0.25*(1/144) + 0.25*(1/72)
0.25*(1/36) + 0.25*(1/144) + 0.5*(1/72) # Super weird that the first one, which does not 
# condition to correct probability is the one that agrees with numerical experiment 

#### Monte Carlo Approach 2 

possible <- c("6","12")
experiment <- rep(0,10^5)

for(i in 1:length(experiment)){
  D1 <- as.numeric(sample(possible,1))
  D2 <- as.numeric(sample(possible,1))
  val1 <- sample(1:D1,1)
  val2 <- sample(1:D2,1)
  experiment[i] <- val1+val2
}

# Sum = 2
length(which(experiment==2))/10^5
0.25*(1/36) + 0.25*(1/144) + 0.5*(1/72)




# Sum = 24
length(which(experiment==24))/10^5
0.25*(1/144)


?geom_histogram
ggplot(data=data.frame(x=experiment),aes(x)) + geom_histogram(bins=23)
hist(experiment,probability=T,breaks=23)
50*5*20*0.55
13033/12
(21033.8+1086.08)*1.12





