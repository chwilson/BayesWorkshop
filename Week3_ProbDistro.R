### Binomial properties 


# Convergence to Normal 
# Fix N large 
N <- 100
curve(choose(N,x)/(2^N),0,N)

# Check against Normal density: 
curve(dnorm(x,N*0.5,sqrt(N*0.25)),0,N,add=T,col=2)

# Position probability of a single particle 
N
process <- matrix(0,N,10^3)
process[1,10^3] <- 0 
for(i in 2:N){
  for(j in 1:10^3){
  if(rbinom(1,1,0.5)==1){
  process[i,j] <- process[i-1,j] + 1
  } else{
    process[i,j] <- process[i-1,j] - 1
  }
  }
}
plot(process[,1])
plot(apply(process,1,var))

curve(choose(N,x)/(2^N) - choose(N,x)/(2^N),0,50)



####Poisson simulation 


rate <- 5
offset <- 10^4
exposure <- c(10^3,10^5)
lambda <- rate/offset 
small_county <- rpois(100,lambda*exposure[1])
large_county <- rpois(100,lambda*exposure[2])

print(c(small_county,large_county))

#Naive inference: 
rate_estSMALL = (small_county/exposure[1])
rate_estLARGE = (large_county/exposure[2])

print(rate_estSMALL*offset)
print(rate_estLARGE*offset)

mean(rate_estSMALL*offset)
mean(rate_estLARGE*offset)


### Negative Binomial 
# First with gamma of mean 4, variance 8
lambda <- rgamma(10^5,2,0.5)
neg_bin <- rep(0,length(lambda))
for(i in 1:length(neg_bin)){
neg_bin[i] <- rpois(1,lambda[i])
}
hist(neg_bin)

mean(neg_bin)
var(neg_bin)

# Now with gamma of mean 4, variance 4
lambda <- rgamma(10^5,4,1)
neg_bin <- rep(0,length(lambda))
for(i in 1:length(neg_bin)){
  neg_bin[i] <- rpois(1,lambda[i])
}
hist(neg_bin)

mean(neg_bin)
var(neg_bin)

### Lesson, the variance of the sampling distribution is the sum of the 
### variance of the two distributions in the factorized form. This is 
### intuitive if you think of the normal distribution, etc. NOTE that a 
### corrollary is that adding hierarchical structure NEVER reduces the uncertainty
### in your sampling distribution. This is about correctly accounting for 
## and propagating all of the uncertainty!

demo_xy <- read.csv("demo_xy.csv")
library(ggplot2)
ggplot(demo_xy,aes(x=x,y=y)) + geom_point()
plot(x,y,demo_xy)

df <- data.frame(x=demo_xy$x, y = demo_xy$y)

bin_x <- seq(range(df$x)[1],range(df$x)[2],length.out=30)
x_marg <- rep(0,length(bin_x))
df2 <- df[order(df$x),]
for(i in 1:length(x_marg)-1){
  x_marg[i] <- nrow(df2[which(df2$x>bin_x[i] & df2$x < bin_x[i+1]),])
}

plot(bin_x,x_marg)

# Law of total probability 
sum(x_marg/10^4)




