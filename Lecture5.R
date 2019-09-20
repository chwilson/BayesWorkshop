### Lecture 5 demos

# Beta-binomial inference 

N <- 10 
y <- 3
alpha <- 5
beta <- 5 

# Adding the components 
curve(dbeta(x,alpha,beta),0,1,ylim = c(0,4)) # Prior 
#abline(v = 0.5,add=T)
curve(dbinom(y,N,x),0,1,col=2,add=T) # Likelihood 
#abline(v = 0.3, add = T,col=2 )

# Joint distro - i.e. product of likelihood and prior 
curve(dbinom(y,N,x)*dbeta(x,alpha,beta),0,1,col=3,add=T) # Likelihood

# Evaluating marginal of y 
grid_x <- seq(0,1,0.01)
delta_x <- 0.01 
joint <- rep(NA,length(grid_x))
for(i in 1:length(grid_x)){
  joint[i] = dbinom(y,N,grid_x[i])*dbeta(grid_x[i],alpha,beta)
}
sum(joint*delta_x) # 0.125 
# Just to drive home full posterior 
sum((joint/0.125)*delta_x)

# Manually getting inference - MLE 
which(joint == max(joint)) # 0.4 

# Mean 
sum(grid_x*joint*delta_x) 
# = 0.05, what's wrong with this picture? 


# Need to normalize the joint to get valid probabilistic inference!!!!!
# sum(grid_x*(joint/0.125)*delta_x)





# Posterior, i.e. normalized joint distro 
curve(dbeta(x,alpha+y,beta+N-y),col=4,add=T)
#abline(v=)

# manual moments 
(alpha + y)/(alpha + beta+N) #0.4 

## BIG NOTE: We will rarely be able to solve for the Bayesian posterior in closed form like this. 
## This single fact, more than any other, is what prevented Laplacian/Bayesian inference from dominating statistical 
## modeling and inference starting in the early 1800s. All kinds of mathematical trickery went into ways 
## of solving difficult integrals, or reducing complex models into tractable (usually Gaussian) distributions, 
## until computer technology advanced sufficiently. 

### CHALLENGE: Poisson-Gamma inference 

# you have a plant protection experiment with one treatment (terrible design!). You have placed 100 plants 
# in the field randomly and then you count the incidence of insects/plant in each treatment.

set.seed(12345)
insects_plants <- rpois(10,4.3)
save(insects_plants, file= "insects.rdata")

# Based on previous work in similar systems, you tend to see around 5 insects/plant on average, but this 
# can vary from around 1 to around 9. 

# Working with a conjugate gamma prior, find and graph the display of the prior, likelihood, joint and posterior densities.
# Now, compare your inference when your prior is substantially different, say a value centered at 30[10;50]. 

alpha <- 20
beta <- 8

# First, our prior: 
curve(dgamma(x,alpha,beta),0,20)

# Next, define function for likelihood 
pois_prod <- Vectorize(function(x,data,scale){
  like <- prod(dpois(as.vector(data),x))
  return(like*(10^scale))
},"x")

# PLOT 
ggplot(data = data.frame(x=c(0,10)),aes(x)) + 
  #stat_function(fun = id_func, args = list(scale=2))
  stat_function(fun = pois_prod, args = list(data=insects_plants,scale=9)) +
  stat_function(fun = dgamma, args = list(shape = alpha, rate = beta),col = "blue") +
  stat_function(fun = gamma_joint, args = list(data=insects_plants,scale=12,alpha=alpha,beta=beta),color = "green")


gamma_joint <- Vectorize(function(x,data,scale,alpha,beta){
  like <- prod(dpois(as.vector(data),x))*dgamma(x,alpha,beta)
  return(like*(10^scale))
},"x")


# Posterior inference 
grid_x <- seq(0,10,0.1)
delta_x <- 0.1 
jointSamps <- gamma_joint(grid_x,insects_plants,0,alpha,beta)
# Posterior expectation: sum(jointSamps*grid_x*delta_x)/sum(jointSamps*delta_x)
which(jointSamps==max(jointSamps))


