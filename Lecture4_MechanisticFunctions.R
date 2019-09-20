### Mechanistic Models 

# Hill function 
hill <- function(x,Vmax=vmax,Q=q,K=k){
  hill <- Vmax*(x^Q)/(K^Q + x^Q);
  return(hill)
}

vmax <- 1
q <- 3
k <- 2.5 
hill(x=3)

curve(hill,0,10)

# What happens in the limit as Q --> Inf ? 
q <- 100
curve(hill,0,10)

# We see that the hill becomes a step function essentially. So the q parameter 
# basically controls how S-shaped it is. 
a <- 2.5
h <- 2
Holling2 <- function(x,A=a,H=h){
  return((A*x)/(1 + A*H*x))
}

# Limit R --> Inf, then f(R) --> 1/h 

curve(Holling2,0,10)



# Solution to moment matching gamma in terms of mean and skewness
mu <- 10
skew <- 500
curve(dgamma(x,shape = sqrt((mu^3)/skew),rate = sqrt(mu/skew)),0,20)


# Stan check on gamma inferences: 
N <- 30
y <- rgamma(N,shape = sqrt((mu^3)/skew),rate = sqrt(mu/skew))

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

lilN <- list(N=N,y=y)
m1 <- stan(file = "gamma_MuSkew.stan",data=lilN,chains = 6, iter = 2000)
print(m1)
traceplot(m1,pars = c("mu","skew"))
# Wow, adding even just OoM priors does WONDERS for the accuracy of our estimation 
MCMC_samps <- extract(m1,pars=c("mu","skew"))

MCdf <- data.frame(mu = MCMC_samps[[1]], skew = MCMC_samps[[2]])

# Plotting posterior density inferences 
ggplot(MCdf) + stat_density(aes(x=mu)) + geom_vline(aes(xintercept=10),color = "red") 
ggplot(MCdf) + stat_density(aes(x=skew)) + geom_vline(aes(xintercept=500),color = "red") 




# Sigmoidal simulation 
error <- 1
Tt <- 100 
N <- rep(NA,Tt);
N[1] <- 1; 
K <- 500; 
r <- 0.1; 
for(i in 2:Tt){
  N[i] = N[i-1] + r*N[i-1]*(1-(N[i-1]/K))
}
M = N + rnorm(Tt,0,error)

plot(seq(1,Tt,1),M)

Nsub <- N[55:75]

diff(Nsub)
# r_hat
(4/K)*mean(diff(Nsub)) # 0.092

# r_hat SE 
4/(K*sqrt(length(diff(Nsub)))) # 0.018 

log_data <- list(N = Tt, y = N);
log_model <- stan(file = "logistic_Bayes.stan", data = log_data, chains = 4, iter = 500)
print(log_model,digits = 4)



curve(dgamma(x,shape = 1, rate = 0.1),0,10)
curve(dexp(x,rate=0.1),0,10,col=2,add=T)

curve((2+1)/(0.5+x),0,50)
curve(6*exp(-4*x),0,10,col=2,add=T)




