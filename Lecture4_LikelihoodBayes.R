
#### Likelihood for single data points 
#Poisson 
y <- 8
curve(dpois(8,x),0,20,ylab = "density", xlab = "theta")

intGrid <- seq(0,20,1)
intHt <- dpois(8,intGrid)

plot(intGrid,intHt)
sum(intHt)

# Bernoulli 
curve(dbinom(1,1,x),0,1)
# What is area under this curve? 

# Compare to: 
curve(dbinom(x,1,0.5),0,1)


### Gamma examnple: 
y_obs <- 75
mu <- 100
var <- 2500 

alpha <- mu^2/var
beta <- mu/var 

curve(dgamma(y_obs,shape=x^2/var,rate=x/var),0,200)

intGrid <- seq(0,200,0.1)
intHt <- dgamma(y_obs,shape=intGrid^2/var,rate=intGrid/var)
#plot(intGrid,intHt)
sum(0.1*intHt) # 0.82 

gamma_LP75 <- ggplot(data = data.frame(x=intGrid,y=intHt),aes(x=x,y=y)) + 
  geom_line() + geom_vline(aes(xintercept = y_obs),linetype="dashed",
                           color = "red") + theme_bw() + 
  xlab("mean parameter") + ggtitle("Gamma Likelihood Profile (y_obs = 75)") +
  ylab("Density")

pdf("Gamma_LP.pdf", width = 8, height = 6)
gamma_LP75
dev.off()

# Likelihood of multiple data 
set.seed(c(1,2,3,4))
N <- c(3,30,300)

y_rep1 <- rnorm(N[1],mean = 0.5, sd = 1)
y_rep2 <- rnorm(N[2],mean = 0.5, sd = 1)
y_rep3 <- rnorm(N[3],mean = 0.5, sd = 1)

print(y_rep1)
plot(y_rep1)

#intGrid <- seq(-5,5,0.1)
#intHt <- rep(0,length(intGrid))
#for(i in 1:length(intHt)){
#  intHt[i] <- prod(dnorm(y_rep1,intGrid[i],1))
#}
#plot(intGrid,intHt)
#lines(intGrid,intHt)
#sum(intHt)


LL_Norm <- function(x,data,var=1){
  LL <- rep(0,length(data))
  for(i in 1:length(data)){
    LL[i] <- -log(dnorm(data[i],x,var))
  }
  return(sum(LL))
}


grid_x <- seq(-10,10,0.1)
LP <- rep(0,length(grid_x))
LP2 <- rep(0,length(grid_x))
LP3 <- rep(0,length(grid_x))
for(i in 1:length(grid_x)){
  LP[i] <- LL_Norm(grid_x[i],y_rep1,var=1)
  LP2[i] <- LL_Norm(grid_x[i],y_rep2,var=1)
  LP3[i] <- LL_Norm(grid_x[i],y_rep3,var=1)
}

LP_data <- data.frame(x = grid_x, y1 = LP, y2 = LP2, y3 = LP3)

LP_plot1 <- ggplot(LP_data) + geom_line(aes(x=x,y=y1)) +
  ylab("- Log Likelihood")
# + geom_line(aes(x=x,y=y2),col="red") +
# geom_line(aes(x=x,y=y3),col="blue") 
LP_plot2 <-  ggplot(LP_data) + geom_line(aes(x=x,y=y1))+
  ylab("- Log Likelihood") + geom_line(aes(x=x,y=y2),col="red") 
# geom_line(aes(x=x,y=y3),col="blue") 
LP_plot3 <-  ggplot(LP_data) + geom_line(aes(x=x,y=y1))+
  ylab("- Log Likelihood") + geom_line(aes(x=x,y=y3),col="red") 

library(cowplot)
plot_grid(LP_plot1,LP_plot2,LP_plot3, ncol=3)

pdf("Likelihood_Profile.pdf",width=12,height=5)
plot_grid(LP_plot1,LP_plot2,LP_plot3, ncol=3)
dev.off()

-log2(0.01)
curve(-log2(1/x),1,100000)
choose(3,1)



y <- c(-0.13, 0.68, -0.34) 

y <- rnorm(3,0.1,1)

LL_norm1 <- function(y,mu,sigma=1){
  return(sum(-log(dnorm(y,mean=mu,1))))
}

grid_x <- seq(-4,4,0.1)
LL_profile <- rep(0,length(grid_x))
for(i in 1:length(LL_profile)){
  LL_profile[i] <- LL_norm1(y,grid_x[i])
}
plot(grid_x,LL_profile)

grid_x[which(LL_profile == min(LL_profile))]


loc_min <- which(LL_profile == min(LL_profile))
# 2nd derivative 
deriv2 <- ((LL_profile[loc_min] - LL_profile[loc_min+1])  - (LL_profile[loc_min] - LL_profile[loc_min-1]))/(0.1^2)
(1/deriv2)
# variance = 0.555
sqrt(1/deriv2)

curve(dgamma(x,1,2),0,10)
curve(dexp(x,1),0,10)



##### Notes on Pareto convergence 
library(EnvStats)

# LLN: 
N=10^4;
alpha = 2.2
x=rpareto(N,location=1,shape = alpha);
y=rep(NA,N);
y_var = rep(NA,N);
y_se = rep(NA,N); 

true_mean <- alpha/(alpha-1)
true_var <- alpha/(((alpha-1)^2)*(alpha-2))

for(index in seq(1,N))
{
  y[index]=mean(x[1:index]);
  y_var[index] = var(x[1:index]);
  y_se[index] = var(x[1:index])/index; 
}
plot(y,type='l',xlab='n',ylab='sum(x_i)/n')
plot(y_se,type='l',xlab='n',ylab='SE(mean)') # Decent...I guess 
plot(y_var,type='l',xlab='n',ylab='var(x)') # So, still nowhere close even after 10^5 iterations! 

### Conclusions: 
# Convergence on true mean is *from below*, i.e. sample mean systematically underestimates 
# true mean. Convergence on true mean is horrible where variance is infinite. Convergence of 
# sample variance to "true" variance is TERRIBLE, even when finite. Severe underestimation 
# even after 10^5 iterations. This shows how convergence in distribution to Normal(mu,sigma) 
# is going to be terrible. 

# Overall: for sufficiently fat-tailed distributions, convergence to Gaussian under CLT, and operation 
# of LLN is biased and slow. This means that standard statistical methodologies are going to end 
# up poorly. We will systematically underestimate the true mean, and even more so the variance. This will 
# lead to false confidence (precision) in a biased estimate. Not good. 

N=10^7;
alpha = 2.2
x=rpareto(N,location=1,shape = alpha);
y_var = rep(NA,N);
true_var <- alpha/(((alpha-1)^2)*(alpha-2))

for(index in seq(1,N))
{
  y_var[index] = var(x[1:index]);
}

str(y_var)
plot(seq(1,N),y_var)

y_var2 <- na.omit(y_var)

save(y_var2, file = "pareto_varSAMPS.rdata")
load(file )
LP <- length(y_var2)

plot(y_var2[10^5:LP],type='l',xlab='n',ylab='var(x)')
abline(a = true_var, b = 0, col = 2)

length(y_var2[10^5:LP])

ggplot(data = data.frame(var = y_var2[10^5:LP],x=seq(1,length(y_var2[10^5:LP]))),aes(y=var,x=x)) + geom_line() + 
  geom_hline(aes(yintercept = true_var),color = "red") + ylab("Sample Variance") + ggtitle("Sample variance does not converge to true variance",
                                                                                           subtitle = "Pareto with alpha = 2.2")

