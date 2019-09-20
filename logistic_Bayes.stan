data{
  int<lower = 0> N; 
  real<lower = 0> y[N];
}

parameters{
  real<lower = 0> r; 
}



model{
  
  vector[N] mu;
  mu[1] = y[1]; 
  
  for(i in 2:N){
    mu[i] = mu[i-1] + r*mu[i-1]*(1 - mu[i-1]/50);
  }
  y ~ normal(mu,1);
}