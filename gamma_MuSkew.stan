data{
  int<lower = 0> N; 
  real<lower = 0> y[N]; 
}

parameters{
  real<lower = 0> mu; 
  real<lower = 0> skew; 
}

transformed parameters{
  real<lower = 0> alpha;
  real<lower = 0> beta; 
  alpha = sqrt(mu^3/skew);
  beta = sqrt(mu/skew);
}

model{
  skew ~ exponential(0.01); 
  mu ~ exponential(0.1); 
  y ~ gamma(alpha,beta); 
}