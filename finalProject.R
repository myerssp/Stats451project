library(ggplot2)
library(rstan)
library(bayesplot)
library(rstanarm)

setwd("C:/Users/micha/Documents/Stats_451/finalProject")
data <- read.csv("summary19_pt.csv")
attach(data)

hist(win_perc)

hist(games)

hist(AdjOE)
hist(AdjDE)
hist(AdjEM)
hist(AdjTempo)

stan_code <- "data{
    int<lower=0> n;
    int<lower=0> f;
    matrix[n, f] x;
    real y[n];
}parameters{
    vector[f] beta;
    real<lower=0> sigma;
}transformed parameters{
    vector<lower = 0>[n] pred;
    pred = x*beta;
}model{
    beta[1] ~ normal(0, 1e6);
    beta[2] ~ normal(0, 1e6);
    beta[3] ~ normal(0, 1e6);
    beta[4] ~ normal(0, 1e6);
    beta[5] ~ normal(0, 1e6);
    for(i in 1:n){
        y[i] ~ beta(pred[i]*(pred[i]*((1-pred[i])/sigma-1)),(1-pred[i])*(pred[i]*((1-pred[i])/sigma-1)) );
    }
}"

data_stan <- data[, c(5, 9, 13, 15, 18, 19, 20, 21, 22)]
head(data_stan)

datalist <- list(n = nrow(data), f = 5, x = data_stan[,c(1:4, 9)], y = data_stan[,8])

stan_fit <- stan(model_code = stan_code, data = datalist, chains = 1, iter = 1000)

stan_samples <- extract(stan_fit, permuted = "TRUE")

pred_wins = matrix(nrow = nrow(stan_samples$pred), ncol = ncol(stan_samples$pred))

for(i in 1:nrow(stan_samples$pred)){
  for(j in 1:ncol(stan_samples$pred)){
    pred_wins[i,j] = rbeta(1, stan_samples$pred[i,j]*stan_samples$sigma[i],
                           (1-stan_samples$pred[i,j])*stan_samples$sigma[i])
  }
}

data_pred = data.frame(wins = colMeans(pred_wins))

hist(data_pred$wins)

stan_samples$pred

stan_samples$pred[1]
stan_samples$win_perc[1]

resid <- data$win_perc - colMeans(pred_wins)

posterior = as.matrix(stan_fit)
  
hist(resid)
mcmc_areas(posterior,
           pars = c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]"),
           prob = 0.95)
ppc_dens_overlay(y = stan_samples$pred[,1],
                 yrep = posterior_predict(stan_samples, draws = 50))
