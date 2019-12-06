library(ggplot2)
library(rstan)

setwd("C:/Users/micha/Documents/Stats_451/finalProject")
data <- read.csv("summary19_pt.csv")
attach(data)

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
    vector[n] pred;
    pred = x*beta;
}model{
    beta[1] ~ normal(0, 1e6);
    beta[2] ~ normal(0, 1e6);
    beta[3] ~ normal(0, 1e6);
    beta[4] ~ normal(0, 1e6);
    beta[5] ~ normal(0, 1e6);
    for(i in 1:n){
        y[i] ~ normal(pred[i], sigma);
    }
}"

data_stan <- data[, c(5, 9, 13, 15, 18, 19, 20, 21, 22)]
head(data_stan)

datalist <- list(n = nrow(data), f = 5, x = data_stan[,c(1:4, 9)], y = data_stan[,8])

stan_samples <- stan(model_code = stan_code, data = datalist, chains = 1, iter = 2000)

test <- extract(stan_samples, permuted = "TRUE")

hist(test$pred)

test$pred[1]
data$win_perc[1]

resid <- data$win_perc - test$pred

