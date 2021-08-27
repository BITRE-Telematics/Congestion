data{
    int<lower=1> N;
    real<lower=0> b_var[N];
    real<lower=0> sd_raw;
    real<lower=0> mu_raw;
    real<lower=0> mu_sig;
    real<lower=0> sig_sig;
}
parameters{
    real<lower=0> mu;
    real<lower=0> sigma;
}
model{
    sigma ~ normal( sd_raw , sig_sig );
    mu ~ normal( mu_raw , mu_sig );
    b_var ~ normal( mu , sigma );
}