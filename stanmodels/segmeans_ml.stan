data{
    int<lower=1> N;
    int<lower=1> N_hr;
    real b_var[N];
    real sd_raw[N_hr];
    real mu_raw[N_hr];
    int hr[N];
    real<lower=0> mu_sig;
    real<lower=0> sig_sig;
}
parameters{
    vector[N_hr] mu;
    vector<lower=0>[N_hr] sigma;
}
model{
    sigma ~ normal( sd_raw , sig_sig );
    mu ~ normal( mu_raw , mu_sig );
    b_var ~ normal( mu[hr] , sigma[hr] );
}