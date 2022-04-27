
data {
  int<lower=0> N_obs;
  int<lower = 0> N_samp;
  int<lower = 0> N_pred;
  vector[N_obs] eir;
  matrix[N_obs, N_pred] prev;
  matrix [N_samp, N_pred] y;
  row_vector[N_pred] tar;
}

transformed data{
 matrix[N_obs, N_pred] x = logit(prev);
 vector[N_obs] y_raw = log(eir);
}

parameters {
  real <lower=0> sigma;
  real alpha;
  vector[N_pred] beta;
}

model {
  y_raw ~ normal(x * beta + alpha, sigma);
}

generated quantities {
 // vector[N_obs] log_lik;
 real pred = exp(normal_rng(logit(tar) * beta + alpha, sigma));
 if(N_samp > 0){
   vector[N_samp] prev_out = exp(logit(y) * beta + alpha);
 }
 // for(j in 1:N_obs){
 //  log_lik[j] = normal_lpdf(y_raw[j] | x[j] * beta + alpha, sigma);
 // }

}
