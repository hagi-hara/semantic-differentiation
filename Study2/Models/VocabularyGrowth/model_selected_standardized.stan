data {
  int Nrow;
  int diff_words[Nrow];
  vector[Nrow] X1;
  vector[Nrow] X2;
  vector[Nrow] X3;
  vector[Nrow] X4;
  vector[Nrow] X5;
}

parameters{
  // main effect
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  vector[Nrow] r;
  real<lower=0> sigma;
}

model{
  diff_words ~ poisson_log(beta0 + beta1 * X1 + beta2 * X2 + beta3 * X3 + beta4 * X4 + beta5 * X5 + r);
  r ~ normal(0, sigma);
  sigma ~ student_t(4,0,3.5);
  beta0 ~ student_t(4,0,3.5);
  beta1 ~ student_t(4,0,3.5);
  beta2 ~ student_t(4,0,3.5);
  beta3 ~ student_t(4,0,3.5);
  beta4 ~ student_t(4,0,3.5);
  beta5 ~ student_t(4,0,3.5);
}

generated quantities{
  vector[Nrow] log_lik; //　log-likelihood to calculate WAIC

  for(k in 1:Nrow){
    log_lik[k] = poisson_log_lpmf(diff_words[k] | beta0 + beta1 * X1[k] + beta2 * X2[k] + beta3 * X3[k] + beta4 * X4[k] + beta5 * X5[k] + r[k]);
  }
}
