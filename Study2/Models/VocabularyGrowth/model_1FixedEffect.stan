data {
  int Nrow;
  int diff_words[Nrow];
  vector<lower=0, upper=1>[Nrow] X1;
//  vector<lower=0, upper=1>[Nrow] X2;
//  vector<lower=0, upper=1>[Nrow] X3;
//  vector<lower=0, upper=1>[Nrow] X4;
//  vector<lower=0, upper=1>[Nrow] X5;
}

parameters{
  // main effect
  real beta0;
  real beta1;
//  real beta2;
//  real beta3;
//  real beta4;
//  real beta5;
  vector[Nrow] r;
  real<lower=0> sigma;
}

model{
  diff_words ~ poisson_log(beta0 + beta1 * X1 + r);
  r ~ normal(0, sigma);
  sigma ~ student_t(4,0,3.5);
  beta0 ~ student_t(4,0,3.5);
  beta1 ~ student_t(4,0,3.5);
//  beta2 ~ student_t(4,0,3.5);
//  beta3 ~ student_t(4,0,3.5);
//  beta4 ~ student_t(4,0,3.5);
//  beta5 ~ student_t(4,0,3.5);
}

generated quantities{
  vector[Nrow] log_lik; //　log-likelihood to calculate WAIC

  for(k in 1:Nrow){
    log_lik[k] = poisson_log_lpmf(diff_words[k] | beta0 + beta1 * X1[k] + r[k]);
  }
}
