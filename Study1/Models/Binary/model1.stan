data {
  int Nrow;
  int N_total [Nrow];
  int N_correct [Nrow];
  vector[Nrow] X1;
  vector[Nrow] X2;
  vector[Nrow] X3;
  int N_subject;
  int<lower=0, upper=N_subject> ID_subject[Nrow];
}

parameters{
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  vector[N_subject] r;
  real<lower=0> sigma;
}

model{
  N_correct ~ binomial_logit(N_total, beta0 + beta1 * X1 + beta2 * X2 + beta3 * X3 + r[ID_subject]);
  r ~ normal(0, sigma);
  sigma ~ student_t(4, 0, 0.5); //weak information prior
}

generated quantities{
  vector[Nrow] log_lik;

  for(j in 1:Nrow){
    log_lik[j] = binomial_logit_lpmf(N_correct[j] | N_total[j], beta0 + beta1 * X1[j] + beta2 * X2[j] + beta3 * X3[j] + r[ID_subject[j]]);
  }
}

