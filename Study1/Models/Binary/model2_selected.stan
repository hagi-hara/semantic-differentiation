data {
  int Nrow;
  int N_total [Nrow];
  int N_correct [Nrow];
  vector[Nrow] X1;
  vector[Nrow] X2;
  // vector[Nrow] X3;
  int N_subject;
  int<lower=0, upper=N_subject> ID_subject[Nrow];
  int N_pred;
  vector[N_pred] X1_pred;
  vector[N_pred] X2_pred;
}

parameters{
  real beta0;
  real beta1;
  real beta2;
  // real beta3;
  vector[N_subject] r;
  real<lower=0> sigma;
}

model{
  N_correct ~ binomial_logit(N_total, beta0 + beta1 * X1 + beta2 * X2 + r[ID_subject]);
  r ~ normal(0, sigma);
  sigma ~ student_t(4, 0, 0.5); //weak information prior
}

generated quantities{
  vector[N_pred] q_pred;

  for (i in 1:N_pred){
    q_pred[i] = inv_logit(beta0 + beta1 * X1_pred[i] + beta2 * X2_pred[i]);
  }
}

