data {
  int Nrow;
  vector<lower=0, upper=1>[Nrow] PropLook;
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
  real<lower=0> f_kappa;
  vector[N_subject] r;
  real<lower=0> sigma;
}

transformed parameters{
  vector[Nrow] mu;
  mu = inv_logit(beta0 + beta1 * X1 + beta2 * X2 + r[ID_subject]);
}

model{
  PropLook ~ beta_proportion(mu, f_kappa);
  r ~ normal(0, sigma);
  sigma ~ student_t(4, 0, 0.5); //weak information prior
}

generated quantities{
  vector[N_pred] q_pred;

  for (i in 1:N_pred){
    q_pred[i] = inv_logit(beta0 + beta1 * X1_pred[i] + beta2 * X2_pred[i]);
  }
}
