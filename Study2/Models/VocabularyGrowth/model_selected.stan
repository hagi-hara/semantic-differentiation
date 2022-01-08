data {
  int Nrow;
  int diff_words[Nrow];
  vector<lower=0, upper=1>[Nrow] X1;
  vector<lower=0, upper=1>[Nrow] X2;
  vector<lower=0, upper=1>[Nrow] X3;
  vector<lower=0, upper=1>[Nrow] X4;
  vector<lower=0, upper=1>[Nrow] X5;
  int N_pred;
  vector<lower=0, upper=1>[N_pred] X2_pred;
  vector<lower=0, upper=1>[N_pred] X3_pred;
  vector<lower=0, upper=1>[N_pred] X4_pred;
  vector<lower=0, upper=1>[N_pred] X5_pred;
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
  vector[N_pred] lambda_pred;

  for(j in 1:N_pred){
    lambda_pred[j] = exp(beta0 + beta1 * 0.6 + beta2 * X2_pred[j] + beta3 * X3_pred[j] + beta4 * X4_pred[j] + beta5 * X5_pred[j]);
  }
}
