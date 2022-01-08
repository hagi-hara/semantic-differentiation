data {
  int Nrow;
  int N_total [Nrow];
  int N_correct [Nrow];
//  vector[Nrow] X1;
//  vector[Nrow] X2;
//  vector[Nrow] X3;
  int N_subject;
//  int N_sub_time;
//  int N_sub_cond;
  int<lower=0, upper=N_subject> ID_subject[Nrow];
//  int<lower=0, upper=N_sub_time> ID_sub_time[Nrow];
//  int<lower=0, upper=N_sub_cond> ID_sub_cond[Nrow];
  int N_pred;
}

parameters{
  real beta0;
//  real beta1;
//  real beta2;
//  real beta3;
  vector[N_subject] r1;
//  vector[N_sub_time] r2;
//  vector[N_sub_cond] r3;
  real<lower=0> sigma1;
//  real<lower=0> sigma2;
//  real<lower=0> sigma3;
}

model{
  N_correct ~ binomial_logit(N_total, beta0 + r1[ID_subject]);
  r1 ~ normal(0, sigma1);
//  r2 ~ normal(0, sigma2);
//  r3 ~ normal(0, sigma3);
  sigma1 ~ student_t(4, 0, 0.5); //weak information prior
//  sigma2 ~ student_t(4, 0, 0.5); //weak information prior
//  sigma3 ~ student_t(4, 0, 0.5); //weak information prior
}

generated quantities{
  vector[N_pred] q_pred;

  for (i in 1:N_pred){
    q_pred[i] = inv_logit(beta0);
  }
}


