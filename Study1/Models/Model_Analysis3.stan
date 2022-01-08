data {
  int Nrow;
  int words[Nrow];
  
  // Main effect
  vector<lower=0, upper=1>[Nrow] age19;
  vector<lower=0, upper=1>[Nrow] age20;
  vector<lower=0, upper=1>[Nrow] age21;
  vector<lower=0, upper=1>[Nrow] age22;
  vector<lower=0, upper=1>[Nrow] age23;
}

parameters{
  real beta0;
  real b_age19;
  real b_age20;
  real b_age21;
  real b_age22;
  real b_age23;
  vector[Nrow] r;
  real<lower=0> sigma;
}

model{
  words ~ poisson_log(beta0 + b_age19 * age19 + b_age20 * age20 + b_age21 * age21 + b_age22 * age22 + b_age23 * age23 + r);
  r ~ normal(0, sigma);
}

generated quantities{
  real age18_pred;
  real age19_pred;
  real age20_pred;
  real age21_pred;
  real age22_pred;
  real age23_pred;
  
  real b_age_diff_18_19;
  real b_age_diff_19_20;
  real b_age_diff_20_21;
  real b_age_diff_21_22;
  real b_age_diff_22_23;
  
  age18_pred = exp(beta0);
  age19_pred = exp(beta0 + b_age19);
  age20_pred = exp(beta0 + b_age20);
  age21_pred = exp(beta0 + b_age21);
  age22_pred = exp(beta0 + b_age22);
  age23_pred = exp(beta0 + b_age23);
  
  b_age_diff_18_19 = b_age19;
  b_age_diff_19_20 = b_age20 - b_age19;
  b_age_diff_20_21 = b_age21 - b_age20;
  b_age_diff_21_22 = b_age22 - b_age21;
  b_age_diff_22_23 = b_age23 - b_age22;
}
