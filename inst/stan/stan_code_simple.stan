
  data {
    int<lower=0> N;           // Number of observations
    int<lower=0> M;           // Number of simulation
    vector[N] y;              // Response variable
    vector[N] x;              // Continuous explanatory variable
    vector[M] x_sim;          // Continuous simulaiton variable
  }
  parameters {
    real beta0;               // Intercept
    real beta1;               // Coefficient for the continuous variable
    real<lower=0> sigma;      // Standard deviation of the error term
  }

  model {
    // Priors
    beta0 ~ normal(0, 10);
    beta1 ~ normal(0, 10);
    sigma ~ cauchy(0, 5);

    // Likelihood
    for (i in 1:N) {
      target += normal_lpdf(y[i] | beta0 + beta1 * x[i], sigma);
    }
  }
  generated quantities {
    array[N] real y_ppc ;
    array[M] real y_sim ;
    for (i in 1:N) {
      y_ppc[i] = normal_rng(beta0 + beta1 * x[i], sigma);
    }
    for (i in 1:M) {
      y_sim[i] = normal_rng(beta0 + beta1 * x_sim[i], sigma);
    }
  }
