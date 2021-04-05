functions {
  matrix covmat(real[] x, real r, real l){
    // Covariance matrix based on the squared exponential kernel.
    matrix[num_elements(x), num_elements(x)] m;

    m = cov_exp_quad(x, r, l);
    for (i in 1:num_elements(x)){
      m[i, i] += 1e-9;
    }
    return m;
  }
}

data {
  int<lower=2> n_players;
  int<lower=1> n_games;
  int<lower=1> n_dates;

  int date_idx[n_games];
  int<lower=1, upper=n_players> white[n_games];
  int<lower=1, upper=n_players> black[n_games];
  int<lower=1, upper=3> result[n_games];  // 1=white win, 2=draw, 3=black win
  int<lower=0, upper=1> speedchess[n_games];
  real dates[n_dates];
}

parameters {
  matrix[n_players, n_dates] eta; // N(0, 1) samples to convert to multivariate normal
  real<lower=0> l;                // GP kernel length-scale
  real<lower=0> r[n_players];     // GP kernel variances, one per player
  real<lower=0> mu;               // Mean of player rating variances
  real<lower=0> sigma;            // Variance of player rating variances
  simplex[n_players] tau;         // Player means
  real<lower=0> nu[2];            // Draw parameters for classical and speed chess
}

transformed parameters {
  matrix[n_players, n_dates] rating;
  vector[3] alpha[n_games];
  matrix[n_dates, n_dates] LT = cholesky_decompose(covmat(dates, 1.0, l))';

  for (i in 1:n_players){
    real rho = mu + r[i]*sigma;
    rating[i, :] = inv_logit(logit(tau[i]) + rho * eta[i, :] * LT);
  }

  for (j in 1:n_dates){
    rating[:, j] /= sum(rating[:, j]);
  }

  for (g in 1:n_games){
    real w = rating[white[g], date_idx[g]];
    real b = rating[black[g], date_idx[g]];

    alpha[g][1] = w/(w + b + nu[speedchess[g]+1]*sqrt(w*b));
    alpha[g][3] = b/(w + b + nu[speedchess[g]+1]*sqrt(w*b));
    alpha[g][2] = 1 - alpha[g][1] - alpha[g][3];
  }
}

model {
  l ~ inv_gamma(5, 500);
  mu ~ normal(0, 1);
  sigma ~ normal(0, 1);
  r ~ normal(0, 1);

  nu ~ exponential(0.1);

  for (i in 1:n_players)
    eta[i, :] ~ normal(0, 1);

  tau ~ dirichlet(rep_vector(1.0, n_players));

  for (g in 1:n_games)
    result[g] ~ categorical(alpha[g]);
}
