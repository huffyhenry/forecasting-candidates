data {
  int<lower=2> N_players;
  int<lower=1> N_games;
  int<lower=1> N_openings;

  int white[N_games];
  int black[N_games];
  int<lower=1, upper=N_openings> opening[N_games];
  int<lower=1, upper=3> result[N_games];  // 1=white win, 2=draw, 3=black win
}

parameters {
  simplex[N_players] rating;
  real<lower=0.0> nu;
}

transformed parameters {
  vector[N_games] den; // Denominator for the calculation of probabilities
  vector[3] alpha[N_games]; // Game result probabilities

  den = rating[white] + rating[black] + nu*sqrt(rating[white] .* rating[black]);
  for (g in 1:N_games){
    alpha[g][1] = rating[white[g]] / den[g];
    alpha[g][3] = rating[black[g]] / den[g];
    alpha[g][2] = 1 - alpha[g][1] - alpha[g][3];
  }
}

model {
  rating ~ dirichlet(rep_vector(1.0, N_players));
  nu ~ exponential(0.1);

  for (g in 1:N_games){
    result[g] ~ categorical(alpha[g]);
  }
}

generated quantities {
  int<lower=1, upper=3> hth[N_players, N_players]; // head-to-head
  real den_;
  vector[3] alpha_;

  for (i in 1:N_players){
    for (j in 1:N_players){
      den_ = rating[i] + rating[j] + nu*sqrt(rating[i]*rating[j]);
      alpha_[1] = rating[i]/den_;
      alpha_[3] = rating[j]/den_;
      alpha_[2] = 1 - alpha_[1] - alpha_[3];
      hth[i, j] = categorical_rng(alpha_);
    }
  }
}

