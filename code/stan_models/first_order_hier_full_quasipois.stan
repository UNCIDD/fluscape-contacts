// Log linear regression model with hierarchical terms
// Clustering at the location, household, and round levels

// The input data is a vector 'y' of length 'N'.
data {
  // Sample sizes
  int<lower=0> N_obs; // number of observations
  int<lower=0> N_part; // number of participants
  int<lower=0> N_hh; // number of households
  int<lower=0> N_loc; // number of locations
  int<lower=0> N_rd; // number of rounds
  
  // Number of covariates
  int<lower=0> K_obs; // number of observation-level covariates
  int<lower=0> K_part; // number of participant-level covariates
  int<lower=0> K_hh; // number of household-level covatiates
  
  // data
  int<lower=0> y[N_obs]; // outcomes
  matrix[N_hh, K_hh] x_hh; // household-level covariates
  matrix[N_part, K_part] x_part; // participant-level covariates
  matrix[N_obs, K_obs] x_obs; // observation-level covariates
  
  // Lookup vectors
  int<lower=0> loc_lookup[N_hh]; // location ID corresponding to each household
  int<lower=0> hh_lookup[N_part]; // household ID corresponding to each participant
  int<lower=0> part_lookup[N_obs]; // participant ID corresponding to each observation
  int<lower=0> rd_lookup[N_obs]; // round ID corresponding to each observation
}

parameters {
  
  // location-level effects
    vector[N_loc] B0_loc; // Location-level intercepts
    real mu_loc; // mean of location-level intercepts
    real<lower=0, upper=10> sigma0_loc; // SD of location-level intercepts
    
  // household-level effects
    vector[N_hh] B0_hh; // Household-level intercepts
    vector[K_hh] B_hh; // array of location-level coefficient vectors
    real<lower=0, upper=10> sigma0_hh; // SD of household-level intercepts
    
  // round-level fixed effects
    vector[N_rd] B0_rd; 
    real<lower=0, upper=10> sigma0_rd; // SD of round-level intercepts
    
  // participant-level effects
    vector[N_part] B0_part; // Participant-level intercepts
    vector[K_part] B_part; // array of participant-level coefficient vectors
    real<lower=0, upper=10> sigma0_part; // SD of participant-level outcomes
    
    // observation-level effects
    vector[K_obs] B_obs; // array of participant-level coefficient vectors`
    
    vector[N_obs] B0_obs;
    real<lower=0, upper=10> sigma_obs;
}

transformed parameters {
  vector[N_loc] eff_loc; // vector of location-level total effects
  vector[N_hh] eff_hh; // vector of household-level total effects
  vector[N_part] eff_part; // vector of participant-level total effects
  
  for(l in 1:N_loc) {
    eff_loc[l] = B0_loc[l];
  }
  
  for(h in 1:N_hh) {
    eff_hh[h] = eff_loc[loc_lookup[h]] + B0_hh[h] + x_hh[h,]*B_hh;
  }
  
  for(p in 1:N_part) {
    eff_part[p] = eff_hh[hh_lookup[p]] + B0_part[p] + x_part[p,]*B_part;
  }
}

model {
  // location intercepts
  B0_loc ~ normal(mu_loc, sigma0_loc);
  
  // household intercepts
  B0_hh ~ normal(0, sigma0_hh);
  
  // participant intercepts
  B0_part ~ normal(0, sigma0_part);
  
  // observation intercepts
  B0_obs ~ normal(0, sigma_obs);
  
  // round intercepts
  B0_rd ~ normal(0, sigma0_rd);

  // observation level
  for(i in 1:N_obs) {
    y[i] ~ poisson_log(x_obs[i,]*B_obs + eff_part[part_lookup[i]] + B0_rd[rd_lookup[i]] + B0_obs[i]);  
  }
}