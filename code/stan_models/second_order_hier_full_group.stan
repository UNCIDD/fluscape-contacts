// Hierarchical logistic regression model
// Clusters at location, household, participant, round, and triangle levels

data {
  // Sample sizes
  int<lower=0> N_tri; // number of triangles
  int<lower=0> N_part; // number of participants
  int<lower=0> N_hh; // number of households
  int<lower=0> N_loc; // number of locations
  int<lower=0> N_rd; // number of rounds
  
  // Number of covariates
  int<lower=0> K_tri; // number of triangle-level covariates
  int<lower=0> K_part; // number of participant-level covariates
  int<lower=0> K_hh; // number of household-level covatiates
  
  // data
  int<lower=0, upper=1> y[N_tri]; // outcomes
  matrix[N_hh, K_hh] x_hh; // household-level covariates
  matrix[N_part, K_part] x_part; // household-level covariates
  matrix[N_tri, K_tri] x_tri; // triangle-level covariates
  
  // Lookup vectors
  int<lower=0> loc_lookup[N_hh]; // location ID corresponding to each household
  int<lower=0> hh_lookup[N_part]; // household ID corresponding to each participant
  int<lower=0> part_lookup[N_tri]; // participant ID corresponding to each triangle
  int<lower=0> rd_lookup[N_tri]; // round ID corresponding to each triangle
  
  // Group sizes
  vector[N_tri] g1; // tertiary contact group size
  vector[N_tri] g2; // random contact contact group size
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
    
  // participant-level effects
    vector[N_part] B0_part; // Participant-level intercepts
    vector[K_part] B_part; // array of participant-level coefficient vectors
    real<lower=0, upper=10> sigma0_part; // SD of participant-level intercepts
  
  // round-level fixed effects
    vector[N_rd] B0_rd; 
    real<lower=0, upper=10> sigma0_rd; // SD of round-level intercepts
    
  // triangle-level effects
    vector[K_tri] B_tri; // triangle-level coefficients
}

transformed parameters {
  vector[N_loc] eff_loc; // vector of location-level total effects
  vector[N_hh] eff_hh; // vector of household-level total effects
  vector[N_part] eff_part; // vector of household-level total effects
  
  
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
  
  vector[N_tri] log_odds;
  vector[N_tri] pr;
  
  // location intercepts
  B0_loc ~ normal(mu_loc, sigma0_loc);
  
  // household intercepts
  B0_hh ~ normal(0, sigma0_hh);
  
  // participant intercepts
  B0_part ~ normal(0, sigma0_part);
  
  // round intercepts
  B0_rd ~ normal(0, sigma0_rd);
  
  // triangle level
  for(i in 1:N_tri) {
    
    // Individual-level log odds of triangle existence
    log_odds[i] = x_tri[i,]*B_tri + eff_part[part_lookup[i]] + B0_rd[rd_lookup[i]];
    
    // Individual-level log odds of triangle existence
    pr[i] = exp(log_odds[i])/(1+exp(log_odds[i]));
    
    // Increment likelihood incorporating group size
    if(y[i] == 1) {
      target += log(1 - (1-pr[i])^(g1[i]*g2[i])); 
    } else {
      target += log((1-pr[i]))*(g1[i]*g2[i]);  
    }
  }
}
