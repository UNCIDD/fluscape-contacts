// Non-hierarchical logistic regression model, unadjusted

data {
  // Sample sizes
  int<lower=0> N_tri; // number of triangles
  
  // Number of covariates
  int<lower=0> K_tri; // number of triangle-level covariates
  
  // data
  int<lower=0, upper=1> y[N_tri]; // outcomes
  matrix[N_tri, K_tri] x_tri; // triangle-level covariates
  
  // Group sizes
  vector[N_tri] g1; // tertiary contact group size
  vector[N_tri] g2; // random contact contact group size
}

parameters {
    
  // intercept
  real B0;
  
  // triangle-level coefficients
  vector[K_tri] B_tri; // array of triangle-level coefficient vectors
}

model {
  
  vector[N_tri] log_odds;
  vector[N_tri] pr;

  // triangle level
  for(i in 1:N_tri) {
    
    //print("pr[i]:", pr[i]);
    
    // Individual-level log odds of triangle existence
    log_odds[i] = x_tri[i,]*B_tri + B0;
    
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

generated quantities {
  vector[K_tri+1] P;
  vector[K_tri+1] O;
  
  O[1] = exp(B0);
  P[1] = O[1]/(1+O[1]);
  
  for(i in 2:(K_tri+1)) {
    O[i] = exp(B0 + B_tri[i-1]);
    P[i] = O[i]/(1+O[i]);
  }
}
