  data {
    int<lower=0> Np;
    int<lower=0> Nw;        
    int<lower=0> Nh; 
    int<lower=0> Ni;
    int<lower=0> M;          
    vector[Np] yp;
    vector[Np] xp;      
    vector[Nw] yw;     
    vector[Nw] xw;     
    vector[Nh] yh;     
    vector[Nh] xh;     
    vector[Ni] yi;     
    vector[Ni] xi; 
    vector[M] x_sim;        
  }
  parameters {
    real b0p, b1p;            
    real b0w, b1w;
    real b0m, b1m;
    real b0h, b0i;
    real<lower=0> sp, sw, sm;  
  }

  model {
    vector[Nh] ypl;
    vector[Ni] ywl; 
    // Priors
    b0p ~ normal(0, 10);
    b1p ~ normal(0, 10);
    b0w ~ normal(0, 10);
    b1w ~ normal(0, 10);
    b0m ~ normal(0, 10);
    b1m ~ normal(0, 10);
    b0h ~ normal(0, 10);
    b0i ~ normal(0, 10);
    sp ~ cauchy(0, 5);
    sw ~ cauchy(0, 5);
    sm ~ cauchy(0, 5);
   
    for (i in 1:Np) {
        target += normal_lpdf(yp[i] | b0p + b1p * xp[i], sp);
    }
    for (i in 1:Nw) {
        target += normal_lpdf(yw[i] | b0w + b1w * xw[i], sw);
    }
    for (i in 1:Nh) {
        ypl[i] = b0p + b1p * xh[i];         
        target += normal_lpdf(yh[i] | b0m + b1m * ypl[i] + b0h, sm);
    }
    for (i in 1:Ni) {
        ywl[i] = b0w + b1w * xi[i];          
        target += normal_lpdf(yi[i] | b0m + b1m * ywl[i] + b0i, sm);
    }
  }
  generated quantities {
    array[Np] real yp_ppc ;
    array[Nw] real yw_ppc ;
    array[Nh] real ypl_ ;
    array[Nh] real yh_ppc ;
    array[Ni] real ywl_ ; 
    array[Ni] real yi_ppc ;  
     
    array[M] real yp_sim ;
    array[M] real yw_sim ;
    array[M] real yh_sim ;
    array[M] real yi_sim ;
    for (i in 1:Np) {
      yp_ppc[i] = normal_rng(b0p + b1p * xp[i], sp);
    }
    for (i in 1:Nw) {
      yw_ppc[i] = normal_rng(b0w + b1w * xw[i], sw);
    }
    for (i in 1:Nh) {
      ypl_[i] = normal_rng(b0w + b1w * xh[i], sp);
      yh_ppc[i] = normal_rng(b0m + b1m * ypl_[i] + b0h, sm);
    }
    for (i in 1:Ni) {
      ywl_[i] = normal_rng(b0w + b1w * xi[i], sw);
      yi_ppc[i] = normal_rng(b0m + b1m * ywl_[i] + b0i, sm);
    }
    for (i in 1:M) {
      yp_sim[i] = normal_rng(b0p + b1p * x_sim[i], sp);
      yw_sim[i] = normal_rng(b0w + b1w * x_sim[i], sw);
      yh_sim[i] = normal_rng(b0m + b1m * yp_sim[i] + b0h, sm);
      yi_sim[i] = normal_rng(b0m + b1m * yw_sim[i] + b0i, sm);
    }
  }  
