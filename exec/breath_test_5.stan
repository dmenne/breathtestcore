# Fit to 13C Curves using covariance estimate and LKJ prior

data{
  int<lower=0> N; # Number of data
  int<lower=0> NRecord; # Number of records
  int<lower=0> BreathTestRecordID[N]; # From BreathTestRecordID
  real<lower=0> lkj;
  real<lower=2> student_df; # When >= 10, assume gaussiang
  real<lower=0> Dose;
  vector<lower=0>[N] Time;
  vector<lower=-30>[N] PDR;
}

parameters{
  matrix [3, NRecord] cf_raw; # m, k, beta
  cholesky_factor_corr[3] L_Omega;
  vector<lower=0>[3] cf_mu;
  real <lower=0> sigma;
  vector<lower=0>[3] tau;
}


transformed parameters{
  matrix [3, NRecord] cf;
  cf = rep_matrix(cf_mu, NRecord) + diag_pre_multiply(tau, L_Omega)*cf_raw;
}

model {
  L_Omega ~ lkj_corr_cholesky(lkj);
  to_vector(cf_raw) ~ normal(0.,1.);
  tau ~ normal(0,1);
  cf_mu[1] ~ normal(40, 10);
  cf_mu[2] ~ lognormal(-5, 2);
  cf_mu[3] ~ normal(2, .2);

  sigma ~ cauchy(0,3);
  { # Block to hide pdr[n]
    vector[N] pdr;
    for (n in 1:N){
      int rec;
      real mn;
      real exp_ktn;
      real kn;
      real betan;
      rec = BreathTestRecordID[n];
      mn  =  cf[1, rec];
      kn = cf[2, rec];
      exp_ktn = exp(-kn* Time[n]);
      betan = cf[3, rec];
      pdr[n] = Dose*mn*kn*betan*exp_ktn * pow(1 - exp_ktn,(betan -1));
    }
    if (student_df < 10 )
      PDR ~ student_t(student_df, pdr, sigma);
    else
      PDR ~ normal(pdr, sigma);
  }

}

