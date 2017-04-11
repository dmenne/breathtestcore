# Simple non-hierarchial fit of Breath Test Curves to Exponential beta
# Version for D13CBreath
# Added student-t
# Non-centered reparametrization of m (or "Matt's Trick")
# Works !!

data{
  int<lower=0> N; # Number of data
  int<lower=0> NRecord; # Number of records
  int<lower=1> student_df; # using Gaussian for student_df >= 10
  real<lower=0> Dose;
  int<lower=0> BreathTestRecordID[N]; # From BreathTestRecordID
  vector<lower=0>[N] Time;
  vector<lower=-30>[N] PDR;
}

parameters{
  vector[NRecord] m_raw;
  real<lower=0> mu_m;
  real<lower=0> sigma_m;

  vector[NRecord] k_raw;
  real<lower=0> mu_k;
  real<lower=0> sigma_k;

  vector[NRecord] beta_raw;
  real<lower=0> mu_beta;
  real<lower=0> sigma_beta;

  real <lower=0> sigma;
}


transformed parameters{
  vector<lower=0>[NRecord] m;
  vector<lower=0>[NRecord] k;
  vector<lower=0>[NRecord] beta;

  # Re-parametrization
  m    = mu_m + sigma_m * m_raw;
  k    = mu_k + sigma_k * k_raw;
  beta = mu_beta + sigma_beta * beta_raw;

}

model {
  # Note: the x_raw parameters all have normal(0,1) here
  m_raw ~ normal(0, 1);
  mu_m ~ normal(40,20);
  sigma_m ~ normal(6,2);

  k_raw ~ normal(0, 1);
  mu_k ~ lognormal(-5, 2);
  sigma_k ~ lognormal(-7, 2);

  beta_raw ~ normal(0, 1);
  mu_beta ~ normal(2,0.1);
  sigma_beta ~ cauchy(0,2);

  sigma ~ cauchy(0,5);
  { # Block to hide pdr[n]
    vector[N] pdr;
    for (n in 1:N){
      int rec;
      real mn;
      real exp_ktn;
      real kn;
      real betan;
      rec = BreathTestRecordID[n];
      mn  =  m[rec];
      kn = k[rec];
      exp_ktn = exp(-kn* Time[n]);
      betan = beta[rec];
      pdr[n] = Dose*mn*kn*betan*exp_ktn * pow(1 - exp_ktn,(betan -1));
    }
    if (student_df <= 10 )
      PDR ~ student_t(student_df, pdr, sigma);
    else
      PDR ~ normal(pdr, sigma);
  }

}


generated quantities {
  vector[N] PDR_rep ;
  for (n in 1:N) {
    int rec;
    real mn;
    real exp_ktn;
    real kn;
    real betan;
    real pdr_rep;
    rec = BreathTestRecordID[n];
    mn  =  m[rec];
    kn = k[rec];
    exp_ktn = exp(-kn* Time[n]);
    betan = beta[rec];
    pdr_rep = Dose*mn*kn*betan*exp_ktn * pow(1 - exp_ktn,(betan -1));
    if (student_df <=10 )
      PDR_rep[n] = student_t_rng(student_df, pdr_rep, sigma) ;
    else
      PDR_rep[n] = normal_rng(pdr_rep, sigma) ;
  }
}
