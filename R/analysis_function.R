# SUPPORT FUNCTIONS FOR LOGIT ANALYSIS

# #################################################
# FUNCTIONS NEEDED FOR THE EXPERIMENT
# #################################################

message('Load functions needed for the experiment')

# definition of logit function. The derivatives necessary to compute Jeffrey's priors
# only 

logit <- function(x) { x = pmin(x, 100);  exp ( x) / (1 + exp ( x ) ) }
dlogitdx <- function(x)  x = pmax(-100, pmin(x, 100)); 
weightx <- function(x) { exp(x) / (1 + exp(x) )^2 } 


# ramp : function varying from 0 to 1

ramp <- function(Age, threshold, p1, deltaTime, deltaThreshold)
{
   ratio <- pmax(0, pmin(1, (Age - p1 + deltaTime / 2 )/deltaTime )) 
   return ( ratio  )
}

if (disable_ramp) ramp <- function(Age, ...) rep(0, length(Age))


# logit : estimate the probability of a Dataset, with params determining
# the ramp and regression coefficients. All data of the datasets are
# considered to be independent. 

logit_Data <- function(params, Data = Data)
{
  with(Data, 
  {
    if (disable_ramp)
    {
        X <<-  cbind(Time, CalIns - params['threshold'] )
    } else 
    {
       dramp    <- ramp ( Age, params['threshold'], params['p1'], params['deltaTime'], 
                       params['deltaThreshold'] ) 
        X <<-  cbind(Time, CalIns - ( params['threshold'] + dramp * params['deltaThreshold'] ) )
    }
    if ( prior_function == "Uniform")
    {
       b = c(b=as.numeric(exp(-params['sd_minus_1'])))
       a = c(a=as.numeric(params['slope'] * b ))

    } else
    {
       b = params['b']* 1000
       a = params['a']
    } 
    Beta <<- cbind (a * rep(1, nrow(X)), b)
  } )
  prob <- apply(  X * Beta , 1, sum)
  logit(prob)
}


# loglikelehood of a single data point (called by logit_Data)
# note: Score = 0 for intergrlacial, 1 for glacial
# but intermediate values to estimate the 'probability of it being an interglacial or a glacial'

loglik_Int <- function( prob, Score )
{
  ProbTrue <- prob * Score + ( 1 - prob ) * (1 - Score )
  sum( log ( ProbTrue)  )

}

loglik_Data <- function(params, Data)
{
  loglik_Int ( logit_Data(params, Data), Data$Score )
}

minus_loglik_Data <- function(...) - loglik_Data(...)

# Jeffreys prior for logit, for a single data point
# but based on the experiment design
# reference: Chen, Ibrahim and Kim, JASA 2008 (1) 

prior.jeffrey <- function(X, beta)
{
  Xbeta  <- X %*% beta    
#  weight <- weightx(Xbeta)
#  Xweight <- sweep(X, 1, weight, '*')
#  prior_matrix  <- t(Xweight) %*% X
  weights <- weightx(Xbeta); 
  prior = log  ( sqrt ( det ( t(X) %*% diag(as.numeric(weights)) %*% X ) ) )
  #prior = log  ( sqrt ( det ( prior_matrix ) ) )
  if (is.na(prior)) prior = -9999

  prior
}

if (prior_function == "Uniform")
prior.jeffrey <- function(...) 0 

# note: we consider uniform priors for ramp parameters params['slope'], ['threshold'], ['p1']
# and then Jeffrey's prior for time discount rate 

prior <- function(params, Data=Data)
{
  with(Data, 
  {
    dramp    <- ramp ( Age, params['threshold'], params['p1'], params['deltaTime'], 
                       params['deltaThreshold'] )

    # X <<-  cbind(Time, CalIns - dramp )
    if (disable_ramp)
     { X <<-  cbind(1, Time, CalIns -  params['threshold'] ) } else
     { X <<-  cbind(1, Time, CalIns - ( params['threshold'] + dramp * params['deltaThreshold'] ) ) }
    # Beta <<- c(params['slope'],  1.) /exp(params['sd_minus_1'] )

    if ( prior_function == "Uniform")
    {
       b = exp(-params['sd_minus_1'])
       a = params['slope'] * b 
    } else
    {
       b = params['b']* 1000
       a = params['a']
    } 

    Beta <<- c(0, a, b)
  } )
  prior <- prior.jeffrey(X, Beta)

  # bounded uniform (conditional) priors on ramp 
  if ( ! disable_ramp)
  {
  # further constraints to avoid excessive drifts
   if (params['deltaTime'] < 0  ) prior = -99999. 
   if (params['deltaTime'] > 1700  ) prior = -99999. 
   if (params['p1'] < 300 ) prior = -99999. 
   if (params['p1'] > 1700 ) prior = -99999. 
   if (abs(params['deltaThreshold'] > 0.05) ) prior = -99999. 
  }
   prior
   
#
}
 
posterior <- function(params, Data=Data) { 
    max( prior(params, Data) + loglik_Data(params, Data), -9999)}

# unifrom priors, as an alternative for the jeffrey priors
# (log-uniform for the standard deviation)

#   prior <- function(params)
#  {
#    prior = 0
#    #if (params['a'] < -7) prior = -99999. 
#    #if (params['b'] < -7) prior = -99999. 
#    if (params['deltaTime'] < 0  ) prior = -99999. 
#    if (params['deltaTime'] > 1700  ) prior = -99999. 
#    if (params['p1'] < 300 ) prior = -99999. 
#    if (params['p1'] > 1700 ) prior = -99999. 
#    if (abs(params['deltaThreshold'] > 0.05) ) prior = -99999. 
#    return(prior)
#  }
# 
# Monte-Carlo Markov chain
# during the first fifth of the run, we use a bigger proposal, to mix as well as we can, 
# and we also use an annealing factor of 1.4

# the part of the chain that will effectively be used for the analysis
# is its last half, furhter skimmed by conserving one value out of 500 
# it may be shown (off-line tests) that this is enough skimming to generate
# uncorrelated variables. 

mcmc <- function(init, loglik, prior,  proposal, ntot, verbose=FALSE, Data=Data)
{
  X = init;
  out = matrix(0, ntot, length(init)+3)
  colnames(out) = c(names(X), 'l', 'lstar', 'lr')
  l = loglik(X, Data)
  for (i in seq(ntot))
  {

    if (verbose)  cat(sprintf('iteration %i \n', i))
    annealing = 1
    BIG = ( i < (ntot / 5 ))
    if (BIG) annealing = 1.4
    P = proposal(X, big = BIG)
    Xstar = P$Xstar
    lstar = loglik(Xstar, Data)  + prior(Xstar, Data)
    lr = exp ( ( lstar - l + P$q1q2 ) / annealing)
    s = runif(1)
    if (lr > s)  { X = Xstar; l = lstar  } else  { }
    out[i,] = c(X, l, lstar, lr)
  }
  out
  keep = seq(ceiling(nrow(out)/2), nrow(out), 500)
  out[keep,]

}



# simulate a trajectory starting from 2690 ka BP. 

simul <- function(params, Data)
{
    m = nrow(Data)
    Simu =  rep(0, m)
    R = runif(m)
    LI = 2690
    for (i in seq(1,m))
    {
      TSL = LI - Data[i, "Age"]

      if ( prior_function == "Uniform")
      {
         b = exp(-params['sd_minus_1'])
         a = params['slope'] * b 
      } else
      {
         b = params['b']* 1000
         a = params['a']
      } 


      if (disable_ramp)
      {
        threshold <- + a * TSL + 
                        ( Data[i, "CalIns"] -  params['threshold'])* b
      } else 
      {
        dramp <-  ramp ( Data[i, "Age"], 
                  params['threshold'], params['p1'], params['deltaTime'], 
                  params['deltaThreshold'] )
        threshold <- + a * 
                       ( 1 - ramp_discount * dramp  ) * TSL +
                       ( ( Data[i, "CalIns"] -  params['threshold'] -  dramp *
                         params['deltaThreshold'] ) * b)
      }
      prob     <- logit ( threshold ) 
      if (prob == Inf) prob = 1.0
      if (prob == -Inf) prob = 0.0
  
      if (R[i] < prob)
      {
        Simu[i]  = 1
        LI  = Data[i, "Age"]
  
      }
    }
   Simu
}



############################################################################
############################################################################

# END OF FUNCTION DEFINITION

############################################################################
############################################################################


