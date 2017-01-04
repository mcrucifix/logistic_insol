experiment <- function(run.params)

{
  
  # #################################################
  # READ INPUT DATA FILE
  # #################################################
  
  message('attach parameters')
  attach(run.params)

  source('analysis_function.R')


  message('Read Input Data File')
  Data <- read.csv(input_data, 
       col.names=c('Index','Age','Name','Int','Open', 'CalIns','Time','Combo'))
  
  
  # Continued integlacials are a 'non deglaciation', i.e., score = 0
  
  Data$Score = Data$Int
  Data$Score [which(Data$Int == 2)] = 0.
  
  
  # downweight all scores before specage


if (downweighted)
{
  factor1 = 1 - specificity 
  factor2 = 1 - 2 * (1 - specificity )

  Data$Score [which(Data$Age > specage )] = 
            factor1 + factor2  * Data$Score [(which(Data$Age > specage))] 
} else {
  Data$Score [which(Data$Open == 1)] = 
          factor1 + factor2  * Data$Score [(which(Data$Open == 1))] 
}

# discard data before start_age

i <- which ( Data$Age < start_age )
if (length(i) > 0) 
 {  Data <- Data[i, ] } else
 {  stop ("Starting age requested leaves no data to use ") }


  
# initial parameters (also found with some trial and error)

init.params <- list(threshold = 6.1432207, a = 0.86640, b = 0.4000215 , 
            p1 = 1079, deltaTime = 835, deltaThreshold = -0.11660)


if (disable_ramp) init.params <- list(threshold = 6.1432207, a = 0.86640, b = 0.4000215)

# for uniform / logunifarm priors we need to define the parameters 
# differently. We consider the 'slope' and the inverse of the
# standard deviation
# this is not use in the final version of the Nature paper 

if (prior_function == "Uniform")
{
  init.params$sd_minus_1 = - log (init.params$b * 1000 )
  init.params$slope = init.params$a * exp ( init.params$sd_minus_1 )
  init.params$a = NULL
  init.params$b = NULL
}

init.params <- simplify2array(init.params)

print (' INIT PARAMS ')
print(init.params)




message('compute hessian matrix at posterior maximum (for proposal')
# O <- optim(params, minus_loglik_Data)

M2 <- optim(init.params, function(...) -posterior(...),  Data=Data,  hessian = TRUE, method = "L-BFGS-B" , lower = c(-9999, -8, -9999))


 
  # NOTES WRITTEN ON Fri Sep 23 12:19:56 2016 based on experiments of this very week. 
  # ---------------------------------------------------------------------------------
  # we use here the optim function to find the posterior maximum. 
  # There are things I still don't quite understand. 
  # Indeed, according to Chen et al. 2008, the prior should be unimodal, but the optim routien
  # run on the prior sometimes finds local minima. I first thought this was related to the
  # oddities of the ramp function, but even without the ramp it still finds local maxima. 
  # This is not something that should happen and  needs to be investigated, 
  # but for sure, after careful checks of the math, my code fro the prior is what it is meant to be. 
  # The idea of Chen et al. is that given that the prior is unimodal and after all quite smoooth, 
  # one should be content with importance sampling using the Hessian.
  # Chen's central therom is that the prior has lighter tails than a Normal, so
  # all should go well and there is no  need for MCMC. 
  # Here the compromise is to use the Hession as a way to define a proposal for the MCMC chain. 
  # after a bit of tuning, this seems to work, but the variables 'a' and 'b' are still quite 
  # correlated in the chain so I can't claim that even the skimmed version is fully decorellated. 
  # In the original version of this routine (with uniform priors), 
  # I used a more heuristic 'guessed' for the proposol to start with
  # and then made a first MCMC to refine the proposal, do SVD analysis of that one, and finally
  # only started the parallel MCMC with the refined proposal. 
  # This worked well as well as long as the model was parameterised
  # under the form logit = ( E -  E_effective )/sigma , where sigma has a log-uniform prior.
  # Here, we used a parameterisation compatible with Jeffrey's prior, which implies
  # logit = a + b * Time + c * (Ecal - ramp(t)) , but a, b , and c end up to be really strongly
  # correlated and this posterior is much harder to explore with a MCMC. 
  #                                                                 M. Crucifix
  # ---------------------------------------------------------------------------------
  
  message ('solve Hessian') 
  E = eigen(solve(M2$hessian))
  
  
  V <- sweep(t(E$vectors), 1, sqrt(E$values),  '*')
  
  message ('The V matrix is ')
  print(V)
  
  # uniform priors
  
  
  
  n <- length(init.params) 
  
  proposal <- function(params, big=FALSE)
   { 
     np = length(params)
     rn = rnorm(np)
     D <-  as.numeric( t(rnorm(np)) %*% V / 2  )
     # if (big) D = D*2
     # some hand tuning when using the ramp... 
     # to prevent numerical instabilities (this was trial/error)
     # note that tuning the proposal is normal practice
     if (! (disable_ramp))
     {
       D[1] = D[1]*3
       D[6] = D[6]*3
  
       D[2] = D[2]*2.3
       D[3] = D[3]*2.3
     }
   
  
     return(list( Xstar = params + D , q1q2 = 0))
   }
 



  require(parallel)
  
  
  message('parrallel MCMC')
  
  
  M <- mclapply( seq(npal), function(i)
       {
          if (verbose) cat (sprintf("Markov Chain nr %i \n", i ))
          mcmc(proposal(init.params, big=TRUE)$Xstar, loglik_Data, prior, proposal, ntot, verbose, Data=Data)},
          mc.cores=8
       )
  
  
  
  
  # now that we have the posterior parameters, 
  # aggregate multiple mcmc
  # we have indeed npal MCMC, each of which should in principle sample the same 
  # distribution. However, there is always the risk that some has been locked
  # into a local maximum. The integral of the posterior, sampled by the chain,
  
  # see http://arxiv.org/pdf/0907.5123v1.pdf 
  # keep the 25% of highest lik and then take the harmonic mean. This will do the job. 
  
  
  ntot_k = nrow(M[[1]])
  init = sample(npal,1)
  rnum = runif(ntot)
  
  
  upper_ratio <- 0.25
  
  probs <- sapply(M, function(m) 
    {
      post_log <- m[,'l']
      # keep upper 25% of the cdf
      post_log <- post_log [ 
            order ( post_log , decreasing = TRUE)[seq(ceiling(upper_ratio * length(post_log)))] ]
    
      # hormonic mean  of the upper 25 % 
      # see http://arxiv.org/pdf/0907.5123v1.pdf 
      1./mean(1./exp(post_log)) }
    ) 
    
  
  # sample the chains with weights equal to to the integrated posterios of each chain 
  
  nc = sample(npal, ntot_k * npal / 2, prob = probs, replace=TRUE)
  out = matrix(0, length(nc), length(M[[1]][1,]) + 1 )
  colnames(out) = c(names(init.params), 'l', 'lstar', 'lr', 'nc')
  
  J = sample( ntot_k, ntot_k * npal / 2, replace=TRUE)
   
  for (i in seq(length(nc)))
  {
    out[i, ] = c(M[[nc[i]]][J[i], ] , nc[i])
  }
  
  
  n = length(init.params)
  
  out.params = out[ , seq(n)]
  liks  = out[ , 'l']
  ncs  = out[ , 'nc']
  
  # perform simulations
  
  message('almost finished. Perform simulations')
  
  sample_simul = sample( nrow(out.params), n_simul , replace=TRUE)
  
  message('start simulations...')
  simuls = t(  apply(out.params[sample_simul,],  1, simul, Data=Data) )
  
  
  detach(run.params)
  
  message('save output')
  
  save(run.params, Data, M, out.params, simuls, liks, ncs, file = run.params$out_file)
  
}



