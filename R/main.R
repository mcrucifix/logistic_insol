# #################################################
# LOAD THE R CODE FOR THE EXPERIMENT
# #################################################

message('Loading code ')
source('experiment.R')

# #################################################
# PARAMETERS OF THE EXPERIMENT
# #################################################

set.seed(123123)

message('Intialise parameters')

experiment_params_1 <- new.env()

# the experiment parameters are specified here. 
# the experiment output are stored in the RDATAPATH (../RData) path with name 'out_file'
# note that the experiment parameters are stored in the output file as well so that the experiments may easily
# be reproduced. 
# Good luck. Any question : michel.crucifix@uclouvain.be. 

local ({ 

downweighted = TRUE     # downweight data before a certain time (specage)
specage = 1000          # age before which specificity is applied
specificity = 1.00      # before 'specage' kaBP , specificity = sensitivity = this factor
                        # after  'specage', specificity = sensitivity = 100 % 
verbose = FALSE

ntot = 25000            # number of iterations of the Metropolis Hastings
n_simul=25000            # number of sample simulations to establish GIG sequence
npal = 40                # number of MCMC run in parallel

disable_ramp <- TRUE

start_age = 1000       # only consider data after that date

prior_function = "Jeffrey"

# set to 1 if ramp discount rate
ramp_discount = 0.
input_data = file.path(DATAPATH,"caloric_peaks.csv")
out_file = file.path(RDATAPATH, "params_mcmc_final_downweighted_j_specificity100_last1Myr.RData")

}, envir=experiment_params_1)


message('Perform experiment 1')
experiment(experiment_params_1)






