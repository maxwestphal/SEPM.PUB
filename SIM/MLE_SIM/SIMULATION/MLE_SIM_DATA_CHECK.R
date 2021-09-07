### SIMULATION STUDY: MACHINE LEARNING AND EVALUATION
#   DATA GENERATION (MLE_SIM_DATA_CHECK)

### INFO
#   Purpose: Data generation (machine learning)
#   Author: Max Westphal (mwestphal@uni-bremen.de; https://github.com/maxwestphal)
#   Date: 10/10/2018

### PACKAGES
require(SEPM.MLE)
require(batchtools)
require(data.table)
require(parallel)
require(dplyr)

### PREPERATION (+++ MODIFY IF NEEDED +++)
rm(list = ls())                           # clear environment

sim <- "MLE_SIM_DATA_CHECK"               # simulation name
Nsim <- 100                               # number of simulations
cr <- 0.9                                 # fraction of cores used
req.packages <- c("SEPM", "SEPM.MLE")     # required packages
main.dir <- "D:/SIM/MLE_SIM"              # main folder
reg.dir <- paste(main.dir, sim, sep="/")  # registry folder
out.dir <- paste0(main.dir, "/RESULTS")   # results folder
out.path <- paste(out.dir, paste0(sim, ".csv"), sep="/") # results path

dir.create(main.dir)
dir.create(out.dir)

### PARAMETER CONFIGURATION (+++ MODIFY IF NEEDED +++)
save.dir <- paste0(main.dir, "/DATA")     # data path
dir.create(save.dir)

prob <- list()
prob[["generate_data"]] <-
  CJ(n.learn = c(400, 800),
     ratio.lv = 0.75,
     n.eval = 10000,  
     n.pop = 100000,
     P = 50,
     red = c(0, 1),   
     rho = 0.5, 
     scenario = c("EOMPM_A2", "EOMPM_B2", "EOMPM_A", "EOMPM_B"),
     methods = "glmnet_xgbTree_rpartCost_svmLinearWeights2", 
     M = 50,                                                                     
     tuning = "random")

algo <- list()
algo[["save_data"]] <- 
  CJ(write = TRUE,
     filename = "job",
     subfolder="DATA",
     folder=save.dir,
     sep="/")

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))

### REGISTRY SETUP
#   Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="generate_data", data=NULL, fun=SEPM.MLE::generate_data, reg=reg)
addAlgorithm(name = "save_data", fun=SEPM.MLE::save_data, reg=reg)

#   Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=Nsim,
               reg=reg)
# summarizeExperiments(reg=reg)

### TEST 
j = 1
testJob(j, reg=reg)
unwrap(getJobPars(j, reg=reg))

### RUN JOBS
Sys.time()
submitJobs(reg=reg)
Sys.time()

### OPTIONAL STEPS
getStatus(reg=reg)

#findExpired(reg=reg)
#removeExperiments(findExpired(reg=reg), reg=reg)
#waitForJobs(reg=reg)
#getErrorMessages()
#killJobs(reg=reg)
#clearRegistry(reg=reg)

### SAVE RESULTS
#   Save jop pars
JP <- unwrap(getJobPars(reg=reg))
readr::write_csv(JP, paste0(out.dir, "/", sim, "_JP.csv"))

