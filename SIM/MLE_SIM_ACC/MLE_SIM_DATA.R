###
#   Author: 
#   Max Westphal (mwestphal@uni-bremen.de)
###
#   Date:
#   2021-12-12
###
#   Project:
#   CPE: A multiple testing framework for diagnostic accuracy studies with co-primary endpoints
###
#   Task: 
#   CPE_SIM_MLE: Simulation study - machine learning and evaluation
#   for co-primary endpoints sensitivity and specificity of binary classifiers
###
#   Script:
#   MLE_SIM_DATA.R: Generation of MLE simulation instances (i.e. model training, validation, evaluation)
###
#   Note:
#   The simulation studies MLE_SIM_ACC and MLE_SIM_CPE utilize the same simulation database 
#   as generated by this script.
###



# Dependencies --------------------------------------------------------------------------------
library(SEPM.SIM)
library(batchtools)
library(data.table)
library(parallel)
library(dplyr)

# Preparation (+++ MODIFY IF NEEDED+++) -------------------------------------------------------
rm(list = ls())                                               # clear environment

sim <- "MLE_SIM_DATA"                                         # simulation name
sep <- "\\"                                                   # path separator
Nsim <- 3000                                                  # number of simulations 
cr <- 7/8                                                     # fraction of cores used
main.dir <- file.path("E:", "MLE_SIM", fsep=sep)              # main folder

req.packages <- c("SIMPle", "SEPM", "SEPM.SIM")               # required packages
reg.dir <- file.path(main.dir, sim, fsep=sep)                 # registry folder
data.dir <- file.path(main.dir, "DATA", fsep=sep)             # data folder
out.dir <- file.path(main.dir, "RESULTS", fsep=sep)           # results folder
out.path <- file.path(out.dir, paste0(sim, ".csv"), fsep=sep) # results path

# Create folders:
dir.create(main.dir)
dir.create(out.dir)
dir.create(data.dir)

# Problem parameters --------------------------------------------------------------------------
prob <- list()
prob[["sample_mle"]] <-
  CJ(n.learn = c(400, 800),
     ratio.lv = 0.75,
     n.eval = 10000,  
     n.pop = 100000,
     P = 50,
     red = c(0, 1),   
     rho = 0.5, 
     scenario = c("EOMPM_A2", "EOMPM_B2",
                  "MLE_SIM_F1_prev30", "MLE_SIM_F1_prev15",
                  "MLE_SIM_F13_prev30", "MLE_SIM_F13_prev15"),
     methods = "glmnet_xgbTree_rpartCost_svmLinearWeights2", 
     M = 50,                                                                     
     tuning = "random")


# Algorithm parameters ------------------------------------------------------------------------
algo <- list()
algo[["save_instance"]] <- 
  CJ(id = NA,
     filename = "mle_instance_",
     folder = data.dir,
     sep = sep,
     write = TRUE)

# Registry setup ------------------------------------------------------------------------------
message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))

# Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="sample_mle", fun=SEPM.SIM::sample_mle, reg=reg)
addAlgorithm(name="save_instance", fun=SEPM.SIM::save_instance, reg=reg)

# Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls = Nsim,
               reg = reg)
summarizeExperiments(reg = reg)

# Test job:
j <- findNotDone()[1]
j
r <- testJob(j, reg=reg)
r
unwrap(getJobPars(j))

# Run jobs ------------------------------------------------------------------------------------
Sys.time()
submitJobs(findNotDone(), reg=reg) 
Sys.time()

Sys.sleep(180)
getStatus(reg=reg)

# Optional steps ------------------------------------------------------------------------------
#findExpired(reg=reg)
#removeExperiments(findErrors(reg=reg), reg=reg)
#waitForJobs(reg=reg)
#getErrorMessages()
#killJobs(reg=reg)
#clearRegistry(reg=reg)



