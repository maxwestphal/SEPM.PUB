###
#   Author: 
#   Max Westphal (mwestphal@uni-bremen.de)
###
#   Date:
#   2020-03-20
###
#   Project:
#   SIMPle: Simultaneous Inference for Multiple Proportions
###
#   Task: 
#   MBB_SIM_ACC: Simulation study - assessment of credible regions based on 
#   multivariate beta-binomial model
###
#   Script:
#   MBB_SIM_ACC_run.R: Conduct the simulation study 
###



# Dependencies --------------------------------------------------------------------------------
library(SEPM.SIM)
library(batchtools)
library(data.table)
library(parallel)
library(dplyr)


# Preparation (+++ MODIFY IF NEEDED+++) -------------------------------------------------------
rm(list = ls())                                               # clear environment

sim <- "MBB_SIM_ACC"                                          # simulation name
sep <- "\\"                                                   # path separator
Nsim <- 500                                                   # number of simulations
cr <- 7/8                                                     # fraction of cores used
main.dir <- file.path("E:", "MBB_SIM", fsep=sep)              # main folder

req.packages <- c("SIMPle", "SEPM", "SEPM.SIM")               # required packages
reg.dir <- file.path(main.dir, sim, fsep=sep)                 # registry folder
data.dir <- file.path(main.dir, "DATA", fsep=sep)             # data folder
out.dir <- file.path(main.dir, "RESULTS", fsep=sep)           # results folder
out.path <- file.path(out.dir, paste0(sim, ".csv"), fsep=sep) # results path

# Create folders:
dir.create(main.dir)
dir.create(out.dir)
dir.create(save.dir)


# Problem parameters --------------------------------------------------------------------------
prob <- list()
prob[["sample_mbb_acc"]] <- CJ(
  gd =  names(SEPM.SIM:::DISTS)[1:8],
  n = c(50, 100, 200, 400, 800),
  nrep = 100
)


# Algorithm parameters ------------------------------------------------------------------------
algo <- list()
algo[["study_mbb_acc"]] <- CJ(
  prob = 0.95,
  method = c("sample", "copula", "approx"),
  prior = c("vague", "correct"),
  contrast = c("raw", "rdm")
)


# Registry setup ------------------------------------------------------------------------------
message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))

# Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="sample_mbb_acc", fun=SEPM.SIM::sample_mbb_acc, reg=reg)
addAlgorithm(name="study_mbb_acc", fun=SEPM.SIM::study_mbb_acc, reg=reg)

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


# Save results --------------------------------------------------------------------------------
D <- reduceResultsList(fun = function(x, job){data.table(job.id=job$id, x)}) %>% rbindlist()

head(D)

readr::write_csv(D, out.path)








