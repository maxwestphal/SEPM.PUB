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
#   MLE_SIM_CPE_run.R: Conduct the simulation study (ML training, validation, evaluation)
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
dir.create(save.dir)

# Problem parameters --------------------------------------------------------------------------
mle_ids <- readr::read_csv(file.path(out.dir, "MLE_SIM_DATA_JP.csv", fsep=sep))$job.id 
mle_ids_eval_pos <- readRDS("SIM/MLE_SIM_CPE/mle_ids_eval_pos.rds")
mle_ids_eval <- mle_ids[mle_ids_eval_pos]

prob <- list()
prob[["load_instance"]] <-
  CJ(id = mle_ids_eval,
     filename = "mle_instance_",
     folder = data.dir,
     sep = "\\") 


# Algorithm parameters ------------------------------------------------------------------------
algo <- list()
algo[["study_mle"]] <- CJ(
  methods = NA,
  M = 200,
  M.start = NA,
  M.probs = "uniform",
  M.seed = 1,
  n.eval = c(400, 800),
  first.eval = 1,
  rdm.eval = FALSE,
  analysis = "cpe",
  delta = 0,
  shift = 0.05,
  select.method = c("best", "close", "oracle", "optimal"), 
  select.limit = c("sqrt"),
  select.args = "",
  estimate.method = "beta.approx",
  estimate.args = "",
  infer.method = "maxT",
  alternative = "greater",
  alpha = 0.025,
  transform = "none"
)

algo[[1]] <- algo[[1]] %>%
  mutate(first.eval = 1 + 100*(n.eval==200) +  300*(n.eval==400) + 
           700*(n.eval==800) +  1500*(n.eval==8000))           

# Registry setup ------------------------------------------------------------------------------
message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))

# Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="load_instance", fun=SEPM.SIM::load_instance, reg=reg)
addAlgorithm(name="study_mle", fun=SEPM.SIM::study_mle, reg=reg)

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
JP.data <- readr::read_csv(file.path(out.dir, "MLE_SIM_DATA_JP.csv", fsep=sep))
SEPM.SIM::save_results_mle(reg, JP.data, target=out.path)