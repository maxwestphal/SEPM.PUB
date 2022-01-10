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
#   LFC_SIM_CPE: Simulation study - least favorable parameters for assessment of
#   co-primary endpoints sensitivity and specificity of binary classifiers
###
#   Script:
#   LFC_SIM_CPE_run.R: Conduct the simulation study (synthetic data generation, eval study)
###



# Dependencies --------------------------------------------------------------------------------
library(SEPM.SIM)
library(batchtools)
library(data.table)
library(parallel)
library(dplyr)

# Preparation (+++ MODIFY IF NEEDED+++) -------------------------------------------------------
rm(list = ls())                                               # clear environment

sim <- "LFC_SIM_CPE"                                          # simulation name
sep <- "\\"                                                   # path separator
Nsim <- 10000                                                 # number of simulations
cr <- 7/8                                                     # fraction of cores used
main.dir <- file.path("E:", "LFC_SIM", fsep=sep)              # main folder

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
prob[["sample_lfc_cpe"]] <- CJ(
  n = c(200, 400, 800, 4000, 20000), 
  prev = c(0.2, 0.5), 
  S = c(1, 10, 20), 
  B = NA,
  se = c(0.8, 0.9),
  sp = NA,
  eps = c(0, 0.001, 0.002),
  L = c(1, 0.95),
  corr.se = "type=equi_rho=0.5", 
  corr.sp = "type=equi_rho=0.5"
)

prob[["sample_lfc_cpe"]] <- 
  prob[["sample_lfc_cpe"]] %>%
  mutate(sp=se, B=round(S/2)) %>%
  filter(! (S==1 & eps != 0)) %>% 
  filter(! (prev==0.5 & eps != 0))


# Algorithm parameters ------------------------------------------------------------------------
algo <- list()
algo[["study_lfc_cpe"]] <- CJ(
  alternative = "greater",
  alpha = 0.025,
  estimate = "beta.approx",
  infer = "maxT",
  transform = "none"
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
addProblem(name="sample_lfc_cpe", fun=SEPM.SIM::sample_lfc_cpe, reg=reg)
addAlgorithm(name="study_lfc_cpe", fun=SEPM.SIM::study_lfc_cpe, reg=reg)

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
P <- getJobPars() %>% unwrap()
R <- reduceResultsDataTable() %>% unwrap()
D <- dplyr::left_join(P, R)

readr::write_csv(D, out.path)
