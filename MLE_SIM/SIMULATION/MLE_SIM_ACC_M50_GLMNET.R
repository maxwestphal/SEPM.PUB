### SIMULATION STUDY: MACHINE LEARNING AND EVALUATION 
#   MODEL EVALUATION (MLE_SIM_ACC_M50_GLMNET)

### INFO
#   Purpose: Data generation (model evaluation)
#   Author: Max Westphal (mwestphal@uni-bremen.de; https://github.com/maxwestphal)
#   Date: 19/12/2018

### PACKAGES
require(SEPM.MLE)
require(batchtools)
require(data.table)
require(parallel)
require(dplyr)

### PREPERATION (+++ MODIFY IF NEEDED +++)
rm(list = ls())                           # clear environment

sim <- "MLE_SIM_ACC_M50_GLMNET"             # simulation name
Nsim <- 1                                 # number of simulations
cr <- 0.9                                 # fraction of cores used
req.packages <- c("SEPM", "SEPM.MLE")     # required packages
main.dir <- "D:/SIM/MLE_SIM"              # main folder
reg.dir <- paste(main.dir, sim, sep="/")  # registry folder
out.dir <- paste0(main.dir, "/RESULTS")   # results folder
out.path <- paste(out.dir, paste0(sim, ".csv"), sep="/") # results path

dir.create(main.dir)
dir.create(out.dir)

### PARAMETER CONFIGURATION (+++ MODIFY IF NEEDED +++)
load.dir <- paste0(main.dir, "/DATA")     # save.dir of MLE_SIM_DATA
load.dir <- "E:/DataScience/R/SIM/MLE_SIM/DATA" 

JP.data <- readr::read_csv("E:/DataScience/R/SIM/MLE_SIM/RESULTS/MLE_SIM_DATA_JP.csv")
ids <- JP.data$job.id

prob <- list()
prob[["study_accuracy"]] <- 
  CJ(load.folder = load.dir,
     load.base = "job",
     load.id = ids,
     methods = c("glmnet"),
     M = 50,
     M.start = 1, 
     M.probs = "uniform",
     M.seed = 1337,
     eval.n = c(100,200,400,800,8000),
     eval.first = 1,
     eval.rdm = FALSE,
     select.method = c("rank", "se", "oracle", "simplest.en"), 
     select.args = c("r=1", "c=1", "learn", ""),
     select.limit = "sqrt",
     known.delta = c(FALSE),
     estimate.method = "beta.approx",
     estimate.args= "",
     infer.method = "maxT",
     alpha = 0.025,
     alternative = "greater",
     transform = "none")

prob[[1]] <- prob[[1]] %>%
  filter(! (select.method=="rank"   & !select.args %in% c("r=1") )) %>%
  filter(! (select.method=="se"     & !select.args %in% c("c=1")) ) %>%
  filter(! (select.method=="simplest.en" & !select.args %in% c("")) ) %>%
  filter(! (select.method=="oracle" & ! (select.args %in% c("train", "learn") & !known.delta)) ) %>%
  filter((!is.na(M.start) & M.probs=="uniform")|(is.na(M.start) & M.probs!="uniform")  ) %>%
  mutate(eval.first = 1 + 100*(eval.n==200) + 300*(eval.n==400) + 700*(eval.n==800) + 1500*(eval.n==8000))

algo <- list()
algo[["id"]] <- data.table()

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))

### REGISTRY SETUP
#   Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="study_accuracy", data=NULL, fun=SEPM.MLE::study_accuracy, reg=reg)
addAlgorithm(name="id", fun=NULL, reg=reg)

#   Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=Nsim,
               reg=reg)
# summarizeExperiments(reg=reg)

### TEST 
j = 2
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

### SAVE_RESULTS:
save_results(reg, JP.data, target=out.path)

