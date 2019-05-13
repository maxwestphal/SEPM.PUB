### SIMULATION STUDY: MACHINE LEARNING AND EVALUATION 
#   MODEL EVALUATION (MLE_SIM_ACC_FULL_EXT)

### INFO
#   Purpose: Data generation (model evaluation)
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

sim <- "MLE_SIM_POWER_ACC"                # simulation name
Nsim <- 1                                 # number of simulations
cr <- 0.8                                 # fraction of cores used
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
prob[["power_accuracy"]] <-
  CJ(load.folder = load.dir,
     load.base = "job",
     load.id = ids,
     methods = NA,
     M = 200,
     M.start = 1, 
     M.probs = NA,
     M.seed = NA,
     eval.n = c(100,200,400,800),
     delta = 0.05,
     select.method = c("rank", "se"),
     select.args = c("r=1", "c=1"),
     select.limit = c("sqrt"),
     estimate.method = "beta.approx",
     estimate.args= "",
     known.gamma = c(FALSE), # = known.delta previously
     alpha = 0.025,
     alternative = "greater",
     power.method = "conditional",
     correct.method = "none")

prob[[1]] <- prob[[1]] %>%
  filter(! (select.method=="rank"   
            & (!select.args %in% c("r=1") | !select.limit %in% "sqrt") )) %>%
  filter(! (select.method=="se"     & select.limit=="sqrt" &
              (!select.args %in% c("c=1"))))


#prob[[1]] %>% filter(eval.n == 200, load.id==1)

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
addProblem(name="power_accuracy", data=NULL, fun=SEPM.MLE::power_accuracy, reg=reg)
addAlgorithm(name="id", fun=NULL, reg=reg)

#   Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=Nsim,
               reg=reg)
# summarizeExperiments(reg=reg)

### TEST 
j = 124 # findExperiments(prob.pars=infer.method=="univariate", reg=reg)[1,]
testJob(j, reg=reg)
unwrap(getJobPars(j, reg=reg))

### RUN JOBS
subs.ids <- JP.data %>% filter(red==0) %>% {.$job.id} 
prio.ids <- findExperiments(prob.pars = load.id %in% subs.ids, reg=reg)
Sys.time()
submitJobs(intersect(findNotDone(reg=reg), prio.ids), reg=reg) # TODO: add removed IDs back in 
Sys.time()

### OPTIONAL STEPS
getStatus(reg=reg)

#findExpired(reg=reg)
#removeExperiments(78, reg=reg)
#waitForJobs(reg=reg)
#getErrorMessages()
#killJobs(reg=reg)
#clearRegistry(reg=reg)

### SAVE_RESULTS:
save_results(reg, JP.data, target=out.path, type="power")





