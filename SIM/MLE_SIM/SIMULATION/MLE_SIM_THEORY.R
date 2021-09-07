### SIMULATION STUDY: MACHINE LEARNING AND EVALUATION 
#   MODEL EVALUATION (MLE_SIM_THEORY)

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

sim <- "MLE_SIM_THEORY"                   # simulation name
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
prob[["theory"]] <- 
  CJ(load.folder = load.dir,
     load.base = "job",
     load.id = ids,
     methods = NA,
     M = c(100, 200),
     M.start = 1,
     M.probs = "uniform",
     M.seed = 1337,
     eval.first = 1,
     eval.n = 100, #c(0 , 25, 50, 100, 200, 400) # attention: for slot 'data' only single value needed
     eval.rdm = FALSE)

prob[[1]] <- prob[[1]] %>%
  mutate(eval.first = 1 + 100*(eval.n==200) + 300*(eval.n==400) + 700*(eval.n==800) 
         + 1500*(eval.n==8000) + 9500*(eval.n==50))

algo <- list()
#algo[["id"]] <- data.table()

k.sep <- paste(c(0,1e-9, seq(0.2, 5, 0.2), seq(5.5, 10, 0.5), 15, 20, 25, Inf), collapse=",")  
S.sep <- paste(c(seq(1, 20, 1), seq(12, 20, 2), seq(25, 100, 5), 101, seq(120, 180, 20), 199, 200, 201), collapse=",")
p.sep <- paste(seq(0,1,0.02), collapse=",")

algo[["aggregate_by"]] <- 
  data.table(var= c(rep("final.theta.mean", 2), rep("learn.acc.pop", 2), "gamma", "train.eps.val",
                    "final.theta.nodata", "max.theta.S"),
             by = c("k.se", "S", c("train.acc.val", "train.acc.pop"), rep("val.rank.rdm", 2), rep("S", 2)),
             breaks = c(k.sep, S.sep, rep(p.sep, 2), rep(S.sep, 4)),
             slot= c(rep("theta.curve", 2), rep("data", 4), rep("theta.curve", 2)),
             query="TRUE",
             out.char="")

algo[[1]]

#algo[["aggregate_by"]] <- 
#  algo[["aggregate_by"]] %>% 
#  filter(! (var %in% c("gamma", "train.eps.val", "learn.acc.pop") & eval.n != 100 ))

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", sum(sapply(algo, nrow)))

### REGISTRY SETUP
#   Create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))

# Add problems / algorithms:
addProblem(name="theory", data=NULL, fun=SEPM.MLE::theory, reg=reg)
#addAlgorithm(name="id", fun=NULL, reg=reg)
addAlgorithm(name="aggregate_by", fun=SEPM.MLE::aggregate_by, reg=reg)

#   Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=Nsim,
               reg=reg)
# summarizeExperiments(reg=reg)

### TEST 
j =  findExperiments(algo.pars=var=="max.theta.S", reg=reg)[1,]
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
save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_final.theta.mean_BY_S.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="final.theta.mean" & by =="S", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_final.theta.mean_BY_k.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="final.theta.mean" & by =="k.se", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_learn.acc.pop_BY_train.acc.val.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="learn.acc.pop" & by =="train.acc.val", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_learn.acc.pop_BY_train.acc.pop.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="learn.acc.pop" & by =="train.acc.pop", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_gamma_BY_val.rank.rdm.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="gamma" & by =="val.rank.rdm", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_train.eps.val_BY_val.rank.rdm.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="train.eps.val" & by =="val.rank.rdm", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_final.theta.nodata_BY_S.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="final.theta.nodata" & by =="S", reg=reg))

save_aggregated(path=paste0(out.dir, "/MLE_SIM_THEORY_max.theta.S_BY_S.csv"),
                matched = findExperiments(algo.name="aggregate_by", 
                                          algo.pars= var=="max.theta.S" & by =="S", reg=reg))
