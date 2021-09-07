


### SIMULATION STUDY: MACHINE LEARNING AND EVALUATION 
#   MODEL EVALUATION (MLE_SIM_SESP_FULL)

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

sim <- "MLE_SIM_SESP"             # simulation name
Nsim <- 1                                 # number of simulations
cr <- 0.8                               # fraction of cores used
req.packages <- c("SIMPle", "SEPM", "SEPM.MLE")     # required packages
main.dir <- "E:\\MLE_SIM"              # main folder
reg.dir <- paste(main.dir, sim, sep="\\")  # registry folder
out.dir <- paste0(main.dir, "\\RESULTS")   # results folder
out.path <- paste(out.dir, paste0(sim, ".csv"), sep="\\") # results path

dir.create(main.dir)
dir.create(out.dir)

### PARAMETER CONFIGURATION (+++ MODIFY IF NEEDED +++)
#load.dir <- paste0(main.dir, "\\DATA")     # save.dir of MLE_SIM_DATA
load.dir <- "F:\\DataScience\\R\\SIM\\MLE_SIM\\DATA"

JP.data <- readr::read_csv("F:\\DataScience\\R\\SIM\\MLE_SIM\\RESULTS\\MLE_SIM_DATA_JP.csv")
ids <- JP.data$job.id

ids <- JP.data  %>% {.$job.id}
# sample random ids used for method comparison
#id0 <- readr::read_csv("E:\\MLE_SIM\\RESULTS\\MLE_SIM_SESP_COMP1.csv")$load.id %>% unique()
#id1 <- setdiff(ids, id0)
#saveRDS(id1, "id1.rds")
id1 <- readRDS("id1.rds")
set.seed(1337)
id11 <- base::sample(id1, 20000, replace=F)
id111 <- base::sample(id11, 50, replace=F)

prob <- list()
prob[["study_sesp"]] <-
  CJ(load.folder = load.dir,
     load.base = "job",
     load.id = ids,
     methods = NA,
     M = 200,
     M.start = NA,
     M.probs = "uniform",
     M.seed = 1,
     eval.first = 1,
     eval.n = c(400, 800), # 800 # 100, 200
     eval.rdm = FALSE,
     estimate.method = "beta.approx",
     estimate.args = "",
     select.method = c("best", "close", "oracle.sesp", "optimal"),
     select.args = c("", "mode=weighted", "mode=min"),
     select.limit = "sqrt",
     sesp.delta = 0,
     known.delta = FALSE,
     infer.method = "maxT",
     infer.args = "lfc=d",
     alternative = "greater",
     alpha = 0.025,
     transform = "none") 

prob[[1]] <- prob[[1]] %>%
  mutate(eval.first = 1 + 100*(eval.n==200) + 300*(eval.n==400) + 700*(eval.n==800)
         + 1500*(eval.n==8000) + 9500*(eval.n==50)) %>% 
  filter(! ((select.method %in% c("best", "close")) & !(select.args %in% c("mode=weighted", "mode=min"))) ) %>% 
  filter(! ((select.method %in% c("oracle.sesp", "optimal")) & !(select.args %in% c(""))) ) %>% 
  filter(load.id %in% id1) # TODO

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
addProblem(name="study_sesp", data=NULL, fun=SEPM.MLE::study_sesp, reg=reg)
addAlgorithm(name="id", fun=NULL, reg=reg)

#   Add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=Nsim,
               reg=reg)
# summarizeExperiments(reg=reg)

### TEST 
j = 1337

jj_nd <- findNotDone(reg=reg)
id_nd <- getJobPars(jj_nd) %>% unwrap() %>% {.$load.id}
length(unique(id_nd))

JP_nd <- getJobPars(jj_nd) %>% unwrap() 
JP_nd$eval.n %>% table()


jj1 <- findExperiments(prob.pars = load.id %in% id11)
findNotDone(jj1)

jj <- findExperiments(prob.pars = select.args == "mode=min")
findNotDone(jj)
j <- jj[3300]
testJob(j, reg=reg)
unwrap(getJobPars(j, reg=reg))

### RUN JOBS
Sys.time()
submitJobs(findNotDone(), reg=reg) 
Sys.time()

### OPTIONAL STEPS
Sys.sleep(900)
getStatus(reg=reg)

#findExpired(reg=reg)
#removeExperiments(findErrors(reg=reg), reg=reg)
#waitForJobs(reg=reg)
#getErrorMessages()
#killJobs(reg=reg)
#clearRegistry(reg=reg)

#getJobPars( findExperiments(prob.pars = select.method=="close") ) %>% unwrap()
#removeExperiments(findExperiments(prob.pars = select.method=="close"))

### SAVE_RESULTS:

SEPM.MLE::save_results(reg, JP.data, target=out.path)



# TODO ----------------------------------------------------------------------------------------

require(dplyr)
#path <- 'E:\\MLE_SIM\\RESULTS\\MLE_SIM_SESP_2.csv'
D <- readr::read_csv(out.path)


delta <- 0.10
D %>% mutate(final.theta = pmin(final.learn.se, final.learn.sp),
             opt.theta = pmin(opt.se.cpe, opt.sp.cpe),
             final.theta.lower = pmin(final.se.lower, final.sp.lower),
             select.rule = interaction(select.method, select.args)) %>%
  group_by(n.learn, eval.n, select.method, select.args) %>%
  summarize(FP = mean(final.theta > opt.theta - delta),
            RR = mean(final.theta.lower > opt.theta - delta),
            meanS = mean(S),
            nsim = n()) %>%
  #filter(select.method=="best")
  #filter(select.method=="close")
  filter(! (select.method %in% c("best", "close") & select.args == "mode=weighted"))
  #View()
  

