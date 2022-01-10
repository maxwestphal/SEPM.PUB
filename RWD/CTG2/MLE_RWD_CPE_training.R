### 
#   Project:
#   A multiple testing framework for diagnostic accuracy studies with co-primary endpoints
###
#   Task:
#   Real world data example: Predictive modelling with the Cardiotocography Data Set 
###
#   Author: 
#   Max Westphal (mwestphal@uni-bremen.de)
###
#   Date:
#   2021-08-31
###
#   References: 
#   https://archive.ics.uci.edu/ml/datasets/cardiotocography
###
#   File:
#   (4) training
###

# Dependencies --------------------------------------------------------------------------------
library(batchtools)
source("MLE_RWD_CPE_functions.R")
DATA <- readRDS("DATA.rds")


# Registry ------------------------------------------------------------------------------------
reg <- makeRegistry(file.dir = "train_reg", seed = 1)
reg <- loadRegistry("train_reg", writeable = TRUE)
reg$cluster.functions = makeClusterFunctionsSocket(26)
reg$packages <- c("plyr", "dplyr", "caret", "Matrix", "ranger",
                  "glmnet", "rpart", "LiblineaR", "xgboost")
reg$source <- "MLE_RWD_SESP_functions.R"

?batchMap
nrep <- 200
batchMap(fun = training_caret, 
         algo = rep(c("glmnet", "rpartCost", "svmLinearWeights2", "xgbTree", "ranger"), nrep),
         more.args = list(data=DATA))

tt <- testJob(5, reg=reg)
submitJobs(reg=reg)
getStatus(reg=reg)
#clearRegistry(reg=reg)
getErrorMessages()

R <- reduceResultsList(reg=reg)

saveRDS(R, "RESULTS.rds")



