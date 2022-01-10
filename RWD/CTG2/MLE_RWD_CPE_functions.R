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
#   (2) functions
###

training_caret <- function(algo, data){
  tc <- caret::trainControl(method = "none", search = "grid")
  tg <- get_tg(algo, data, 1)
  
  modt <- caret::train(Y ~ ., data=data$train, method=algo,
                       trControl=tc, tuneGrid=tg)
  modl <- caret::train(Y ~ ., data=data$learn, method=algo,
                       trControl=tc, tuneGrid=tg)
  
  pred <- list()
  pred[["train"]] <- predict(modt, data$train)
  pred[["val"]] <- predict(modt, data$val)
  pred[["learn"]] <- predict(modl, data$learn)
  pred[["eval"]] <- predict(modl, data$eval)
  pred <- pred %>% lapply(as.character) %>% lapply(as.integer)
  
  info <- list()
  info$algo <- algo
  info$hp <- tg
  
  return(list(info=info, pred=pred))
}

get_tg <- function(algo, data, len=1){
  stopifnot(algo %in% c("glmnet", "rpartCost", "svmLinearWeights2", "xgbTree", "mlpSGD", "ranger"))
  if(algo == "glmnet"){
    tg <- data.frame(alpha = runif(len, min = 0, 1),
                     lambda = 2^runif(len, min = -10, 3))
  }
  if(algo == "rpartCost"){
    tg <- data.frame(cp = 10^runif(len, min = -8, max = -1),
                     Cost = runif(len, min = 1, max = 30)) 
  }
  if(algo == "svmLinearWeights2"){
    tg <- data.frame(cost = 2^runif(len, min = -10, max = 10),
                     Loss = sample(c("L1", "L2"), size = len, replace = TRUE),
                     weight = runif(len, min = 1, max = 25))
  }
  if(algo == "xgbTree"){
    tg <- data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                     max_depth = sample(1:10, replace = TRUE, size = len),
                     eta = runif(len, min = .001, max = .6),
                     gamma = runif(len, min = 0, max = 10),
                     colsample_bytree = runif(len, min = .3, max = .7),
                     min_child_weight = sample(0:20, size = len, replace = TRUE),
                     subsample = runif(len, min = .25, max = 1))
  }
  if(algo == "mlpSGD"){
    n <- nrow(data$train)
    tg <- data.frame(size = sample(2:20, replace = TRUE, size = len),
                     l2reg = 10^runif(len, min = -5, 1),
                     lambda = runif(len, max = .4),
                     learn_rate = runif(len),
                     momentum = runif(len, min = .5),
                     gamma = runif(len),
                     minibatchsz = floor(n*runif(len, min = .1)),
                     repeats = sample(1:10, replace = TRUE, size = len))
  }
  if(algo == "ranger"){
    srules <- c("gini", "extratrees") # classification variant
    x <- data$train[, -1]
    tg <- data.frame(
      min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
      mtry = sample(1:ncol(x), size = len, replace = TRUE),
      splitrule = sample(srules, size = len, replace = TRUE)
    )
  }
  tg
}

estimate_perf <- function(pred, labels){
  l <- list(acc=c(0,1), se=1, sp=0)
  sapply(l, function(x) sum(pred == labels & labels %in% x)/sum(labels %in% x))
}

norn <- function(x){
  rownames(x) <- NULL; x
}


do_inference <- function(sel){
  pred_eval <- sapply(predictions[sel], \(x) x$eval)
  colnames(pred_eval) <- info$model[sel]
  labels_eval <- labels$eval
  
  comp_eval <- SEPM::define_hypothesis("sensspec",
                                       threshold=threshold,
                                       alternative = "greater",
                                       alpha=0.025) %>% 
    SEPM::compare(labels=labels_eval, predictions=pred_eval) 
  
  est_eval <- SEPM::estimate(comparison = comp_eval, method = "beta.approx", lambda=list(2, 2))
  
  SEPM::infer(est_eval)
}

get_rej <- function(inf){
  inf$inference[1:2] %>%
    sapply(\(x) x$reject) %>%
    matrix(nrow = nrow(inf$inference[[1]])) %>%
    apply(1, all)
}