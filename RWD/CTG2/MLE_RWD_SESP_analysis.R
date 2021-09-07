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
#   (5) analysis
###


# Prep ----------------------------------------------------------------------------------------
library(dplyr)

DATA <- readRDS("DATA.rds")
R <- readRDS("RESULTS.rds")

source("MLE_RWD_SESP_functions.R")


# Data ----------------------------------------------------------------------------------------
labels <- lapply(DATA, function(x) as.integer(as.character(x$Y)))
predictions <- lapply(R, function(x) x$pred)
algo <- sapply(R, function(x) x$info$algo)
hp <- sapply(R, function(x) x$info$hp) %>% 
  lapply(function(x) sapply(1:ncol(x), function(y){
    paste0(substr(names(x)[y], 1, 5),"=", ifelse(is.numeric(x[1, y]), round(x[1, y], 2), x[1, y]) )
  })) %>%
  sapply(function(x) do.call(paste, c(as.list(x), list(sep="_")))) 

info <- data.frame(model = paste0("model_", 1:length(algo)), algo=algo, hp=hp)
head(info)


# Performance measures ------------------------------------------------------------------------
ntry <- 1000; se0 <- 0.8; sp0 <- 0.7
threshold <- c(se0, sp0);

perf <- lapply(names(labels), function(ds){
  lapply(predictions, function(p){
    estimate_perf(p[[ds]], labels[[ds]])
  }) %>% dplyr::bind_rows()
}) %>% 
  lapply(function(x) {
    cbind(model = info$model, 
          model_id = as.integer(substr(info$model, 7, 10)),
          algo = info$algo, x) %>% 
      dplyr::mutate(tau = pmin(se-se0, sp-sp0))
  })
names(perf) <- names(labels)
perf


pa <- lapply(perf, function(p) {
  p %>% 
    filter(model_id <= ntry) %>% 
    dplyr::group_by(algo) %>% 
    dplyr::summarize(tau_mean = mean(tau),
                     tau_max = max(tau))
})
pa

plot(perf$val$se[1:ntry], perf$val$sp[1:ntry], col=factor(perf$val$algo))

perf$val %>% 
  filter(se > 0.8, sp > 0.7)

perf$val %>% 
  filter(se > 0.9, sp > 0.9) %>% 
  group_by(algo) %>% 
  summarize(n=n())

sapply(DATA, nrow)
sapply(DATA, \(x) sum(x$Y=="1"))
377/1594
94/532

# Model selection ----------------------------------------------------------------------------------
require(SEPM)

ntry; threshold
pred_val <- sapply(predictions[1:ntry], \(x) x$val)
colnames(pred_val) <- info$model[1:ntry]
labels_val <- labels$val

lapply(labels, length )
lapply(labels, sum)

comp_val <- SEPM::define_hypothesis("accuracy.cp", threshold=threshold) %>% 
  SEPM::compare(labels=labels_val, predictions=pred_val) 

## model selection according to different selection rules (same as in simulation study)
set.seed(1337)
sel_best <- SEPM::select(comp_val,
                         method = "best",
                         mode="weighted",
                         threshold=threshold)

sel_close <- SEPM::select(comp_val,
                          method="close",
                          k=1,
                          mode="weighted",
                          max_models = round(sqrt(500)), 
                          threshold=threshold)

sel_opt <- SEPM::select(comp_val, 
                        method="optimal",
                        method_order = "param",
                        target_order = "prob",
                        threshold = threshold,
                        n_eval = 500, 
                        prev_eval = 0.25,
                        method_ext = "basic",
                        method_pred = "mbeta_approx",
                        batch_size = 10,
                        max_iter = 50,
                        max_models = round(sqrt(500)), 
                        target_tol = 1e-4,
                        steady_plot = TRUE, 
                        info_plot=TRUE, 
                        ylim_plot=NULL) 

list(sel_best, sel_close, sel_opt)

## which models have been selected?
info[sel_best, ]
info[sel_close, ]
info[sel_opt, ]

perf %>% lapply(\(x) x[588, ])
perf %>% lapply(\(x) x[sel_close, ])
perf %>% lapply(\(x) x[sel_opt, ])

# Model evaluation ----------------------------------------------------------------------------
## conduct inference?
inf_best <- do_inference(sel_best)
inf_close <- do_inference(sel_close)
inf_opt <- do_inference(sel_opt)

## for which models can the null hypothesis be rejected?
get_rej(inf_best)
get_rej(inf_close)
get_rej(inf_opt)

## display result from positively evaluated models:
lapply(inf_best$inference[1:2], \(x) x[get_rej(inf_best), ])
lapply(inf_close$inference[1:2], \(x) x[get_rej(inf_close), ])
lapply(inf_opt$inference[1:2], \(x) x[get_rej(inf_opt), ])

## results from all evaluated models:
summary(inf_best) %>% sapply(function(x) x$lower)
summary(inf_close) %>% sapply(function(x) x$lower)
summary(inf_opt) %>% sapply(function(x) x$lower)


