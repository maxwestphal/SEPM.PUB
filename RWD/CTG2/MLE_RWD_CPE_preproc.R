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
#   (3) preproc
###

# Dependencies --------------------------------------------------------------------------------
# Clean environment:
# rm(list=ls())

library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(data.table)
library(splitstackshape)


# Data ----------------------------------------------------------------------------------------


# Make analysis reproducible:
set.seed(1337)

data <- readr::read_csv2("DATA/CTG_raw.csv") 
data <- data[- which(rowSums(is.na(data))>0), ] 
# -> remove 4 incomplete observations, everything else is complete
data <- data %>% 
  mutate(Tendency = factor(Tendency),
         CLASS = factor(CLASS),
         NSP = factor(NSP)) %>%
  mutate(Date = (as.Date(Date, format="%d.%m.%Y"))) %>%
  select(-c(FileName, SegFile, b, e)) %>%
  select(-c(A, B, C, D, E, AD, DE, LD, FS, SUSP)) %>%
  arrange(Date) %>%
  select(-Date) %>% 
  as.data.frame()

# EDA on endpoints
table(data$CLASS) # -> Class code (1 to 10) for classes A to SUSP
table(data$NSP)   # -> Normal=1; Suspect=2; Pathologic=3
table(data$CLASS, data$NSP)
colSums(is.na(data))


# Goal: prediction of NSP != 1, i.e. suspect or pathologic (without CLASS AS PREDICTOR)
dat <- data %>% 
  mutate(Y = as.factor(as.integer(NSP != 1))) %>%
  select(-c(CLASS, NSP))
dat %>% setcolorder(c("Y", names(dat)[-ncol(dat)])) 


# Data splitting ------------------------------------------------------------------------------
# dat is sorted by date, i.e. newest obs come last, i.e. those will form 

rt <- 0.60  # training percentage
rv <- 0.15  # validation percentage
rl <- rt+rv # learning percentage
re <- 1-rl  # test percentage

# evaluation indices
il <- 1:round(rl*nrow(dat))

# split learning data (stratified by outcome)
set.seed(1337)
DATA <- stratified(dat[il, ], c("Y"), rt/rl, bothSets = TRUE) %>% lapply(as.data.frame)
names(DATA) <- c("train", "val")

DATA$learn <- dat[il, ]
DATA$eval = dat[-il, ]

# check sample sizes
sapply(DATA, dim)

# check class balance
sapply(DATA, function(d){table(d$Y)/nrow(d)})
sapply(DATA, function(d){table(d$Y)})

saveRDS(DATA, file="DATA.rds")



