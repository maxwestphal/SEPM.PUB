---
title:  |
  | Simulation Study: 
  | Machine Learning and Evaluation 
  | (MLE_SIM_ACC)
author: 
- Max Westphal <br> mwestphal@uni-bremen.de <br> https://github.com/maxwestphal <br> https://www.linkedin.com/in/maxwestphal/ <br> https://www.anstat.uni-bremen.de/node/22
date: "11 June, 2019"
knit: (function(inputFile, encoding) { 
      out.dir <- file.path(dirname(dirname(dirname(inputFile))), 'docs');
      dir.create(out.dir);
      rmarkdown::render(inputFile,
                        encoding=encoding, 
                        output_file=file.path(out.dir, 'MLE_SIM_ACC.html')) })
output: 
  rmdformats::readthedown:
    toc_depth: 3
    toc_float: true
    number_sections: true
    df_print: paged
    code_folding: hide
    css: styles.css
    keep_md: yes
---

<!-- usepackage_latex() -->

<!-- Operators - Start -->
\newcommand{\pr}{\mathbb{P}}
\newcommand{\E}{\mathbb{E}}
\newcommand{\ND}{\bar{D}}
\newcommand{\boldX}{\mathbf{X_{|1}}}
\newcommand{\boldXnot}{\mathbf{X_{|0}}}
\newcommand{\Xnoi}{X_{|0}}
\newcommand{\X}{X_{|1}}
\newcommand{\hatf}{\hat{f}}
\newcommand{\hatY}{\hat{Y}}
\newcommand{\one}{\mathds{1}}
\newcommand{\setM}{\mathcal{M}}
\newcommand{\setS}{\mathcal{S}}
\newcommand{\setT}{\mathcal{T}}
\newcommand{\setL}{\mathcal{L}}
\newcommand{\setE}{\mathcal{E}}
\newcommand{\setD}{\mathcal{D}}
\newcommand{\setR}{\mathcal{R}}
\newcommand{\setN}{\mathcal{N}}
\newcommand{\RD}{R_D}
\newcommand{\RND}{R_{\ND}}
\newcommand{\za}{z_{1-\alpha}}
\newcommand{\zM}{z_{1-1/2M}}
\newcommand{\asim}{\stackrel{\cdot}{\sim}}
\newcommand{\betahat}{\hat{\beta}}
\newcommand{\thetahat}{\hat{\vartheta}}
\newcommand{\thetatilde}{\tilde{\vartheta}}
\newcommand{\thetan}{\vartheta_0}
\newcommand{\setP}{\mathcal{P}}
\newcommand{\setB}{\mathcal{B}}
\newcommand{\setF}{\mathcal{F}}
\newcommand{\setH}{\mathcal{H}}
\newcommand{\setV}{\mathcal{V}}
\newcommand{\haty}{\hat{y}}
\newcommand{\hatbeta}{\hat{\beta}}
\newcommand{\niton}{\not\owns}
\newcommand{\valest}{ (\thetahat_m(\setV))_{m \in \setM}}
\newcommand{\D}{\mathfrak{D}}
\newcommand{\eps}{\epsilon}

\DeclareMathOperator{\argmax}{argmax}
\DeclareMathOperator{\err}{err}
\DeclareMathOperator{\Err}{Err}
\DeclareMathOperator{\var}{var}
\DeclareMathOperator{\cov}{cov}
\DeclareMathOperator{\Var}{Var}
\DeclareMathOperator{\Cov}{Cov}
\DeclareMathOperator{\corr}{corr}
\DeclareMathOperator{\ROC}{ROC}
\DeclareMathOperator{\AUC}{AUC}
\DeclareMathOperator{\wAUC}{wAUC}
\DeclareMathOperator{\pAUC}{pAUC}
\DeclareMathOperator{\TPF}{TPF}
\DeclareMathOperator{\FPF}{FPF}
\DeclareMathOperator{\FWER}{FWER}
\DeclareMathOperator{\power}{power}
\DeclareMathOperator{\Power}{Power}
\DeclareMathOperator{\CP}{CP}
\DeclareMathOperator{\SE}{SE}
\DeclareMathOperator{\logit}{logit}
\DeclareMathOperator{\diag}{diag}
\DeclareMathOperator{\expo}{exp}
\DeclareMathOperator{\Abb}{Abb}
\DeclareMathOperator{\argmin}{argmin}
\DeclareMathOperator{\acc}{acc}
\DeclareMathOperator{\se}{se}
\DeclareMathOperator{\CI}{CI}
\DeclareMathOperator{\Bin}{Bin}
\DeclareMathOperator{\KL}{KL}
\DeclareMathOperator{\IQR}{IQR}

---

# Introduction 
Prediction models are everywhere. They predict if an email contains spam, forecast tommorrow's stock prices or diagnose if a patient is diseased. Such models take some feature data as input and return a prediction of the target variable. Features could be specific word frequencies, recent market movements or X-ray images. More often than not, machine-learning (ML) algorithms are used to train such models from data. An ML algorithm takes some data, a collection of feature-label pairs, as input and outputs a prediction model. Once a model has been trained it can be employed into practice. Theoretically...

Of course, things are rarely that simple. In practice, it is never known beforehand which algorithm and hyperparameter choices lead to the model with the highest performance. That is why in practice usually many different approaches are (rightly) tried out and compared. Unfortunately, if many such comparisons are conducted it is hard to distinguish between truly good models and a model that only empirically appears to perform well enough. This is in particular true, when the amount of available data is limited. Thus, for serious applications, a rigorous evaluation before implementation in practice becomes very important. After all, while there are certainly benefits when implementing a good model there are also costs when implementing a bad one.

This document contains the results of an extensive simulation study regarding the selection and evaluation of machine learned prediction models described by **Westphal & Brannath (2019, ICML)**. The main purpose of this document is to act as a supplementary report of results. It shows several additional analyses which could not be included in the recent publication. Moreover, this report may also serve as a starting point for non-technical readers as complex mathematical notation and theoretical details are avoided whenever possible. For more detailed information regarding the employed methods we refer the reader to **Westphal & Brannath (2019, ICML)** and our previous work **Westphal & Brannath (2019, SMMR)**. 

Any question or suggestion regarding this report and our underlying research is very welcomed - feel free to contact me. While the above mentioned publications are joint work with Werner Brannath, I compiled this report on my own and hence I am solely responsible for any potential error. An overview over other projects in the context of model evaluation can be found at https://maxwestphal.github.io/SEPM.PUB/.

## Background

The usual recommendation in applied machine learning is to select a prediction model based on the empirical performance on a hold-out validation set to avoid overfitting to the training data. When computationally feasible, the results of different data splits (learning = training + validation) can be averaged. Such cross-validation and bootstrapping approaches are however not investigated in this study - instead we focus on simple hold-out validation. Once a final model is selected, its performance should be evaluated on a seperate test dataset, as illustrated in the following diagram:


```r
knitr::include_graphics('figures/ML_flowchart_FINAL_SINGLE_tagged.png')
```

<img src="figures/ML_flowchart_FINAL_SINGLE_tagged.png" width="9993" />

The main goal of this simulation study is to compare this *default* model selection approach with other strategies. In particular, we highlight differences to selecting multiple promising models (based on the validation data) for a simultaneous evaluation on the test data. This novel approach to assess several models at once allows to utilise the test data for model selection. In contrast, model selection and evaluation are usually stricly seperated to avoid selection-induced bias. Our approach involves countering this overoptimism with a particular simultaneous test procedure, the so-called maxT-approach, which is based on approximate joint distribution of performance estimates. This expanded machine-learning and evaluation pipeline is shown below.


```r
knitr::include_graphics('figures/ML_flowchart_FINAL_MULTIPLE_tagged.png')
```

<img src="figures/ML_flowchart_FINAL_MULTIPLE_tagged.png" width="9995" />

## Setup

In the following, some preliminary steps for for data handling are carried out. In theory, showing these steps makes the whole analysis reproducible. To this end, all simulations need however to be conducted locally first which is very time consuming. For us, this process took roughly 3 months with a 16 core CPU (AMD Ryzen Threadripper 1950X). If you are just interested in the results of the study, feel free to skip this section. 


```r
## load packages:
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(knitr)
library(rmdformats)
library(DT)
library(SEPM.MLE) 
```

After loading all neccessary packages (assuming they are installed already), a few global options are set which may alter the appearance of this document.


```r
## set global options:
config <- list()

## simulation results path (NEEDS TO BE REPLACED!!!):
config$path <- 'E:/DataScience/R/SIM/MLE_SIM/RESULTS'

## enable/disable caching
config$load.cache <- TRUE
config$data.cache <- TRUE
config$figure.cache <- TRUE

## render plots:
config$plot <- TRUE

## plotting options:
config$fig.dpi <- 240
config$fig.size <- 12
config$title.size <- 24
config$text.size <- 18
config$big.cex <- 5
config$small.cex <- 2

## subset data for faster compilation, set to NA for full data analysis:
config$subset.size <- NA

## specify methods to include in certain analysis (must be contained in the data):
config$bias.methods <- c("within1SE", "default")
```

Finally, a few custom objects and functions are defined.




```r
## default ggplot theme:
owntheme <- function(x=NULL, pars=config){
  theme(axis.text    = element_text(size=pars$text.size),
        axis.title   = element_text(size=pars$title.size,face="bold"),
        legend.text  = element_text(size=pars$title.size),
        legend.title = element_text(size=pars$title.size, face="bold"),
        strip.text.x = element_text(size=pars$title.size, face="bold"),
        strip.text.y = element_text(size=pars$title.size, face="bold", angle=90),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'),
        legend.background = element_rect(fill="white", size=0.75, linetype="solid", colour="black"))}

## read in simulation results
read_data <- function(sim, path=config$path){
  bind_rows(lapply(sim, function(x) read_csv(paste0(path, "/", x, ".csv"), col_types = cols())))
}

## data preprocessing
preproc_data <- function(data, size=config$subset.size, seed=1337,
                         paired=TRUE, test=TRUE){
  if(inherits(data, "list")){return(data)}
  if(!is.na(size)){
    set.seed(seed)
    data <- filter(data, load.id %in% sample(unique(data.in$load.id), size, replace=FALSE))
  }
  out <- list()
  # 1st slot: raw data
  out$raw <- data %>%
    mutate(select.args = ifelse(is.na(select.args), "", select.args)) %>%
    mutate(select.rule = as.character(interaction(select.method, select.args, select.limit))) %>%
    mutate(select.rule = recode(select.rule, 
                                oracle.learn.sqrt = "oracle[learn]",
                                oracle.train.sqrt = "oracle[train]",
                                `se.c=1.sqrt`     = "within1SE",
                                `rank.r=1.sqrt`   = "default",
                                `simplest.en..sqrt` = "simple",
                                `rank.r=100.none` = "S=100")) %>%
    mutate(select.rule = ordered(select.rule, 
                                 levels = info$select.rule)) %>%
    mutate(score = recode(scenario,
                          EOMPM_A2 = "A",
                          EOMPM_B2 = "B",
                          MLE_SIM_F1_prev30 = "C",
                          MLE_SIM_F1_prev15 = "D",
                          MLE_SIM_F13_prev30 = "E",
                          MLE_SIM_F13_prev15 = "F")) %>%
    mutate(redundancy = recode(red,
                               `0` = "[I]",
                               `1` = "[R]")) %>%
    mutate(scenario = paste0(score, redundancy)) %>%
    mutate(n.eval = eval.n) %>%
    mutate(abs.dev = final.mod.thetahat-final.mod.learn.theta,
           rel.dev = abs.dev/final.mod.learn.theta,
           abs.dev.c = final.mod.thetatilde-final.mod.learn.theta,
           rel.dev.c = abs.dev.c/final.mod.learn.theta,
           maxS.sqrt = S == round(sqrt(n.eval))) %>%
    unite(eval.strat, select.rule, infer.method, sep="|", remove=FALSE) 
  

  # 2nd slot for paired analysis
  if(paired){
    out$paired <- out$raw %>%
    dplyr::select(load.id, M.start, M, n.eval, n.learn, scenario, methods,
                  select.rule, infer.method, final.mod.learn.theta, opt.theta) %>%
    dplyr::filter(select.rule %in% c("within1SE", "default")) %>%
    tidyr::spread(select.rule, final.mod.learn.theta)
  }
  
  #3rd slot for analysis of rejection rate
  if(test){
    out$test <- out$raw %>%
      preproc_shift() %>%
      aggr_results()
  }

  return(out)
}

## read in theory data:
read_theory <- function(sim, path=config$path, at=18:67, vars=5:54){
  data.in <- bind_rows(lapply(sim, function(x) read_csv(paste0(path, "/", x, ".csv"), col_types = cols())))
  data.in %>%
    group_by(M, n.learn, eval.n, scenario) %>%
    summarise_at(at, function(x) mean(x, na.rm=T)) %>%
    gather("by", "var", vars) %>%
    mutate(x = unname(sapply(lapply(strsplit(sapply(by, function(x)substr(x, 2, nchar(x)-1)), ","), as.numeric), mean)))
}
```

---

## Data preparation 

### Loading 

To reproduce the results shown in this report, several numerical experiments have to be conducted first. It is assumed that this has been done such that the resulting CSV files can be loaded. 


```r
## read in data:
data.in <- read_data(c("MLE_SIM_ACC_FULL", "MLE_SIM_ACC_M100_UNI", "MLE_SIM_ACC_M40_UNI"))
```



### Preprocessing

Next we perform several preproccessing steps via the custom function `preproc_data`, defined above. To reduce the computational demand of compiling this Rmd, only a subset of the data may be analysed. To do so, the variable `config$subset.size` needs to be set accordingly. 


```r
data <- preproc_data(data.in)
```


```r
active <- which(info$select.rule %in% unique(data$raw$select.rule))
str(data, 1)
```

```
## List of 3
##  $ raw   :Classes 'tbl_df', 'tbl' and 'data.frame':	7200000 obs. of  55 variables:
##  $ paired:Classes 'tbl_df', 'tbl' and 'data.frame':	1800000 obs. of  11 variables:
##  $ test  :Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	1920 obs. of  9 variables:
##   ..- attr(*, "vars")= chr [1:5] "shift" "select.rule" "infer.method" "M" ...
##   ..- attr(*, "drop")= logi TRUE
```


The following main analysis is based on 72000 instances of the complete machine learning and evaluation pipeline.

---

# Preliminaries

## Machine Learning 

Every instance of the simulation study consists of a triple $(\setT, \setV, \setE)$ of training, validation and evaluation (test) data. All candidate algorithms $A_m, m\in \setM = \{1,\ldots,M\},$ are applied to the training data. A selection rule is used to select one or more models deemed suitable for evaluation. This selection $\setS \subset \setM$ is based on the validation data. The selected algorithms $A_m, m\in \setS,$ are than applied again to training and validation data - refered to as learning data $\setL = \setT \cup \setV$. In our simulations, we consider the following four learning algorithms implemented in the [caret](https://github.com/topepo/caret/) package:

- **EN**: Elastic Net: Penalized Logistic Regression [(glmnet)](https://github.com/topepo/caret/blob/master/models/files/glmnet.R)
- **CART**: Cost-Sensitive Classification and Regression Trees [(rpartCost)](https://github.com/topepo/caret/blob/master/models/files/rpartCost.R)
- **SVM**: L2 Regularized Linear Support Vector Machines with Class Weights [(svmlinearweights)](https://github.com/topepo/caret/blob/master/models/files/svmLinearWeights2.R)
- **XGB**: eXtreme Gradient Boosting [(xgbTree)](https://github.com/topepo/caret/blob/master/models/files/xgbTree.R)

If not specified otherwise, an equal number of algorithms from each class with different (randomly sampled) hyperparameters is applied per training dataset, i.e. $M/4$ models of of each type. In our main analysis we investigate the cases of $M \in \{40, 100, 200\}$ candidate models. 

## Prediction tasks

We consider binary classifcation tasks and classification accuracy (proportion of correct predictions) as our performance measure of interest. The following table gives an overview over the considered learning tasks. In terms of important characteristics. The optimal performance $\vartheta_{opt}$ is defined as the performance of the classifier which results from thresholding the true (data-generating) risk-score $\pr(Y=1|\mathbf X)$ at $0.5$. The four rightmost columns show the mean of $\vartheta_{max}=\max_{m\in \setM}\vartheta_m$ over all simulation instances for different combinations of learning samples size $n_\setL \in {400, 800}$ and independent [I] or redundant [R] features. $\vartheta_{max}$ can be seen as a (rough) measure how well the candidate algorithms are able to learn the true relationship between features $\mathbf X$ and target $Y \in \{0,1\}$. In all simulations a 3:1 ratio between training and validatio is used, e.g. $n_\setL = 400 = 300+100 = n_\setT + n_\setV$. 


```r
## display tasks:
left_join(tasks,
          data$raw %>%
            group_by(score, redundancy, M, n.learn) %>%
            summarize(theta.max = mean(opt.theta)) %>%
            unite(case, n.learn, redundancy) %>%
            spread(case, theta.max),
          by="score") %>%
  rename("theta.max[400,I]"="400_[I]",
         "theta.max[400,R]"="400_[R]",
         "theta.max[800,I]"="800_[I]",
         "theta.max[800,R]"="800_[R]") %>%
  select(c(1:5, 7, 6, 8:11)) %>%
  arrange(-M, score) %>%
  mutate_at(vars(-(1:5)), function(x)round(x, 3)) %>%
  datatable(class=c("compact"), rownames = FALSE, filter="top", extensions = 'FixedHeader',
            options = list(pageLength = 6, fixedHeader = FALSE)) %>%
  formatStyle(columns = 1:12, color="black") %>%
  formatStyle(columns = c(1:2, 6, 8:11), fontWeight = "bold") %>%
    formatStyle(
    c(9, 11),
    background = styleColorBar(c(0.75, 1), '#8d99ae'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) %>% 
  formatStyle(
    c(8, 10),
    background = styleColorBar(c(0.75, 1), '#f05028'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) 
```

<!--html_preserve--><div id="htmlwidget-4432f320a89582c5250e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-4432f320a89582c5250e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;linear&quot;,&quot;nonlinear&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"5\" data-max=\"9\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.15\" data-max=\"0.5\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"40\" data-max=\"200\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.86\" data-max=\"0.966\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.847\" data-max=\"0.892\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.845\" data-max=\"0.89\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.854\" data-max=\"0.901\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.853\" data-max=\"0.9\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["FixedHeader"],"data":[["A","B","C","D","E","F","A","B","C","D","E","F","A","B","C","D","E","F"],["linear","linear","nonlinear","nonlinear","nonlinear","nonlinear","linear","linear","nonlinear","nonlinear","nonlinear","nonlinear","linear","linear","nonlinear","nonlinear","nonlinear","nonlinear"],[50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50],[5,5,5,5,9,9,5,5,5,5,9,9,5,5,5,5,9,9],[0.5,0.5,0.3,0.15,0.3,0.15,0.5,0.5,0.3,0.15,0.3,0.15,0.5,0.5,0.3,0.15,0.3,0.15],[200,200,200,200,200,200,100,100,100,100,100,100,40,40,40,40,40,40],[0.885,0.86,0.951,0.966,0.95,0.963,0.885,0.86,0.951,0.966,0.95,0.963,0.885,0.86,0.951,0.966,0.95,0.963],[0.878,0.85,0.855,0.892,0.854,0.891,0.877,0.849,0.853,0.89,0.852,0.889,0.875,0.847,0.848,0.887,0.847,0.886],[0.877,0.85,0.853,0.89,0.852,0.89,0.876,0.849,0.851,0.888,0.849,0.889,0.873,0.846,0.846,0.885,0.845,0.886],[0.882,0.856,0.867,0.901,0.865,0.9,0.882,0.855,0.866,0.9,0.864,0.899,0.881,0.854,0.863,0.898,0.861,0.897],[0.881,0.855,0.865,0.899,0.864,0.9,0.881,0.855,0.864,0.898,0.862,0.899,0.879,0.853,0.861,0.896,0.86,0.897]],"container":"<table class=\"compact\">\n  <thead>\n    <tr>\n      <th>score<\/th>\n      <th>score.type<\/th>\n      <th>features.tot<\/th>\n      <th>features.act<\/th>\n      <th>prev<\/th>\n      <th>M<\/th>\n      <th>theta.opt<\/th>\n      <th>theta.max[400,I]<\/th>\n      <th>theta.max[400,R]<\/th>\n      <th>theta.max[800,I]<\/th>\n      <th>theta.max[800,R]<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"fixedHeader":false,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10]}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100],"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':'black'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'color':'black'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'color':'black'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'color':'black'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'color':'black'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'color':'black'});\nvar value=data[11]; $(this.api().cell(row, 11).node()).css({'color':'black'});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'font-weight':'bold'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'font-weight':'bold'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'font-weight':'bold'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'font-weight':'bold'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'font-weight':'bold'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.750000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/0.250000 * 100 + '%, #8d99ae ' + (1.000000 - value)/0.250000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.750000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/0.250000 * 100 + '%, #8d99ae ' + (1.000000 - value)/0.250000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.750000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/0.250000 * 100 + '%, #f05028 ' + (1.000000 - value)/0.250000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.750000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/0.250000 * 100 + '%, #f05028 ' + (1.000000 - value)/0.250000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


## Selection rules

The following selection rules will be investigated in this analysis. The color coding is fixed for the entire report. All *orcale* rules cannot be employed in practice as they are based on population quantities but serve as additional comparators in this simulation study. The median (IQR) number of models within one standard error of the best validation model is 9 (11) for $n_\setL=400$ and 8 (10) for $n_\setL=800$, given $M=200$ initial candidate models.


```r
## display selection rules:
info[active, ] %>% 
  dplyr::select(1:2) %>%
  datatable(rownames=FALSE, colnames = c("selection rule", "description")) %>%
  formatStyle('select.rule', fontWeight="bold", #target = 'row',
              color = styleEqual(info$select.rule[active], info$col[active])) %>%
  formatStyle(columns = 2, color="black")
```

<!--html_preserve--><div id="htmlwidget-7791c34c4324ab7937bf" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7791c34c4324ab7937bf">{"x":{"filter":"none","data":[["oracle[learn]","oracle[train]","default","within1SE"],["truly best model","truly best model (before re-training)","empirically best validation model","all models within 1 standard error of best validation model [S.max=sqrt(n.eval)]"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>selection rule<\/th>\n      <th>description<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','color':value == 'oracle[learn]' ? '#F8766D' : value == 'oracle[train]' ? '#E76BF3' : value == 'default' ? '#E08B00' : value == 'within1SE' ? '#00B81F' : ''});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

## Model evaluation
Finally, model performances of the selected models $\hatf_m = A_m(\setL), m \in \setS \subset \setM,$ are assessed based on the evaluation data. If multiple models are evaluated, the final model $\hatf_{m^*}$ is defined as the model with the highest empirical test performance. In addition we are interested in a formal test decision for the null hypotheses $H_0: \vartheta_m \leq \vartheta_0$ where $\vartheta_0$ is a given comparator (performance threshold or benchmark model). That is to say the goal of the evaluation study is to provide evidence that at least one model (the final model) has a sufficiently high performance.


# Results

## Model performance

### Overall analysis

The first figure shows the final model performance $\vartheta_{m^*}$ of the employed selection rules minus the best model performance $\vartheta_{max} = \max_{m\in \setM} \vartheta_m$ among all candidate models. The results are stratified by the number of learning samples $n_\setL$ and the number of evaluation samples $n_\setE$. Remember, that we use a $n_\setT:n_\setV= 3:1$ ratio to split the learning data into training and validation data, i.e. $n_\setV=1/4 \cdot n_\setL$. The red symbol ontop of the boxplots indicates the sample average of the corresponding observations. The first plot is restricted to the case of $M=200$ initial candidate models.


```r
data$raw %>%
  filter(M==200) %>%
  ggplot(aes(select.rule, (final.mod.learn.theta-opt.theta), colour=select.rule)) +
  geom_hline(yintercept=0, cex=1, col="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(y = bquote(bold("Loss in final model performance: ")*vartheta[m*"*"]-vartheta[max]),
       x = bquote(bold("Selection rule")))+ 
  scale_color_manual(values = info$col[active], name="Selection rule:  ",
                     labels = paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%   
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank()) 
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/finaltheta-1.png)<!-- -->

In the next plot shows the same loss as above but now relative to $\vartheta_{max}$.


```r
data$raw %>%
  filter(M==200) %>%
  ggplot(aes(select.rule, (final.mod.learn.theta-opt.theta)/opt.theta, colour=select.rule)) +
  geom_hline(yintercept=0, cex=1, color="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(y = bquote(bold("Relative loss in performance: ")*(vartheta[m*"*"]-vartheta[max])/vartheta[max]),
       x = bquote(bold("Selection rule"))) + 
  scale_color_manual(values = info$col[active], name="Selection rule:  ",
                     labels = paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%   
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank()) 
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/finaltheta_alt-1.png)<!-- -->




As we are most interested in comparing the *within 1 SE* selection rule with the *default* selection rule, the following figure shows exactly this comparison.


```r
data$paired %>%
  ggplot(aes(as.factor(n.eval), (within1SE-default) ), colour="darkgrey") +
  geom_hline(yintercept = 0, cex=1.1, color="blue") +
  geom_boxplot(cex=1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ M,
             labeller = label_bquote(rows = bold(n[L]==.(n.learn)),
                                     cols = bold(M==.(M)))) +
  labs(y = bquote(bold("Performance gain: "*vartheta[m*"*(w1SE)"]-vartheta[m*"*(default)"])),
       x = bquote(bold(n[E]))) + 
  owntheme() %+replace%   
  theme()
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/finaltheta_paired-1.png)<!-- -->






### Stratified analysis

In the following, detailed results regarding the final model performance are presented. First, the last figure is stratified by learning task. The analysis is restricted to the case of $M=200$ candidate models.


```r
data$paired %>%
  filter(M==200) %>%
  filter(n.eval < 1000) %>%
  ggplot(aes(as.factor(n.eval), within1SE-default), colour="darkgrey") +
  geom_hline(yintercept = 0, cex=1, color="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ scenario,
             labeller = label_bquote(rows=bold(n[L]==.(n.learn)),
                                     cols=bold(.(scenario)))) +
  labs(y = bquote(bold("Performance gain: "*vartheta[m*"*(w1SE)"]*"-"*vartheta[m*"*(default)"])),
       x = bquote(bold(n[E]))) + 
  #ylim(c(-0.1, 0.2)) +
  owntheme() %+replace%   
  theme(axis.text.x = element_text(size=config$text.size, angle=90)) 
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/finaltheta_paired_stratified-1.png)<!-- -->


In the following the previous results are given in detail in two tables. The quantity of main interest here is the performance gain $\Delta=\E[\vartheta_{m^*|\text{w1SE}}-\vartheta_{m^*|\text{default}}]$ (`theta.delta`) for which (unadjusted) two-sided 95% confidence intervals and p-values regarding the null hypothesis $H_0: \Delta=0$ are given.  


```r
data$paired %>%
  mutate(delta = within1SE - default) %>% 
  group_by(M, n.learn, n.eval) %>%
  summarize(Nsim = n(),
            theta.max     = mean(opt.theta),
            theta.default = mean(default),
            theta.delta   = mean(delta),
            se    = sqrt(var(delta)/Nsim),
            lower = theta.delta + qnorm(0.025)*se,
            upper = theta.delta + qnorm(0.975)*se,
            pval  = pnorm(theta.delta/se, lower.tail=F)) %>%
  filter() %>%
  ungroup() %>%
  select(-Nsim, -se) %>%
  mutate_at(vars(-1), function(x){round(x, 4)} ) %>%
  datatable(class=c("compact"), rownames = FALSE, filter="top") %>%
  formatStyle(columns = 1:12, color="black") %>%
  formatStyle(columns = 1:3, color="black", fontWeight = "bold") %>%
  formatStyle(
    'theta.delta',
    background = styleColorBar(c(0,0.05), '#f05028'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  )
```

<!--html_preserve--><div id="htmlwidget-0dc3aef4782f371daecf" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0dc3aef4782f371daecf">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"40\" data-max=\"200\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"400\" data-max=\"800\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"100\" data-max=\"8000\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.8642\" data-max=\"0.8779\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.8445\" data-max=\"0.8666\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0023\" data-max=\"0.0183\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0023\" data-max=\"0.018\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0025\" data-max=\"0.0185\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[[40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200],[400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800],[100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000],[0.8642,0.8642,0.8642,0.8642,0.8642,0.875,0.875,0.875,0.875,0.875,0.8677,0.8677,0.8677,0.8677,0.8677,0.877,0.877,0.877,0.877,0.877,0.8692,0.8692,0.8692,0.8692,0.8692,0.8779,0.8779,0.8779,0.8779,0.8779],[0.8445,0.8446,0.8445,0.8446,0.8445,0.8651,0.8651,0.8651,0.8652,0.8651,0.8467,0.8467,0.8467,0.8467,0.8466,0.8662,0.8662,0.8662,0.8662,0.8662,0.8476,0.8476,0.8475,0.8475,0.8475,0.8665,0.8665,0.8665,0.8666,0.8665],[0.0083,0.0114,0.0136,0.0148,0.0161,0.0024,0.0044,0.006,0.0071,0.0085,0.0087,0.0119,0.0144,0.0158,0.0177,0.0028,0.0047,0.0064,0.0076,0.0094,0.0086,0.0119,0.0146,0.0163,0.0183,0.0032,0.005,0.0067,0.0079,0.0099],[0.0082,0.0112,0.0134,0.0147,0.016,0.0023,0.0043,0.0059,0.007,0.0084,0.0085,0.0117,0.0142,0.0157,0.0175,0.0027,0.0046,0.0063,0.0075,0.0093,0.0083,0.0117,0.0144,0.016,0.018,0.003,0.0048,0.0065,0.0077,0.0097],[0.0085,0.0115,0.0138,0.015,0.0163,0.0025,0.0045,0.0061,0.0072,0.0086,0.0088,0.0121,0.0145,0.016,0.0178,0.0029,0.0048,0.0065,0.0077,0.0095,0.0088,0.0121,0.0148,0.0165,0.0185,0.0033,0.0051,0.0068,0.008,0.01],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"compact\">\n  <thead>\n    <tr>\n      <th>M<\/th>\n      <th>n.learn<\/th>\n      <th>n.eval<\/th>\n      <th>theta.max<\/th>\n      <th>theta.default<\/th>\n      <th>theta.delta<\/th>\n      <th>lower<\/th>\n      <th>upper<\/th>\n      <th>pval<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5,6,7,8]}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':'black'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'color':'black'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'color':'black'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'color':'black'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'color':'black'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'color':'black'});\nvar value=data[11]; $(this.api().cell(row, 11).node()).css({'color':'black'});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (0.050000 - value)/0.050000 * 100 + '%, #f05028 ' + (0.050000 - value)/0.050000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

Next, we show the same table, additionally stratified for the prediction task scenario, for a more detailed analysis.


```r
data$paired %>%
  mutate(delta = within1SE - default) %>% 
  group_by(scenario, M, n.learn, n.eval) %>%
  summarize(Nsim = n(),
            theta.max     = mean(opt.theta),
            theta.default = mean(default),
            theta.delta   = mean(delta),
            se    = sqrt(var(delta)/Nsim),
            lower = theta.delta + qnorm(0.025)*se,
            upper = theta.delta + qnorm(0.975)*se,
            pval  = pnorm(theta.delta/se, lower.tail=F)) %>%
  ungroup() %>%
  select(-Nsim, -se) %>%
  #filter(n.eval == n.learn/4) %>%
  #mutate_at(vars(1:4), as.factor) %>%
  mutate_at(vars(-(1:4)), function(x){round(x, 4)} ) %>%
  datatable(class=c("compact"), rownames = FALSE, filter="top", extensions = 'FixedHeader',
  options = list(pageLength = 30, fixedHeader = FALSE)) %>%
  formatStyle(columns = 1:12, color="black") %>%
  formatStyle(columns = 1:4, color="black", fontWeight = "bold") %>%
  formatStyle(
    'theta.delta',
    background = styleColorBar(c(0,0.05), '#f05028'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  )
```

<!--html_preserve--><div id="htmlwidget-2c38272dcb8b5341603a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-2c38272dcb8b5341603a">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"40\" data-max=\"200\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"400\" data-max=\"800\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"100\" data-max=\"8000\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.8449\" data-max=\"0.9013\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.8233\" data-max=\"0.8901\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0011\" data-max=\"0.0213\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"7e-04\" data-max=\"0.0205\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.0014\" data-max=\"0.0222\" data-scale=\"4\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["FixedHeader"],"data":[["A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[I]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","A[R]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[I]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","B[R]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[I]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","C[R]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[I]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","D[R]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[I]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","E[R]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[I]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]","F[R]"],[40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200,40,40,40,40,40,40,40,40,40,40,100,100,100,100,100,100,100,100,100,100,200,200,200,200,200,200,200,200,200,200],[400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800,400,400,400,400,400,800,800,800,800,800],[100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000,100,200,400,800,8000],[0.8746,0.8746,0.8746,0.8746,0.8746,0.8806,0.8806,0.8806,0.8806,0.8806,0.8772,0.8772,0.8772,0.8772,0.8772,0.8817,0.8817,0.8817,0.8817,0.8817,0.878,0.878,0.878,0.878,0.878,0.882,0.882,0.882,0.882,0.882,0.8728,0.8728,0.8728,0.8728,0.8728,0.8793,0.8793,0.8793,0.8793,0.8793,0.8757,0.8757,0.8757,0.8757,0.8757,0.8807,0.8807,0.8807,0.8807,0.8807,0.8766,0.8766,0.8766,0.8766,0.8766,0.8811,0.8811,0.8811,0.8811,0.8811,0.8467,0.8467,0.8467,0.8467,0.8467,0.8538,0.8538,0.8538,0.8538,0.8538,0.8495,0.8495,0.8495,0.8495,0.8495,0.8552,0.8552,0.8552,0.8552,0.8552,0.8503,0.8503,0.8503,0.8503,0.8503,0.8557,0.8557,0.8557,0.8557,0.8557,0.8456,0.8456,0.8456,0.8456,0.8456,0.8531,0.8531,0.8531,0.8531,0.8531,0.8486,0.8486,0.8486,0.8486,0.8486,0.8548,0.8548,0.8548,0.8548,0.8548,0.8496,0.8496,0.8496,0.8496,0.8496,0.8554,0.8554,0.8554,0.8554,0.8554,0.8481,0.8481,0.8481,0.8481,0.8481,0.8628,0.8628,0.8628,0.8628,0.8628,0.8527,0.8527,0.8527,0.8527,0.8527,0.8655,0.8655,0.8655,0.8655,0.8655,0.8548,0.8548,0.8548,0.8548,0.8548,0.8668,0.8668,0.8668,0.8668,0.8668,0.8463,0.8463,0.8463,0.8463,0.8463,0.8611,0.8611,0.8611,0.8611,0.8611,0.8509,0.8509,0.8509,0.8509,0.8509,0.8638,0.8638,0.8638,0.8638,0.8638,0.853,0.853,0.853,0.853,0.853,0.865,0.865,0.865,0.865,0.865,0.8873,0.8873,0.8873,0.8873,0.8873,0.8983,0.8983,0.8983,0.8983,0.8983,0.8903,0.8903,0.8903,0.8903,0.8903,0.9003,0.9003,0.9003,0.9003,0.9003,0.8919,0.8919,0.8919,0.8919,0.8919,0.9013,0.9013,0.9013,0.9013,0.9013,0.8852,0.8852,0.8852,0.8852,0.8852,0.8963,0.8963,0.8963,0.8963,0.8963,0.8882,0.8882,0.8882,0.8882,0.8882,0.8982,0.8982,0.8982,0.8982,0.8982,0.8898,0.8898,0.8898,0.8898,0.8898,0.8992,0.8992,0.8992,0.8992,0.8992,0.8469,0.8469,0.8469,0.8469,0.8469,0.8615,0.8615,0.8615,0.8615,0.8615,0.8516,0.8516,0.8516,0.8516,0.8516,0.8642,0.8642,0.8642,0.8642,0.8642,0.8537,0.8537,0.8537,0.8537,0.8537,0.8655,0.8655,0.8655,0.8655,0.8655,0.8449,0.8449,0.8449,0.8449,0.8449,0.8597,0.8597,0.8597,0.8597,0.8597,0.8494,0.8494,0.8494,0.8494,0.8494,0.8624,0.8624,0.8624,0.8624,0.8624,0.8515,0.8515,0.8515,0.8515,0.8515,0.8637,0.8637,0.8637,0.8637,0.8637,0.8861,0.8861,0.8861,0.8861,0.8861,0.8967,0.8967,0.8967,0.8967,0.8967,0.889,0.889,0.889,0.889,0.889,0.8986,0.8986,0.8986,0.8986,0.8986,0.8905,0.8905,0.8905,0.8905,0.8905,0.8996,0.8996,0.8996,0.8996,0.8996,0.8861,0.8861,0.8861,0.8861,0.8861,0.8968,0.8968,0.8968,0.8968,0.8968,0.889,0.889,0.889,0.889,0.889,0.8987,0.8987,0.8987,0.8987,0.8987,0.8905,0.8905,0.8905,0.8905,0.8905,0.8997,0.8997,0.8997,0.8997,0.8997],[0.8595,0.8598,0.8596,0.8598,0.8597,0.8726,0.8726,0.8727,0.8726,0.8726,0.8602,0.8601,0.8601,0.86,0.8601,0.8729,0.873,0.873,0.8729,0.8729,0.8594,0.8595,0.8593,0.8592,0.8592,0.8724,0.8725,0.8724,0.8724,0.8724,0.8573,0.8575,0.8575,0.8575,0.8574,0.8712,0.8712,0.8712,0.8712,0.8712,0.8584,0.8587,0.8584,0.8584,0.8583,0.8714,0.8714,0.8713,0.8714,0.8713,0.8582,0.8581,0.8581,0.8581,0.8583,0.8709,0.8709,0.8711,0.871,0.871,0.8267,0.8266,0.8264,0.8264,0.8265,0.8436,0.8434,0.8434,0.8436,0.8435,0.8274,0.8273,0.8273,0.8273,0.8274,0.8437,0.8437,0.8438,0.8438,0.8436,0.8263,0.8266,0.8265,0.8266,0.8263,0.8434,0.8433,0.8432,0.8433,0.8432,0.8252,0.8253,0.825,0.8251,0.8252,0.8424,0.8424,0.8423,0.8423,0.8424,0.8255,0.8255,0.8254,0.8255,0.8253,0.8426,0.8427,0.8426,0.8425,0.8425,0.8249,0.8251,0.8247,0.8246,0.8252,0.8421,0.8421,0.8421,0.8422,0.8421,0.826,0.8258,0.8258,0.826,0.8258,0.8524,0.8524,0.8525,0.8524,0.8524,0.8297,0.8297,0.8299,0.8297,0.8296,0.8543,0.8543,0.8544,0.8544,0.8544,0.8319,0.8317,0.8315,0.832,0.8319,0.8552,0.8553,0.8552,0.8552,0.855,0.824,0.8241,0.8241,0.824,0.8242,0.8507,0.8506,0.8506,0.8508,0.8506,0.8277,0.8277,0.8277,0.8276,0.8277,0.8523,0.8523,0.8523,0.8523,0.8523,0.83,0.8299,0.8301,0.8298,0.8297,0.8529,0.8532,0.8531,0.8531,0.8531,0.8671,0.8675,0.8675,0.8675,0.8673,0.8883,0.8884,0.8883,0.8884,0.8884,0.8695,0.8695,0.8696,0.8699,0.8694,0.8895,0.8895,0.8895,0.8896,0.8896,0.8708,0.8707,0.8708,0.8707,0.8707,0.89,0.8899,0.8901,0.8901,0.8899,0.8661,0.866,0.866,0.866,0.8659,0.8865,0.8866,0.8866,0.8866,0.8866,0.8681,0.868,0.8679,0.8682,0.8678,0.8878,0.8877,0.8877,0.8878,0.8877,0.8694,0.8693,0.8693,0.8692,0.8695,0.8885,0.8885,0.8887,0.8885,0.8885,0.8248,0.8247,0.8245,0.8247,0.8245,0.8509,0.8508,0.8509,0.851,0.8508,0.8285,0.8286,0.8286,0.8285,0.8286,0.8524,0.8525,0.8525,0.8526,0.8525,0.8301,0.8304,0.8303,0.8305,0.83,0.8531,0.8531,0.8531,0.8531,0.8533,0.8234,0.8233,0.8235,0.8233,0.8234,0.8491,0.8491,0.8493,0.8491,0.8492,0.827,0.8269,0.827,0.827,0.8272,0.851,0.851,0.851,0.8509,0.8509,0.8291,0.8292,0.8291,0.8289,0.8292,0.8518,0.8517,0.8516,0.8518,0.8517,0.8676,0.8675,0.8671,0.8674,0.8673,0.8872,0.8874,0.8872,0.8873,0.8873,0.8693,0.8694,0.8694,0.8693,0.8694,0.8881,0.8881,0.8882,0.8882,0.8881,0.8702,0.87,0.8701,0.8703,0.87,0.8887,0.8887,0.8887,0.8888,0.8887,0.8667,0.8668,0.8669,0.8669,0.867,0.8867,0.8865,0.8865,0.8866,0.8867,0.8687,0.8688,0.869,0.8689,0.8686,0.8882,0.8883,0.8883,0.8882,0.8882,0.8703,0.87,0.8699,0.8699,0.8701,0.889,0.8891,0.8891,0.889,0.8891],[0.0063,0.0091,0.0111,0.0119,0.0131,0.0012,0.0032,0.0047,0.0059,0.0072,0.0075,0.0108,0.0129,0.0141,0.0153,0.0017,0.0036,0.0053,0.0065,0.008,0.0087,0.0116,0.014,0.0153,0.0168,0.0028,0.0045,0.0061,0.0071,0.0087,0.0065,0.0093,0.0111,0.0123,0.0134,0.0011,0.0033,0.0048,0.006,0.0073,0.0073,0.0101,0.0126,0.014,0.0155,0.0019,0.0038,0.0058,0.0068,0.0086,0.008,0.0109,0.0131,0.0148,0.0162,0.0029,0.0048,0.0063,0.0075,0.0092,0.0077,0.0116,0.0143,0.0159,0.017,0.0017,0.0041,0.0062,0.0075,0.0092,0.0085,0.0129,0.016,0.0176,0.019,0.0026,0.0048,0.0071,0.0086,0.0106,0.0101,0.0137,0.017,0.0188,0.0209,0.0032,0.0052,0.0078,0.0093,0.0113,0.0081,0.0116,0.0147,0.0161,0.0174,0.0017,0.0041,0.0065,0.008,0.0097,0.0092,0.0134,0.0167,0.0185,0.0204,0.0025,0.0047,0.0075,0.0092,0.0111,0.0104,0.0141,0.0179,0.0199,0.0213,0.0034,0.0058,0.0086,0.0099,0.012,0.01,0.0132,0.0152,0.0163,0.0179,0.003,0.0049,0.0063,0.0075,0.0089,0.0101,0.0131,0.0155,0.017,0.019,0.0031,0.005,0.0065,0.0075,0.0094,0.0088,0.0123,0.0156,0.0167,0.0189,0.0033,0.005,0.0066,0.0076,0.01,0.0098,0.0128,0.015,0.0162,0.0176,0.0027,0.0048,0.0063,0.0072,0.0089,0.0097,0.013,0.0156,0.0171,0.0188,0.0031,0.005,0.0066,0.0078,0.0097,0.0086,0.0123,0.0152,0.017,0.0192,0.0034,0.0051,0.0067,0.008,0.01,0.0084,0.0111,0.0131,0.0144,0.0159,0.0028,0.0046,0.0061,0.0071,0.0084,0.0082,0.0112,0.0135,0.0146,0.0169,0.0031,0.0049,0.0063,0.0074,0.0091,0.0079,0.0111,0.0134,0.015,0.0172,0.0034,0.0053,0.0064,0.0075,0.0096,0.0082,0.0109,0.013,0.0143,0.0156,0.0029,0.0043,0.0059,0.0068,0.0082,0.0081,0.011,0.0133,0.0145,0.0169,0.0028,0.0046,0.0061,0.0072,0.009,0.0077,0.0107,0.013,0.0147,0.0166,0.0026,0.0044,0.0057,0.0071,0.0091,0.0097,0.0128,0.0152,0.0163,0.0178,0.0031,0.0051,0.0065,0.0075,0.009,0.0094,0.0131,0.0156,0.0172,0.0189,0.0036,0.0053,0.0069,0.008,0.0099,0.0095,0.0126,0.0156,0.0172,0.0197,0.004,0.0056,0.0072,0.0084,0.0103,0.0098,0.0128,0.0144,0.0159,0.0172,0.0033,0.0052,0.0063,0.0076,0.0089,0.0096,0.0131,0.0152,0.0167,0.0184,0.0031,0.0052,0.0065,0.008,0.0098,0.0083,0.0121,0.0148,0.0168,0.0186,0.0033,0.0053,0.0068,0.0079,0.0101,0.0075,0.0103,0.0128,0.0138,0.0152,0.0025,0.0042,0.0056,0.0067,0.0081,0.0081,0.0104,0.0127,0.0143,0.0161,0.003,0.0046,0.0059,0.0072,0.009,0.0076,0.0107,0.0128,0.0144,0.0169,0.0031,0.0046,0.0062,0.0073,0.0093,0.0081,0.0108,0.0131,0.0143,0.0156,0.0029,0.0051,0.0064,0.0073,0.0087,0.008,0.0109,0.013,0.0146,0.0169,0.0028,0.0045,0.006,0.0073,0.009,0.0072,0.0105,0.013,0.0147,0.0168,0.0026,0.0042,0.0059,0.0071,0.009],[0.0058,0.0085,0.0105,0.0114,0.0126,0.0008,0.0028,0.0044,0.0056,0.0069,0.0069,0.0102,0.0124,0.0135,0.0147,0.0014,0.0032,0.005,0.0061,0.0076,0.0079,0.0107,0.0131,0.0145,0.016,0.0023,0.0039,0.0055,0.0066,0.0082,0.0059,0.0087,0.0105,0.0118,0.0129,0.0007,0.003,0.0045,0.0057,0.007,0.0067,0.0096,0.0121,0.0135,0.0149,0.0015,0.0035,0.0054,0.0065,0.0082,0.0072,0.0101,0.0123,0.014,0.0154,0.0024,0.0043,0.0059,0.007,0.0087,0.0071,0.011,0.0137,0.0154,0.0165,0.0013,0.0037,0.0058,0.0072,0.0089,0.0079,0.0123,0.0154,0.017,0.0185,0.0022,0.0044,0.0068,0.0083,0.0103,0.0093,0.0129,0.0162,0.0179,0.0201,0.0027,0.0047,0.0073,0.0089,0.0108,0.0075,0.011,0.0141,0.0155,0.0168,0.0013,0.0037,0.0061,0.0076,0.0094,0.0086,0.0128,0.0161,0.0179,0.0198,0.0022,0.0043,0.0072,0.0088,0.0107,0.0096,0.0133,0.0171,0.0191,0.0205,0.0028,0.0053,0.008,0.0094,0.0115,0.0094,0.0126,0.0146,0.0157,0.0173,0.0026,0.0045,0.006,0.0071,0.0085,0.0095,0.0125,0.0149,0.0164,0.0184,0.0027,0.0046,0.0061,0.0071,0.009,0.0079,0.0115,0.0147,0.0159,0.0182,0.0027,0.0045,0.0061,0.0072,0.0095,0.0091,0.0121,0.0144,0.0156,0.0169,0.0023,0.0044,0.006,0.0069,0.0085,0.009,0.0123,0.015,0.0165,0.0182,0.0027,0.0046,0.0063,0.0074,0.0093,0.0078,0.0115,0.0144,0.0162,0.0184,0.0029,0.0046,0.0062,0.0075,0.0095,0.0078,0.0106,0.0125,0.0139,0.0154,0.0025,0.0042,0.0057,0.0067,0.008,0.0077,0.0106,0.0129,0.0141,0.0163,0.0028,0.0046,0.0059,0.007,0.0087,0.0071,0.0103,0.0126,0.0142,0.0164,0.003,0.0048,0.0059,0.007,0.0091,0.0076,0.0103,0.0125,0.0138,0.0151,0.0025,0.004,0.0056,0.0065,0.0078,0.0076,0.0105,0.0128,0.014,0.0163,0.0025,0.0043,0.0058,0.0069,0.0086,0.0069,0.01,0.0122,0.0139,0.0159,0.0021,0.004,0.0053,0.0066,0.0086,0.009,0.0121,0.0146,0.0157,0.0171,0.0027,0.0047,0.0061,0.0071,0.0086,0.0088,0.0125,0.015,0.0166,0.0183,0.0032,0.0049,0.0065,0.0076,0.0096,0.0087,0.0118,0.0148,0.0163,0.0189,0.0035,0.005,0.0066,0.0078,0.0098,0.0092,0.0122,0.0138,0.0153,0.0166,0.0029,0.0048,0.006,0.0072,0.0085,0.009,0.0125,0.0146,0.0161,0.0178,0.0027,0.0048,0.0062,0.0076,0.0094,0.0075,0.0113,0.014,0.016,0.0178,0.0027,0.0047,0.0062,0.0074,0.0096,0.007,0.0098,0.0123,0.0133,0.0147,0.0021,0.0039,0.0053,0.0064,0.0078,0.0076,0.0099,0.0122,0.0138,0.0156,0.0026,0.0043,0.0056,0.0069,0.0087,0.0069,0.01,0.0121,0.0137,0.0162,0.0026,0.0041,0.0057,0.0068,0.0089,0.0075,0.0103,0.0125,0.0138,0.0151,0.0026,0.0047,0.0061,0.007,0.0084,0.0075,0.0104,0.0125,0.014,0.0164,0.0025,0.0042,0.0057,0.007,0.0087,0.0065,0.0097,0.0123,0.0139,0.0161,0.0022,0.0038,0.0055,0.0067,0.0086],[0.0069,0.0096,0.0116,0.0124,0.0136,0.0016,0.0035,0.0051,0.0062,0.0075,0.0081,0.0113,0.0135,0.0146,0.0158,0.0021,0.0039,0.0057,0.0068,0.0083,0.0096,0.0124,0.0148,0.0162,0.0177,0.0034,0.005,0.0066,0.0076,0.0092,0.0071,0.0098,0.0116,0.0129,0.014,0.0014,0.0036,0.0051,0.0063,0.0076,0.0079,0.0107,0.0132,0.0146,0.016,0.0022,0.0042,0.0061,0.0071,0.0089,0.0088,0.0117,0.0139,0.0156,0.017,0.0034,0.0053,0.0068,0.0079,0.0097,0.0083,0.0122,0.0148,0.0165,0.0176,0.002,0.0044,0.0065,0.0079,0.0095,0.0091,0.0134,0.0166,0.0181,0.0196,0.003,0.0052,0.0075,0.009,0.0109,0.011,0.0145,0.0178,0.0196,0.0217,0.0037,0.0057,0.0083,0.0098,0.0118,0.0087,0.0122,0.0153,0.0167,0.0179,0.0021,0.0045,0.0068,0.0083,0.01,0.0098,0.014,0.0173,0.0191,0.021,0.0029,0.0051,0.0079,0.0096,0.0114,0.0113,0.015,0.0188,0.0207,0.0222,0.0039,0.0063,0.0091,0.0104,0.0125,0.0107,0.0138,0.0159,0.0169,0.0185,0.0034,0.0053,0.0067,0.0079,0.0092,0.0108,0.0137,0.0161,0.0176,0.0195,0.0035,0.0053,0.0068,0.0078,0.0097,0.0096,0.0131,0.0164,0.0175,0.0197,0.0038,0.0054,0.0071,0.0081,0.0105,0.0104,0.0134,0.0157,0.0169,0.0182,0.0031,0.0052,0.0067,0.0076,0.0093,0.0103,0.0136,0.0162,0.0177,0.0194,0.0035,0.0054,0.007,0.0081,0.01,0.0095,0.0132,0.016,0.0178,0.02,0.0039,0.0056,0.0072,0.0085,0.0105,0.009,0.0117,0.0136,0.015,0.0164,0.0032,0.0049,0.0064,0.0074,0.0087,0.0088,0.0117,0.014,0.0151,0.0174,0.0035,0.0053,0.0066,0.0077,0.0094,0.0087,0.0118,0.0141,0.0157,0.0179,0.0039,0.0057,0.0068,0.0079,0.01,0.0087,0.0114,0.0135,0.0148,0.0161,0.0033,0.0047,0.0062,0.0071,0.0085,0.0086,0.0116,0.0138,0.015,0.0174,0.0031,0.005,0.0065,0.0075,0.0093,0.0084,0.0115,0.0137,0.0154,0.0173,0.003,0.0049,0.0062,0.0075,0.0095,0.0104,0.0134,0.0158,0.0169,0.0184,0.0036,0.0055,0.0069,0.0079,0.0094,0.0101,0.0137,0.0162,0.0178,0.0195,0.004,0.0057,0.0073,0.0084,0.0103,0.0104,0.0135,0.0164,0.018,0.0206,0.0046,0.0061,0.0077,0.0089,0.0108,0.0104,0.0134,0.0151,0.0165,0.0178,0.0037,0.0056,0.0067,0.008,0.0092,0.0102,0.0137,0.0158,0.0173,0.0189,0.0035,0.0056,0.0069,0.0083,0.0102,0.0091,0.0129,0.0156,0.0176,0.0193,0.0038,0.0058,0.0073,0.0084,0.0105,0.0081,0.0108,0.0134,0.0144,0.0157,0.0029,0.0046,0.006,0.007,0.0084,0.0086,0.0109,0.0132,0.0148,0.0166,0.0033,0.0049,0.0063,0.0075,0.0093,0.0083,0.0114,0.0135,0.0151,0.0176,0.0035,0.005,0.0066,0.0077,0.0097,0.0087,0.0114,0.0136,0.0149,0.0161,0.0033,0.0055,0.0068,0.0077,0.009,0.0086,0.0115,0.0135,0.0151,0.0175,0.0031,0.0049,0.0064,0.0077,0.0093,0.0079,0.0112,0.0138,0.0154,0.0176,0.0031,0.0046,0.0063,0.0075,0.0094],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"compact\">\n  <thead>\n    <tr>\n      <th>scenario<\/th>\n      <th>M<\/th>\n      <th>n.learn<\/th>\n      <th>n.eval<\/th>\n      <th>theta.max<\/th>\n      <th>theta.default<\/th>\n      <th>theta.delta<\/th>\n      <th>lower<\/th>\n      <th>upper<\/th>\n      <th>pval<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":30,"fixedHeader":false,"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9]}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"lengthMenu":[10,25,30,50,100],"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':'black'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'color':'black'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'color':'black'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'color':'black'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'color':'black'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'color':'black'});\nvar value=data[11]; $(this.api().cell(row, 11).node()).css({'color':'black'});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (0.050000 - value)/0.050000 * 100 + '%, #f05028 ' + (0.050000 - value)/0.050000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


## Estimation

In this section we investigate how well the true final model performance (accuracy) $\vartheta_{m^*}$ can be estimated. In particular, we are interested in the distribution of the relative deviation $(\thetahat_{m^*} - \vartheta_{m^*})/\vartheta_{m^*}$ between the point estimate $\thetahat_{m^*}$ and the true value $\vartheta_{m^*}$.

### Naive performance estimation

The obvious naive estimator for classification accuracy is just the sample proportion of correct predictions. In our simulations we implemented a slightly different naive estimator by adding two pseudo-observations (1 correct and 1 false prediction) to the observated evaluation data before calculating this sample proportion. This procedure has a Bayesian motivation and results in a slightly more conservative (downward biased) estimate. The main reason for using this estimator was to prevent a singular covariance matrix due to estimates $\thetahat_m \{0,1\}$. The sample mean of deviations (red) is an estimator for the bias of the estimator. The slight downward bias observed for the *default* approach (evaluate only single model) results only from this modification.


```r
## define methods to investigate
bias.active <- info$select.rule %in% config$bias.methods  

## properties of naive estimatior:
data$raw %>%
  filter(M==200) %>%
  filter(select.rule %in% config$bias.methods) %>%
  ggplot(aes(select.rule, rel.dev, colour=select.rule)) +
  geom_hline(yintercept=0, cex=1, col="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(y = bquote(bold("Relative deviation: ")*(hat(vartheta)[m*"*"]-vartheta[m*"*"])/vartheta[m*"*"]))+ 
    scale_color_manual(values = info$col[bias.active], name="Selection rule:  ",
                     labels = paste0(info$select.rule[bias.active], "  ")) +
  owntheme() %+replace%   
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'))
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/bias_naive-1.png)<!-- -->





### Corrected performance estimation

The corrected estimator $\thetatilde$ takes into account the number of models in the evaluation study as well as the correlation structure between raw performance estimates. It is computed as a simultaneous lower 50%-confidence bound for all model performances and is thus a median conservative point estimator. In effect, the corrected estimate is rather biased downward than upward. This bias again decreases with increasing $n_\setE$. Note that naive and corrected estimator coincide if only a single model is evaluated (*default* selection rule).


```r
## properties of corrected estimator
data$raw %>%
  filter(M==200) %>%
  filter(select.rule %in% config$bias.methods) %>%
  ggplot(aes(select.rule, abs.dev.c, colour=select.rule)) +
  geom_hline(yintercept=0, cex=1, col="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(y = bquote(bold("Relative deviation: ")*(tilde(vartheta)[m*"*"]-vartheta[m*"*"])/vartheta[m*"*"]))+ 
  scale_color_manual(values = info$col[bias.active], name="Selection rule:  ",
                     labels = paste0(info$select.rule[bias.active], "  ")) +
  owntheme() %+replace%   
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/bias_corrected-1.png)<!-- -->


### Detailed results

Finally, more detailed results are given in the following table. We investiage the relative Bias, RMSE and MAD of the naive [N] and corrected [C] estimators. All these number are given as percent values, i.e. are multiplied by 100.





```r
bias.active <- info$select.rule %in% config$bias.methods  

## range of observed proportions:
config$p.range <- c(-5,5)

## estimator properties:
data$raw %>%
  filter(select.rule %in% config$bias.methods) %>%
  group_by(M, n.learn, n.eval, select.rule) %>%
  summarize(Nsim = n(),
            bias.naive = mean(rel.dev),
            bias.corrected = mean(rel.dev.c),
            rmse.naive  = sqrt(mean(rel.dev^2)),
            rmse.corrected  = sqrt(mean(rel.dev.c^2)),
            mad.naive  = mean(abs(rel.dev)),
            mad.corrected  = mean(abs(rel.dev.c))) %>%
  mutate_at(vars(matches("bias|mse|mad")), function(x)round(100*x, 3)) %>%
  arrange(-M, n.learn, n.eval) %>%
  select(-Nsim) %>%
  datatable(class=c("compact"), rownames = FALSE, filter="top", extensions = 'FixedHeader',
  options = list(pageLength = 20, fixedHeader = TRUE),
  colnames=c("M", "n.learn", "n.eval", "select.rule", #"Nsim",
                       "rBias [N]", "rBias [C]", "rRMSE [N]", "rRMSE [C]", "rMAD [N]", "rMAD [C]")) %>%
  formatStyle(columns = 1:12, color="black") %>%
  formatStyle(columns = 1:4, color="black", fontWeight = "bold") %>%
    formatStyle('select.rule', #target = 'row',
              color = styleEqual(info$select.rule[bias.active], info$col[bias.active])) %>%
  formatStyle(
    c('bias.naive', 'rmse.naive', 'mad.naive'),
    background = styleColorBar(config$p.range, '#8d99ae'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) %>% 
  formatStyle(
    c('bias.corrected', 'rmse.corrected', 'mad.corrected'),
    background = styleColorBar(config$p.range, '#f05028'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) 
```

<!--html_preserve--><div id="htmlwidget-b0a51b131783fa4c8f19" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b0a51b131783fa4c8f19">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"40\" data-max=\"200\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"400\" data-max=\"800\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"100\" data-max=\"8000\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;default&quot;,&quot;within1SE&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"-0.847\" data-max=\"1.496\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"-1.454\" data-max=\"-0.01\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.44\" data-max=\"4.29\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.456\" data-max=\"4.29\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.351\" data-max=\"3.396\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.363\" data-max=\"3.396\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["FixedHeader"],"data":[[200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40],[400,400,400,400,400,400,400,400,400,400,800,800,800,800,800,800,800,800,800,800,400,400,400,400,400,400,400,400,400,400,800,800,800,800,800,800,800,800,800,800,400,400,400,400,400,400,400,400,400,400,800,800,800,800,800,800,800,800,800,800],[100,100,200,200,400,400,800,800,8000,8000,100,100,200,200,400,400,800,800,8000,8000,100,100,200,200,400,400,800,800,8000,8000,100,100,200,200,400,400,800,800,8000,8000,100,100,200,200,400,400,800,800,8000,8000,100,100,200,200,400,400,800,800,8000,8000],["default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE","default","within1SE"],[-0.821,1.496,-0.428,1.094,-0.206,0.728,-0.105,0.433,-0.015,0.056,-0.847,1.427,-0.425,1.206,-0.216,0.828,-0.115,0.548,-0.01,0.089,-0.806,1.426,-0.43,0.982,-0.201,0.624,-0.108,0.345,-0.014,0.036,-0.847,1.366,-0.43,1.1,-0.225,0.719,-0.118,0.45,-0.01,0.067,-0.798,1.18,-0.428,0.75,-0.204,0.449,-0.111,0.227,-0.014,0.017,-0.835,1.096,-0.418,0.809,-0.227,0.5,-0.12,0.293,-0.011,0.037],[-0.821,-1.418,-0.428,-1.121,-0.206,-0.923,-0.105,-0.772,-0.015,-0.337,-0.847,-1.197,-0.425,-0.788,-0.216,-0.666,-0.115,-0.545,-0.01,-0.266,-0.806,-1.445,-0.43,-1.157,-0.201,-0.937,-0.108,-0.775,-0.014,-0.322,-0.847,-1.205,-0.43,-0.81,-0.225,-0.678,-0.118,-0.552,-0.01,-0.252,-0.798,-1.454,-0.428,-1.157,-0.204,-0.918,-0.111,-0.743,-0.014,-0.289,-0.835,-1.214,-0.418,-0.848,-0.227,-0.678,-0.12,-0.541,-0.011,-0.226],[4.259,4.065,3.021,2.939,2.134,2.071,1.513,1.447,0.495,0.46,3.949,3.757,2.803,2.807,1.978,1.995,1.405,1.414,0.456,0.442,4.258,4.078,3.033,2.923,2.143,2.055,1.515,1.437,0.496,0.461,3.954,3.771,2.798,2.784,1.983,1.971,1.408,1.389,0.457,0.44,4.29,4.087,3.059,2.921,2.162,2.055,1.527,1.445,0.499,0.469,3.967,3.759,2.811,2.742,1.985,1.941,1.415,1.368,0.458,0.442],[4.259,4.146,3.021,3.021,2.134,2.205,1.513,1.622,0.495,0.582,3.949,3.792,2.803,2.718,1.978,1.975,1.405,1.444,0.456,0.518,4.258,4.182,3.033,3.054,2.143,2.228,1.515,1.638,0.496,0.576,3.954,3.821,2.798,2.738,1.983,1.997,1.408,1.457,0.457,0.513,4.29,4.235,3.059,3.111,2.162,2.256,1.527,1.649,0.499,0.564,3.967,3.872,2.811,2.796,1.985,2.028,1.415,1.474,0.458,0.507],[3.367,3.256,2.389,2.349,1.688,1.654,1.199,1.156,0.393,0.367,3.112,3.023,2.218,2.258,1.568,1.602,1.12,1.136,0.363,0.352,3.366,3.26,2.398,2.336,1.694,1.637,1.199,1.147,0.393,0.367,3.117,3.028,2.216,2.234,1.574,1.578,1.119,1.111,0.363,0.351,3.396,3.264,2.417,2.327,1.711,1.634,1.209,1.15,0.396,0.372,3.129,3.008,2.225,2.189,1.576,1.546,1.125,1.092,0.364,0.353],[3.367,3.287,2.389,2.388,1.688,1.752,1.199,1.29,0.393,0.467,3.112,2.995,2.218,2.154,1.568,1.563,1.12,1.145,0.363,0.414,3.366,3.316,2.398,2.416,1.694,1.767,1.199,1.303,0.393,0.462,3.117,3.015,2.216,2.172,1.574,1.581,1.119,1.157,0.363,0.409,3.396,3.354,2.417,2.465,1.711,1.789,1.209,1.312,0.396,0.451,3.129,3.056,2.225,2.217,1.576,1.609,1.125,1.17,0.364,0.403]],"container":"<table class=\"compact\">\n  <thead>\n    <tr>\n      <th>M<\/th>\n      <th>n.learn<\/th>\n      <th>n.eval<\/th>\n      <th>select.rule<\/th>\n      <th>rBias [N]<\/th>\n      <th>rBias [C]<\/th>\n      <th>rRMSE [N]<\/th>\n      <th>rRMSE [C]<\/th>\n      <th>rMAD [N]<\/th>\n      <th>rMAD [C]<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":20,"fixedHeader":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,4,5,6,7,8,9]}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"lengthMenu":[10,20,25,50,100],"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':'black'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'color':'black'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'color':'black'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'color':'black'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'color':'black'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'color':'black'});\nvar value=data[11]; $(this.api().cell(row, 11).node()).css({'color':'black'});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':value == 'default' ? '#E08B00' : value == 'within1SE' ? '#00B81F' : ''});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #8d99ae ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #8d99ae ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #8d99ae ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #f05028 ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #f05028 ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= -5.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (5.000000 - value)/10.000000 * 100 + '%, #f05028 ' + (5.000000 - value)/10.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->



## Test decisions

In our framework, a prediction model $\hatf_m$ can be seen to be positively evaluated if the null hypothesis $H_0^m: \vartheta_m \leq \vartheta_0$ can be rejected in the evaluation study. Hereby $\vartheta_0$ can be the performance of a reference model or, in our case, a prespecified performance benchmark, e.g. an accuracy of $90\%$. In the following, we will investigate type 1 and type 2 errors rate of the employed statistical test when the number of evaluated models changes.

### Type 1 error rate
The type 1 error rate or family wise error rate (FWER) in the multiple texting context is given as the probability to reject at least one false hypotheses. The FWER is required to be bounded by the significance level $\alpha$ which is set to $2.5\%$ in all our simulations. The first three figures are show only result for the case $M=200$.


```r
## get colors/shapes
cols.act <- info$col[active]; names(cols.act) <- info$select.rule[active]
shapes.act <- info$pch[active]; names(shapes.act) <- info$select.rule[active]
y.breaks <- seq(0,0.05,0.01)

## type 1 error rate:
data$test %>%
  filter(M==200, n.eval<1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=0.75) +
  geom_line( aes(shift, fpr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(shift, fpr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("Type 1 error rate: ")* pi[0](delta)==P(varphi[m*"*"]==1~"|"~delta, H[0]^{m*"*"}))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule  ", 
                     labels=paste0(info$select.rule[active], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/fpr-1.png" style="display: block; margin: auto;" />


### Statistical power

Statistical power is the probability to correctly reject at least one false null hypotheses. This is equal to the probability to correctly reject the global null hypotheses $G = \cap_{m\in \setM} H_0^m$ if it is false. In our model evaluation framework, the global null $G$ corresponds to the case that none of the considered candidate models perform well enough, i.e. all performances $\vartheta_m$ are less or equal then the  prespecified threshold $\vartheta_0$. Therefore, the minimal requirement for a successfull evaluation study from a statistical viewpoint is rejection of $G$. Comparing the statistical power of a test is a accepted way to choose between different tests with same type 1 error.


```r
y.breaks <- seq(0,1,0.1)

## statistical power:
data$test %>%
  filter(M==200, n.eval<1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=0.75) +
  geom_line( aes(shift, tpr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(shift, tpr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("Power: ")* pi[1](delta)==P(varphi[m*"*"]==1~"|"~delta, H[1]^{m*"*"}))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/tpr-1.png" style="display: block; margin: auto;" />


### Overall rejection rate

A more compact way to combine the two previous figures is to show the overall rejection rate.


```r
y.breaks <- seq(0, 1, 0.1)

data$test %>%
  filter(M==200, n.eval<1000, n.eval>100) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=0.75) +
  geom_line( aes(shift, rr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(shift, rr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("Rejection rate: ")* pi(delta)==P(varphi[m*"*"]==1~"|"~delta))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size, angle=45, vjust=0.85, hjust=0.85))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/rr-1.png" style="display: block; margin: auto;" />





### Stratified analysis
In the following we will investigate the previous findings from a different perspective. We will include all
cases $M \in \{40, 100, 200\}$ but only look at the type 1 error rate (false positive test decisions) at $\delta = \vartheta_{max} - \vartheta_0 = 0$ and statistical power (true positive test decisions) at $\delta=0.05$.



```r
y.breaks <- seq(0, 0.05, 0.01)
data$test %>%
  filter(shift==0) %>%
  filter(n.eval < 1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_line( aes(n.eval, fpr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(n.eval, fpr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ M,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(M==.(M)))) +
  labs(x = bquote(n[E]),
       y = bquote(bold("Type 1 error rate: ")* pi[0](0)==P(varphi[m*"*"]==1~"|"~delta==0, H[0]^{m*"*"}))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/fpr_straftified-1.png)<!-- -->


```r
y.breaks <- seq(0,1,0.1)

## overall rejection rate (fpr+tpr):
data$test %>%
  filter(shift==0.05) %>%
  filter(n.eval < 1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_line( aes(n.eval, tpr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(n.eval, tpr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ M,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(M==.(M)))) +
  labs(x = bquote(bold(n[E])),
       y = bquote(bold("Power: ")* pi[1](0.05)==P(varphi[m*"*"]==1~"|"~delta==0.05, H[1]^{m*"*"}))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/tpr_stratified-1.png" style="display: block; margin: auto;" />

## Further results
In the following, the results of different sensitivity and auxiliary analyses are presented.

### Model performance and similarity


```r
data.details <- read_data(c("MLE_SIM_ACC_M50_GLMNET", "MLE_SIM_ACC_M50_RPARTCOST", "MLE_SIM_ACC_M50_XGBTREE", "MLE_SIM_ACC_M50_SVMLINEARWEIGHTS2")) 
```

The following table gives a more detailed overview of the model quality per learning algorithm, task and sample size. The column `theta.max` indicates the maximum true performance $\vartheta_{max}$ stratified by algorithm, averaged over all simulation runs. The column `S` shows how many models (trained by the respective algorithm) are within 1 Se of the best validation model. The final column `dependence` contains (the mean of) a dimensionless measure of dependence defined as $\varrho=1 - (c_{\alpha}-c_d)/(c_i-c_d)$. Hereby $c_\alpha$ is the actual critical value, $c_d$ is the critical value under perfect dependence (i.e. corresponding quantile of the univariate normal distribution) and $c_i$ is the according quantile of the $S$-dimensional normal distribution under the assumption of independece (of the model predictions). This implies $\varrho=0$ if the performances of the $S$ evaluated models are completely independent and $\varrho=1$ if the models are actually all the same (at least on the test data, which in this analysis has a size of $n_\setE=8000$.)


```r
data.details %>% preproc_data() %>% {.$raw} %>%
  filter(select.rule=="within1SE", n.eval==8000) %>%
  mutate(methods = recode(methods, glmnet="EN", rpartCost="CART", svmLinearWeights2="SVM", xgbTree="XGB")) %>% 
  mutate(theta.opt = sapply(score, function(x) switch(x, A=0.885, B=0.86, C=0.951, D=0.966, E=0.95, F=0.963))) %>%
  group_by(score, n.learn, methods) %>%
  summarize(Nsim=n(),
            theta.opt = mean(theta.opt),
            theta.max.m = mean(opt.theta),
            theta.max.sd = sd(opt.theta),
            S.m = mean(S),
            S.sd = sd(S),
            dependence.m = mean(dependence, na.rm=T),
            dependence.sd = sd(dependence, na.rm=T)) %>%
  mutate_at(5:11, function(x) round(x, 3)) %>%
  select(score, n.learn, algorithm=methods, theta.opt, theta.max=theta.max.m, S=S.m, dependence=dependence.m) %>%
  mutate(S = round(S, 1)) %>%
  datatable(class=c("compact"), rownames = FALSE, filter="top", extensions = 'FixedHeader',
  options = list(pageLength = 8, fixedHeader = FALSE)) %>%
  formatStyle(columns = 1:12, color="black") %>%
  formatStyle(columns = 1:2, color="black", fontWeight = "bold") %>%
  formatStyle('algorithm', fontWeight = "bold",
              color = styleEqual(unique(data.details$methods),
                                 colorspace::rainbow_hcl(4))) %>%
  formatStyle(
    c('theta.max'),
    background = styleColorBar(c(0.50, 1), '#f05028'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) %>%
  formatStyle(
    c('S'),
    background = styleColorBar(c(1:50), '#8d99ae'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) %>%
    formatStyle(
    c('dependence'),
    background = styleColorBar(c(0:1), '#8d99ae'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = "bold"
  ) 
```

<!--html_preserve--><div id="htmlwidget-692cfdcfbe8bfbe2f66d" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-692cfdcfbe8bfbe2f66d">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"400\" data-max=\"800\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.86\" data-max=\"0.966\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.728\" data-max=\"0.9\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"8.6\" data-max=\"35.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.179\" data-max=\"0.613\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["FixedHeader"],"data":[["A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F"],[400,400,400,400,800,800,800,800,400,400,400,400,800,800,800,800,400,400,400,400,800,800,800,800,400,400,400,400,800,800,800,800,400,400,400,400,800,800,800,800,400,400,400,400,800,800,800,800],["CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB","CART","EN","SVM","XGB"],[0.885,0.885,0.885,0.885,0.885,0.885,0.885,0.885,0.86,0.86,0.86,0.86,0.86,0.86,0.86,0.86,0.951,0.951,0.951,0.951,0.951,0.951,0.951,0.951,0.966,0.966,0.966,0.966,0.966,0.966,0.966,0.966,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.963,0.963,0.963,0.963,0.963,0.963,0.963,0.963],[0.728,0.877,0.851,0.829,0.748,0.882,0.868,0.85,0.783,0.85,0.827,0.822,0.793,0.856,0.843,0.835,0.833,0.754,0.738,0.854,0.841,0.759,0.752,0.866,0.879,0.859,0.852,0.89,0.886,0.862,0.857,0.9,0.832,0.753,0.737,0.853,0.839,0.758,0.751,0.865,0.879,0.862,0.856,0.89,0.886,0.864,0.86,0.9],[15.7,15.9,17.5,9.8,12.9,19.4,18,10.3,15.7,16.1,17.6,13,12,18.7,18.2,13.7,15.6,22.8,15.7,9.3,12.6,21.2,14.8,10.7,13.1,34.6,8.8,15.8,9.6,34.3,9.3,13.5,15.5,23.5,15.6,9.2,12.4,21.3,14.8,10.7,12.6,35,8.6,17.5,9.2,35.7,9.3,13.9],[0.301,0.453,0.304,0.179,0.259,0.511,0.368,0.215,0.327,0.44,0.311,0.233,0.267,0.498,0.375,0.256,0.356,0.504,0.3,0.232,0.298,0.539,0.351,0.26,0.378,0.599,0.348,0.306,0.323,0.605,0.424,0.294,0.353,0.507,0.303,0.232,0.296,0.538,0.352,0.26,0.377,0.606,0.354,0.318,0.323,0.613,0.432,0.298]],"container":"<table class=\"compact\">\n  <thead>\n    <tr>\n      <th>score<\/th>\n      <th>n.learn<\/th>\n      <th>algorithm<\/th>\n      <th>theta.opt<\/th>\n      <th>theta.max<\/th>\n      <th>S<\/th>\n      <th>dependence<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":8,"fixedHeader":false,"columnDefs":[{"className":"dt-right","targets":[1,3,4,5,6]}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"lengthMenu":[8,10,25,50,100],"rowCallback":"function(row, data) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'color':'black'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'color':'black'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'color':'black'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'color':'black'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'color':'black'});\nvar value=data[7]; $(this.api().cell(row, 7).node()).css({'color':'black'});\nvar value=data[8]; $(this.api().cell(row, 8).node()).css({'color':'black'});\nvar value=data[9]; $(this.api().cell(row, 9).node()).css({'color':'black'});\nvar value=data[10]; $(this.api().cell(row, 10).node()).css({'color':'black'});\nvar value=data[11]; $(this.api().cell(row, 11).node()).css({'color':'black'});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':'black'});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'font-weight':'bold','color':value == 'glmnet' ? '#E495A5' : value == 'rpartCost' ? '#ABB065' : value == 'xgbTree' ? '#39BEB1' : value == 'svmLinearWeights2' ? '#ACA4E2' : ''});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.500000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/0.500000 * 100 + '%, #f05028 ' + (1.000000 - value)/0.500000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 1.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (50.000000 - value)/49.000000 * 100 + '%, #8d99ae ' + (50.000000 - value)/49.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'font-weight':'bold','background':isNaN(parseFloat(value)) || value <= 0.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + (1.000000 - value)/1.000000 * 100 + '%, #8d99ae ' + (1.000000 - value)/1.000000 * 100 + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->



<!-- ### Choice of candidate algorithms -->
<!-- In this sensivity analysis, we define the set of inital candidate models differently -->
<!-- for the case $M=100$: -->

<!-- - (uniform) randomly from all 200 available models (same as in main analysis) -->
<!-- - randomly, with probability $p_m$ proportional to $\vatheta_m$, mimicking a good a prior knowledge -->
<!-- which algorithms/hyperparameters are suitable -->
<!-- - randomly, with probability $p_m$ proportional to $1-\vatheta_m$, mimicking bad prior knowledge -->
<!-- - subset the 200 available models to only 2 of the 4 algorithms, this results in 6 total combinations. -->


<!-- ```{r candidate_choice} -->
<!-- NA -->
<!-- ``` -->


### Advantages of the maxT-approach
In this brief analysis, we compare the employed maxT-Test with the better known Bonferroni-correction which is simpler but also more conservative. On the other hand we compare to a "naive" evaluatuion strategy without any statistical testing (i.e. concluding success if the point estimate is above the performance threshold).


```r
data.ext <- read_data(c("MLE_SIM_ACC_FULL", "MLE_SIM_ACC_FULL_EXT")) %>% mutate(M=200) %>% preproc_data()
```

The statistical testing approach does not affect the final model performance but it does affect the rejection rate. We can observe that the Bonferroni approach result in more conservative test decision, i.e. fewer rejections. The naive approach of concluding $\vartheta_{m^*}>\vartheta_0$ whenever $\thetahat_{m^*} > \vartheta_0$ on the other hand results in a way too optimistic approach with type 1 error rates up to 50%.


```r
y.breaks <- seq(0, 1, 0.1)

## overall rejection rate (fpr+tpr):
data.ext$test %>%
  filter(M==200) %>%
  filter(select.rule=="within1SE", n.eval < 1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=0.75) +
  geom_line( aes(shift, rr, color=infer.method), alpha=0.8, cex=0.75) +
  geom_point(aes(shift, rr, color=infer.method), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("Rejection rate: ")* pi(delta)==P(varphi[m*"*"]==1~"|"~delta))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values= info[info$select.rule=="within1SE","pch"], name="Multiple test:  ") +
  scale_color_manual(values= colorspace::rainbow_hcl(length(unique(data.ext$test$infer.method)), 100), name="Multiple test:  ") +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/rr_extended-1.png" style="display: block; margin: auto;" />



### Selection based on model complexity

```r
data.simple <- read_data("MLE_SIM_ACC_M50_GLMNET") %>% preproc_data()
```

There are different model selection approaches which focus on trading of performance versus model complexity, penalizing the latter. In general, it may however be hard to find such a criterion which is applicable to all candidate models at hand when a diverse set of learning algorithms is used.

We performed a small sensitivity analysis in this regard by considering a traditional selection approach for the elastic net (EN) algorithm. Hereby, the number of nonzero model coefficient is minimized (or the L1-penalty maximized) under the side condition that the validation performance is at most one standard errors below the best validation model. The results concerning this *simple* (also referred to as 1-SE rule) rule are given below.

Quite surprisingly, this approach is not even beating the *default* approach, neither regarding  the expected model performance nor the rejection rate of the according statistical test. The bias is not shown here, as this method also enables an unbiased estimation ($|\setS|=1$).


```r
active.simple <- which(info$select.rule %in% unique(data.simple$raw$select.rule))

data.simple$raw  %>%
  filter(n.eval<1000) %>%
  ggplot(aes(select.rule, (final.mod.learn.theta-opt.theta), colour=select.rule)) +
  geom_hline(yintercept=0, cex=1, col="blue") +
  geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(y = bquote(bold("Loss in final model performance: ")*vartheta[m*"*"]-vartheta[max]),
       x = bquote(bold("Selection rule")))+
  scale_color_manual(values = info$col[active.simple], name="Selection rule:  ",
                     labels = paste0(info$select.rule[active.simple], "  ")) +
  owntheme() %+replace%
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/simple_finaltheta-1.png" style="display: block; margin: auto;" />



```r
y.breaks <- seq(0, 1, 0.1)
cols.act.simple <- info$col[active.simple]; names(cols.act) <- info$select.rule[active.simple]
shapes.act.simple <- info$pch[active.simple]; names(shapes.act) <- info$select.rule[active.simple]

## overall rejection rate (fpr+tpr):
data.simple$test %>%
  filter(n.eval<1000) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=0.75) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=0.75) +
  geom_line( aes(shift, rr, color=select.rule), alpha=0.8, cex=0.75) +
  geom_point(aes(shift, rr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex/2*3) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=0.75) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("Rejection rate: ")* pi(delta)==P(varphi[m*"*"]==1~"|"~delta))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act.simple, name="Selection rule:  ",
                     labels = paste0(info$select.rule[active.simple], "  ")) +
  scale_color_manual(values=cols.act.simple, name="Selection rule:  ",
                     labels = paste0(info$select.rule[active.simple], "  ")) +
  owntheme() %+replace%
  theme(axis.text.x = element_text(size=config$text.size/3*2))
```

<img src="C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/simple_rr-1.png" style="display: block; margin: auto;" />


<!-- ### Limiting the number of models? -->
<!-- ```{r finaltheta, dpi=config$fig.dpi, fig.width=config$fig.size, fig.height=config$fig.size/3*2, message=FALSE, cache=FALSE} -->
<!-- active.ext <- which(info$select.rule %in% unique(data.ext$raw$select.rule)) -->

<!-- data.ext$raw %>% -->
<!--   filter(M==200) %>% -->
<!--   filter(infer.method=="maxT") %>% -->
<!--   ggplot(aes(select.rule, (final.mod.learn.theta-opt.theta), colour=select.rule)) + -->
<!--   geom_hline(yintercept=0, cex=1, col="blue") + -->
<!--   geom_boxplot(cex=1.1, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) + -->
<!--   stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") + -->
<!--   facet_grid(n.learn ~ n.eval, -->
<!--              labeller = label_bquote(rows= bold(n[L]==.(n.learn)), -->
<!--                                      cols= bold(n[E]==.(n.eval)))) + -->
<!--   labs(y = bquote(bold("Loss in final model performance: ")*vartheta[m*"*"]-vartheta[max]), -->
<!--        x = bquote(bold("Selection rule")))+  -->
<!--   scale_color_manual(values = info$col[active.ext], name="Selection rule:  ", -->
<!--                      labels = paste0(info$select.rule[active.ext], "  ")) + -->
<!--   owntheme() %+replace%    -->
<!--   theme(axis.ticks.x = element_blank(), -->
<!--         axis.text.x  = element_blank(), -->
<!--         axis.title.x = element_blank())  -->

<!-- ``` -->


<!-- ### Power calculation -->

<!-- ```{r data_power, cache=FALSE}  -->
<!-- data.pow <- read_data('MLE_SIM_POWER_ACC', config$path) -->
<!-- # TODO: set cache to TRUE -->
<!-- ``` -->

<!-- ```{r} -->
<!-- data.pow <- data.pow %>%  -->
<!--   dplyr::select(load.id, M, eval.n, select.method, select.args, select.limit, power.method, correct.method, -->
<!--                 power, delta, theta0, S.ctrl, V, best.val.mod) %>% -->
<!--   mutate(select.args = ifelse(is.na(select.args), "", select.args)) %>% -->
<!--   mutate(select.rule = as.character(interaction(select.method, select.args, select.limit))) %>% -->
<!--   mutate(select.rule = recode(select.rule,  -->
<!--                               oracle.learn.sqrt = "oracle[learn]", -->
<!--                               oracle.train.sqrt = "oracle[train]", -->
<!--                               `se.c=1.sqrt`     = "within1SE", -->
<!--                               `rank.r=1.sqrt`   = "default", -->
<!--                               `simplest.en..sqrt` = "simple_EN", -->
<!--                               `rank.r=100.none` = "S=100")) %>% -->
<!--   mutate(select.rule = ordered(select.rule,  -->
<!--                                levels = info$select.rule)) %>% -->
<!--   select(-select.method, -select.args, -select.limit) %>% -->
<!--   rename(n.eval = eval.n) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- dim(data.pow) -->

<!-- data.power <- right_join(data$raw %>% filter(M==200, red==0, n.eval < 1000) %>% preproc_shift(0.05) %>% rename(delta=shift), data.pow) # TODO: not filter for red==0 -->

<!-- ``` -->
<!-- ```{r} -->
<!-- data.power %>% mutate(test = S == S.ctrl) %>% {mean(.$test)} -->
<!-- names(data.power) -->
<!-- data.power %>% filter(tp, eval.n==400) %>% {summary(.$power)} -->

<!-- xtabs( ~ eval.n + tp, data=data.power %>% filter(select.rule=="within1SE")) -->
<!-- ``` -->


<!-- ```{r power_accuracy} -->
<!-- summary(data.power %>% filter(select.rule=="within1SE") %>% {.$power}) -->
<!-- caret::calibration(as.character(!tp) ~ power, data=data.power, subset=select.rule=="default" & n.eval==200, cuts=10) %>% plot() -->
<!-- caret::calibration(as.character(!tp) ~ power, data=data.power, subset=select.rule=="within1SE" & n.eval==400, cuts=9) %>% plot() -->
<!-- ``` -->


### Optimal number of evaluated models



```r
## read in theory data:
data.theory <- read_theory('MLE_SIM_THEORY_final.theta.mean_BY_S', config$path, 18:65, 5:52)
```

For this simulation study, we prespecified the *within1SE* selection rule as a comparator for the *default* selection rule. However, choosing models within one (and not e.g. two) standard error(s) is not necessarily optimal. In our numerical experiments, the *within1SE* lead to a mean number of evaluated models $S$ of 9 to 10 averaged over all learning tasks (in reality the number is lower due to our self-imposed limit $S \leq \lfloor \sqrt{n_\setE} \rceil$). 

In the following we inspect how the expected model performance changes depending on $S$. Two important conclusions can be drawn from the following figure:

- Given $n_\setE \geq n_\setV = n_\setL/4$ it would be more reasonable to conduct model selection completely on the test data ($S=M$) compared to only on the validation data ($S=1$). This is expected, as the final models (after re-training on the complete learning data) can be only assessed directly only on the test data as the hold-out validation estimate is downward biased by construction.
- However, $S=M$ as well as $S=1$ are far away from being the optimal $S$. We rather observe an optimum around between $S=10$ and $S=15$. The magnitude of the decline in expected model performance after that (i.e. when $S \rightarrow M$) is inversely related to the test set size $n_\setE$.



```r
## read in and prepare data:
data.theory.aggr <- data.theory %>%
  group_by(x, M, n.learn, eval.n) %>%
  summarize(aggr = mean(var)) 

## determine performance for S=1:
tab1 <- data.theory.aggr %>% filter(x==1.5) %>% group_by(M, n.learn) %>% summarize(aa = mean(aggr))

## Final model performance, depending on S (number of evaluated models):
na.omit(data.theory.aggr) %>%
  ggplot() +
  geom_hline(data=tab1, aes(yintercept=aa), lwd=1.2, col="steelblue", lty=2) +
  geom_line(aes(x, aggr, col=factor((eval.n))), lwd=1.2) +
  facet_grid(n.learn ~ M, scales = "free_x",
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(M==.(M)))) +
  labs(y = bquote(bold("Expected final model performance: ")*vartheta[m*"*"]),
       x = bquote(bold("Number of evaluated models: "*S)))+ 
  scale_color_manual(values = colorspace::heat_hcl(4, c(20,50), c(80, 100)),
                     name=bquote(bold(n[E]*"  "))) +
  ylim(0.84, 0.88) + 
  owntheme() %+replace%   
  theme(legend.justification="right",
        legend.box.margin=margin(8,0,0,0)) 
```

![](C:/Users/maxwe/Documents/R/PUBLIC/SEPM.PUB/docs/MLE_SIM_ACC_files/figure-html/theory_S-1.png)<!-- -->

We have also conducted this analysis stratified by learning task and the results did not change qualitively. We remark here that this analysis only covers the aspect of expected model performance. The estimation bias can be expected to increase in $S$ while for the statistical power we expect a similar curve as above (increase at first, followed by a decrease). We expect the power loss to be immense at some point due to necessary adjustment for multiplicity. We have not investiagted these aspects in detail so far, mainly due to the high computational demand (for calculation of critical values for $S>>10$).


---

# Summary

In this simulation study, we examined if the *default* evaluation strategy in machine learning can be improved. Our main finding is that evaluating multiple instead of only a single promising model has several benefits:

- In general, our multiple testing approach leads to a greater flexibility in the evaluation study.
- As more data informs the model selection, the expected final model performance is increased. The magnitude of performance gain is between 0.5% and 1% accuracy in our simulations (when the test data has the same size as the validation data). This effect is increasing in the size of the test set. 
- The probability to successfully find a sufficiently good model increases. This gain in statistical power can be immense, up to 20% in our simulations.

These benefits come at the cost of the biased estimation of the final model performance. Luckily, we are able to correct the upward bias of the naive estimator quite well. The corrected estimate is rather downward biased which is preferable. The other properties (MSE, MAD) of the point estimator however do not differ strongly when a single or multiple models are evaluated. 

In practice, other considerations than raw predictive power may of course also play a role. A resonable strategy would be to check suitable side conditions before the evaluation study. For instance, if a model would not be implemented in pratice because it is implausible or too complex it should not be considered for evaluation in the first place.

Alltogether, evaluating multiple promising models instead of a single candidate seems to be preferable. How to choose the number of models to be evaluated in an optimal way is still an open question. We observed good results with the *within1SE* rule, i.e. when all models within one standard error of the best validation model are evaluated. In the future, we plan to investigate the performance of more elaborate approaches.

---

# Resources

## References
- **Westphal & Brannath (2019, SMMR):** Westphal, M. and Brannath, W. Evaluation of multiple prediction models: a novel view on model selection and performance assessment. Statistical Methods in Medical Research, forthcoming 2019.
- **Westphal & Brannath (2019, ICML):** Westphal, M. and Brannath, W. Improving Model Selection by Employing the Test Data. In Proceedings of the 36th International Conference on Machine
Learning, Long Beach, California, PMLR 97, forthcoming 2019.

## Software

- SEPM (Statistical Evaluation of Prediction Models, R package): https://github.com/maxwestphal/SEPM
- SEPM.MLE (Statistical Evaluation of Prediction Models - Machine Learning and Evaluation, R package): https://github.com/maxwestphal/SEPM.MLE
<!-- - SEPM.SYN (Statistical Evaluation of Prediction Models - Synthetic Data Generation, R package): https://github.com/maxwestphal/SEPM.SYN -->
- SEPM.PUB (Statistical Evaluation of Prediction Models - Simulation Details and Reports, R project): https://github.com/maxwestphal/SEPM.MLE

## Version History

- 13/05/2019 - Version 1: Initial release
- 16/05/2019 - Version 2: Added 'selection on based on model complexity'
