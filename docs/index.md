---
title:  |
  | Statistical Evaluation of Prediction Models:
  | Simulation Details and Reports
  | (SEPM.PUB)
author: 
- Max Westphal <br> mwestphal@uni-bremen.de <br> https://github.com/maxwestphal <br> https://www.linkedin.com/in/maxwestphal/ <br> https://www.anstat.uni-bremen.de/node/22
date: "07 June, 2019"
knit: (function(inputFile, encoding) { 
      out.dir <- file.path(dirname(inputFile), 'docs');
      dir.create(out.dir);
      rmarkdown::render(inputFile,
                        encoding=encoding, 
                        output_file=file.path(out.dir, 'index.html')) })
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

This R project (SEPM.PUB) contains a collection of research results in the context of model evaluation for machine-learned prediction models. 

# Research Projects

## **MLE_SIM**: Machine learning and evaluation

This project includes a wide variety of functions for generation of artifical data consisting of features and labels and additional functions to carry out the complete machine learning pipeline:

(a) **Training:** Prediction models are trained by different machine learning algorithms.
(b) **Validation:** A single or multiple models are choosen for an evaluation study.
(c) **Test:** The selected models are evaluated for a sufficently high performance. 

The main novelty of our investigation is that the evaluation study is not restricted to only be conducted for a single (prespecified) model which is the usual recommendation in the machine learning literature. This idea was introduced by **Westphal & Brannath (2019, SMMR)**. As a consequence, statistical methods are needed to adjust for the introduced overoptimism due to the overlap of model selection and evaluation (based on the test data). Several simulation studies have been conducted to assess the benefits and drawbacks of this approach, see **Westphal & Brannath (2019, ICML)**. Detailed reports are provided below:

- [MLE_SIM_ACC](https://maxwestphal.github.io/SEPM.PUB/MLE_SIM_ACC.html): Extensive simulation study concerning binary classification accuracy 
- **MLE_SIM_CPE**: Smaller study concerning co-primary endpoint analysis of sensitivity and specificity (forthcoming)

## **SYN_SIM:** Synthetic Data Scenarios

Forthcoming...

## **RDX**: Real Data Examples

The two available real data examples are described in **Westphal & Brannath (2019, SMMR)** and are currently only available as part of the older **EOMPM** project (see below). At some point these will be recompiled for easier access. 

## **EOMPM**: Evaluation of multiple prediction models

Numerical experiments related to the publication **Westphal & Brannath (2019, SMMR)**, described in detail at https://github.com/maxwestphal/EOMPM.



# R Packages

## SEPM: Statistical Evaluation of Prediction Models

This R package implements a convinient pipeline of hypothesis formulation and testing in the context of model evaluation and comparison. The development version is available at https://github.com/maxwestphal/SEPM.

## SEPM.MLE: Machine Learning and Evaluation

This R package contains all function for the simulations in the machine learning context (MLE_SIM) and is available at https://github.com/maxwestphal/SEPM.MLE.

## SEPM.SYN: Synthetic Data Generation

The main purpose of this package is the investigation of finite sample properties of different selection approaches and multiple testing procedures in model or biomarker evaluation studies.
To this end, several functions for the generation of synthetic binary data in various scenarios are provided: 

- Least favorable parameter configurations: assessment of frequentist (minimax) properties.
- Bayesian framework: assessment of Bayes risk, e.g. expected risk under different prior distributions.
- Biomarker setting: Sampling under the simple binormal model to emulate the optimal cutpoint problem.

## SIMPle: Simultaneous Inference of Multiple Proportions

Forthcoming...

# References 

- **Westphal & Brannath (2019, SMMR):** Westphal, M. and Brannath, W. Evaluation of multiple prediction models: a novel view on model selection and performance assessment. Statistical Methods in Medical Research, forthcoming 2019.
- [**Westphal & Brannath (2019, ICML):**](http://proceedings.mlr.press/v97/westphal19a.html) Westphal, M. and Brannath, W. Improving Model Selection by Employing the Test Data. In Proceedings of the 36th International Conference on Machine
Learning, Long Beach, California, PMLR 97, 2019. 
