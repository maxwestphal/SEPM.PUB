---
title:  | 
  | Statistical Evaluation of Prediction Models:
  | Simulation Details and Reports
  | (SEPM.PUB)
pagetitle: SEPM.PUB
author: 
- Max Westphal <br> mwestphal@uni-bremen.de <br> https://github.com/maxwestphal <br> https://www.linkedin.com/in/maxwestphal/ <br> https://www.anstat.uni-bremen.de/node/22
date: "28 July, 2019"
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

## **EOMPM**: Evaluation of Multiple Prediction Models

Numerical experiments related to the publication **Westphal & Brannath (2019, SMMR)**. A detailed description is provided at https://github.com/maxwestphal/EOMPM.

## **IMS**: Improving Model Selection

In this project, the complete machine learning and evluation pipeline is emulated:

(a) **Training:** Prediction models are trained by different machine learning algorithms.
(b) **Validation:** A single or multiple models are choosen for an evaluation study.
(c) **Test:** The selected models are evaluated for a sufficently high performance. 

The main novelty of our investigation is that the evaluation study is not restricted to only be conducted for a single (prespecified) model which is the usual recommendation in the machine learning literature. This idea was introduced by **Westphal & Brannath (2019, SMMR)**. As a consequence, statistical methods are needed to adjust for the introduced overoptimism due to the overlap of model selection and evaluation (based on the test data). Several simulation studies have been conducted to assess the benefits and drawbacks of this approach, see **Westphal & Brannath (2019, ICML)**. 

- [MLE_SIM_ACC](https://maxwestphal.github.io/SEPM.PUB/MLE_SIM_ACC.html): Extensive simulation study concerning the assessment of the overall accuracy of binary classifiers


## **SIMPle**: Simultaneous Inference for Multiple Proportions

Numerical experiments related to the publication **Westphal (2019, forthcoming)**: Assessment of coverage probability of multivariate Beta-Binomial credible regions under different prior distributions.

Forthcoming...


## **CPE**: Co-Primary Endpoint Analysis

This is a follow-up of the **IMS** project. Again, we investiagte how to improve model evaluation studies in the supervised machine learning context. Instead of looking at the overall accuracy of binary classifiers, we now focus on the so-called co-primary endpoint analysis. Hereby, sensitivity and specificity are both assessed simultaneously.

- **MLE_SIM_CPE**: Simulation study concerning co-primary endpoint analysis of sensitivity and specificity (forthcoming)


# Real Data Examples

So far, two real data examples are available, both described in **Westphal & Brannath (2019, SMMR)**. They are currently only available as part of the older [**EOMPM**](https://github.com/maxwestphal/EOMPM) project. At some point these will be recompiled for easier access... 


# R Packages

## SEPM: Statistical Evaluation of Prediction Models

The [SEPM](https://github.com/maxwestphal/SEPM) package implements a convinient pipeline of hypothesis formulation and testing for the purpose of model evaluation and comparison studies. 

NOTE: This package is still under development - no warranty!

## SIMPle: Simultaneous Inference for Multiple Proportions

The [SIMPle](https://github.com/maxwestphal/SIMPle) package allows the specification and data based update of a multivariate Beta (mBeta) model as described by **Westphal (2019, forthcoming)**

NOTE: This package is still under development - no warranty!

## DGN: Data Generation

Many custom functions for the generation of (sythetic) binary data have been developed. Currently they are availabe in the packages listed below, with minimal documentation. Our main goal is to generate data in order to investigate finite sample properties of different selection approaches and multiple testing procedures in model or biomarker evaluation studies as described above.

### SEPM.MLE: Machine Learning and Evaluation

This [SEPM.MLE](https://github.com/maxwestphal/SEPM.MLE) package contains all functions for the simulations in the machine learning context, i.e. for generation of feature-label data, hyperparameter generation and model training.

### SEPM.SYN: Synthetic Data

The [SEPM.SYN](https://github.com/maxwestphal/SEPM.SYN) package allows sampling from different generative mBeta distributions for an assessment of the Bayes risk, i.e. the expected risk under different priors.

### SEPM.LFC: Least Favorable Parameter Configurations

The [SEPM.LFC](https://github.com/maxwestphal/SEPM.LFC) package allows sampling from least favorable parameter configuration for an assessment of frequentist (minimax) properties.

### SEPM.BIO: Biomarker Data

Goal: Sampling under the simplistic binormal model to emulate the optimal cutpoint problem. Forthcoming...



# References 

## Scientific Publications

- **Westphal & Brannath (2019, SMMR):** Westphal, M. and Brannath, W. Evaluation of multiple prediction models: a novel view on model selection and performance assessment. Statistical Methods in Medical Research, forthcoming 2019.
- [**Westphal & Brannath (2019, ICML):**](http://proceedings.mlr.press/v97/westphal19a.html) Westphal, M. and Brannath, W. Improving Model Selection by Employing the Test Data. In Proceedings of the 36th International Conference on Machine
Learning, Long Beach, California, PMLR 97, 2019.
- **Westphal (2019, forthcoming):** Simultaneous Inference for Multiple Proportions: A Multivariate Beta-Binomial Model. Submitted for Publication, 2019.


