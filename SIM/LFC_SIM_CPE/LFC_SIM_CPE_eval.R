###
#   Author: 
#   Max Westphal (mwestphal@uni-bremen.de)
###
#   Date:
#   2021-12-12
###
#   Project:
#   CPE: A multiple testing framework for diagnostic accuracy studies with co-primary endpoints
###
#   Task: 
#   LFC_SIM_MLE: Simulation study - least favorable parameters for assessment of
#   co-primary endpoints sensitivity and specificity of binary classifiers
###
#   Script:
#   LFC_SIM_CPE_eval.R: Perform analysis of simulation results and create figures and summary table
###


# Dependencies --------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Load data -----------------------------------------------------------------------------------
## path should correspond to out.path from MLE_SIM_CPE_run.R
path <- "E:\\LFC_SIM\\RESULTS\\LFC_SIM_CPE.csv"

D <- readr::read_csv(path)

dim(D) 

# Preparation ---------------------------------------------------------------------------------
A <- D %>%
  group_by(algorithm, prev, n, S, se, sp, eps, corr.se, corr.sp) %>%
  summarize(Nsim = n(),
            fwer = mean(reject)) %>%
  tidyr::unite(pars, se, sp) %>%
  mutate(p = as.numeric(substr(pars, 1,3))) %>%
  mutate(rho = as.factor(prev))

dim(A)

# Visualization -------------------------------------------------------------------------------
config <- list()
config$title.size <- 24
config$text.size <- 20


owntheme <- function(x=NULL, pars=config){
  theme(axis.text    = element_text(size=pars$text.size),
        axis.title   = element_text(size=pars$title.size,face="bold"),
        legend.text  = element_text(size=pars$title.size),
        legend.title = element_text(size=pars$title.size, face="bold"),
        strip.text.x = element_text(size=pars$title.size, face="bold"),
        strip.text.y = element_text(size=pars$title.size, face="bold", angle=90),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "horizontal",
        legend.spacing.x = unit(0.5, 'char'),
        legend.background = element_rect(fill="white", size=0.75, linetype="solid", colour="black"))}

png("LFC_SIM_CPE.png", width=6000, height= 4000, res=600)
A %>%
  filter() %>%
  ggplot() +
  xlim(5, 10) +
  ylim(c(0,0.15)) +
  geom_hline(yintercept=0.025, lwd=1.25, lty=2, col="darkred") +
  geom_hline(yintercept=0.0, lwd=1.25, lty=1, col="darkgrey") +
  geom_line(aes(log(n), fwer, col=factor(eps), lty=as.factor(prev)), lwd=1.5) +
  facet_grid(p ~ S, 
             labeller = label_bquote(rows=Se[0]~"="~Sp[0]~"="~.(as.character(p)),
                                     cols=S~"="~.(as.character(S)))) +
  labs(y = bquote(bold("FWER")),
       x = bquote(bold("log(n)"))) +
  scale_color_manual(values = rev(c('#41b6c4','#2c7fb8','#253494')),
                     labels = c("0 (LFC)", "0.001", "0.002"),
                     name=bquote(bold(" " ~ epsilon ~ " "))) +
  scale_linetype_manual(values=c(1,3)) +
  guides(lty=guide_legend(title=bquote(bold(" " ~ rho ~ " ")))) +
  owntheme() %+replace%   
  theme()
dev.off()

