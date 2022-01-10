###
#   Author: 
#   Max Westphal (mwestphal@uni-bremen.de)
###
#   Date:
#   2020-03-20
###
#   Project:
#   SIMPle: Simultaneous Inference for Multiple Proportions
###
#   Task: 
#   MBB_SIM_ACC: Simulation study - assessment of credible regions based on 
#   multivariate beta-binomial model
###
#   Script:
#   MBB_SIM_ACC_eval.R: Perform analysis of simulation results and create figures 
###



# Dependencies --------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Data preparation ----------------------------------------------------------------------------
## path should correspond to out.path from MBB_SIM_ACC_run.R
path <- "E:\\MBB_SIM\\RESULTS\\MBB_SIM_ACC.csv"

D <- readr::read_csv(path)
names(D)

D <- D %>% 
  mutate(m = 5*as.numeric(substr(gen_prior, nchar(gen_prior), nchar(gen_prior)))) %>%
  mutate(nu  = as.numeric(substr(gen_prior, 3, 4))) %>%
  mutate(rho = as.numeric(substr(gen_prior, 9, 10))/100) %>% 
  mutate(method = recode(method, sample="mcmc")) %>% 
  mutate(np = n + 5*(-(method=="approx")+(method=="mcmc"))) 


# ggplot configuration ------------------------------------------------------------------------
small.size <- 30
large.size <- 40
point.size <- 10

symbols <- c(15, 19, 18)
linetypes <- c(1, 5, 2)
#colours <- colorRamps::matlab.like2(5)[c(5,3,1)]
colours <- c('#41b6c4','#2c7fb8','#253494')
labels <- c("approximate ","copula ","extensive ")

owntheme <- function(){
  theme_grey() +
    theme(axis.text.y=element_text(size=small.size),
          axis.text.x=element_text(size=small.size, angle=0),
          axis.title.y=element_text(size=large.size,face="bold"),
          axis.title.x=element_text(size=large.size,face="bold", vjust=-0.5),
          legend.text = element_text(size=large.size),
          legend.title = element_text(size=large.size, face="bold"),
          strip.text.x = element_text(size=large.size, face="bold"),
          strip.text.y = element_text(size=large.size, face="bold", angle=90),
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"),
          legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
          legend.spacing.x = unit(0.75, 'char'))
}



# (1) Inference for raw proportions -----------------------------------------------------------
lims <- c(0.93, 0.97)
A <- D %>% 
  filter(contrast == "raw") %>% 
  group_by(n, method, prior, m, np) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered)) 
head(A)

png("figure2.png", width=7500, height= 5000, res=360, pointsize=large.size)
A %>%
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims) + 
  facet_grid(prior ~ m, scales = "free_x",
             labeller = label_bquote(cols= bold(m==.(m)),
                                     rows= bold(.(prior) ~ "prior"))) +
  labs(y = bquote(bold("Bayes coverage probability")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks = c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  owntheme()
dev.off()


# (2) Inference for raw proportions (stratified, correct prior only) --------------------------
lims <- c(0.92, 0.98)
A <- D %>%
  filter(contrast == "raw") %>% 
  filter(prior == "correct") %>% 
  group_by(m, gen_prior, nu, rho, n, np, prior, method) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered))
head(A)

png("figureA2c.png", width=7500, height= 10000, res=360, pointsize=large.size)
A %>%
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims)  +
  facet_grid(nu+rho ~ m, scales = "free_x",
             labeller = label_bquote(rows= bold(nu==.(nu)* ",   "~ rho == .(rho) ),
                                     cols= bold(m==.(m)))) +
  labs(y = bquote(bold("Bayes coverage probability (correct prior)")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks=c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  owntheme()
dev.off()


# (3) Inference for raw proportions (stratified, vague prior only) ----------------------------
lims <- c(0.92, 0.98)
A <- D %>%
  filter(contrast == "raw") %>% 
  filter(prior == "vague") %>% 
  group_by(m, gen_prior, nu, rho, n, np, prior, method) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered))
head(A)


png("figureA2v.png", width=7500, height= 10000, res=360, pointsize=large.size)
A %>%
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims)  +
  facet_grid(nu+rho ~ m, scales = "free_x",
             labeller = label_bquote(rows= bold(nu==.(nu)* ",   "~ rho == .(rho) ),
                                     cols= bold(m==.(m)))) +
  labs(y = bquote(bold("Bayes coverage probability (vague prior)")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks=c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  theme_grey()+
  theme(axis.text.y=element_text(size=small.size),
        axis.text.x=element_text(size=small.size, angle=0),
        axis.title.y=element_text(size=large.size,face="bold"),
        axis.title.x=element_text(size=large.size,face="bold", vjust=-0.5),
        legend.text = element_text(size=large.size),
        legend.title = element_text(size=large.size, face="bold"),
        strip.text.x = element_text(size=large.size, face="bold"),
        strip.text.y = element_text(size=large.size, face="bold", angle=90),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.75, 'char'))
dev.off()


# (4) Inference for difference of proportions -------------------------------------------------
lims <- c(0.93, 0.97)
A <- D %>% 
  filter(contrast == "rdm") %>% 
  group_by(n, method, prior, m, np) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered)) 
head(A)

png("figure3.png", width=7500, height= 5000, res=360, pointsize=large.size)
A %>%
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims) + 
  facet_grid(prior ~ m, scales = "free_x",
             labeller = label_bquote(cols= bold(m==.(m)),
                                     rows= bold(.(prior) ~ "prior"))) +
  labs(y = bquote(bold("Bayes coverage probability")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks=c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  owntheme()
dev.off()


# (5) Inference for difference of proportions (stratified, correct prior only) ----------------
lims <- c(0.92, 0.98)
A <- D %>%
  filter(contrast == "rdm") %>% 
  filter(prior == "correct") %>% 
  group_by(m, gen_prior, nu, rho, n, np, prior, method) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered))
head(A)


png("figureA3c.png", width=7500, height= 10000, res=360, pointsize=large.size)
A %>%
  filter(prior=="correct") %>% 
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims)  +
  facet_grid(nu+rho ~ m, scales = "free_x",
             labeller = label_bquote(rows= bold(nu==.(nu)* ",   "~ rho == .(rho) ),
                                     cols= bold(m==.(m)))) +
  labs(y = bquote(bold("Bayes coverage probability (correct prior)")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks=c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  owntheme()
dev.off()


# (6) Inference for difference of proportions (stratified, vague prior only) ------------------
lims <- c(0.92, 0.98)
A <- D %>%
  filter(contrast == "rdm") %>% 
  filter(prior == "vague") %>% 
  group_by(m, gen_prior, nu, rho, n, np, prior, method) %>% 
  summarize(nsim = n(),
            adm_rate = mean(adm_lower & adm_upper & adm_diff),
            vol_mean = mean(volume),
            coverage = mean(all_covered))
head(A)

png("figureA3v.png", width=7500, height= 10000, res=360, pointsize=large.size)
A %>%
  filter(prior=="vague") %>% 
  ggplot() +
  geom_hline(yintercept=0.95, lty=3, lwd=3, color="red") +
  geom_line( aes(n, coverage, color=(method), lty=method), lwd=2) + 
  geom_point(aes(np, coverage, color=(method), pch=method), cex=point.size) + 
  scale_y_continuous(breaks = seq(lims[1], lims[2], 0.01), limits=lims)  +
  facet_grid(nu+rho ~ m, scales = "free_x",
             labeller = label_bquote(rows= bold(nu==.(nu)* ",   "~ rho == .(rho) ),
                                     cols= bold(m==.(m)))) +
  labs(y = bquote(bold("Bayes coverage probability (vague prior)")),
       x = bquote(bold("sample size"))) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1), shape=symbols))) +
  scale_shape_manual(guide = "none", values=symbols)+
  scale_linetype_manual(guide = "none", values=linetypes) +
  scale_color_manual("method:", 
                     breaks=c("approx", "copula", "mcmc"),
                     labels = labels,
                     values = colours) + 
  owntheme()
dev.off()



  
  
  