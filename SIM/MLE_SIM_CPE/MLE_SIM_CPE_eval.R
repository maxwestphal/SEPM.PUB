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
#   CPE_SIM_MLE: Simulation study - machine learning and evaluation
#   for co-primary endpoints sensitivity and specificity of binary classifiers
###
#   Script:
#   MLE_SIM_CPE_eval.R: Perform analysis of simulation results and create figures and summary table
###


# Dependencies --------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Load data -----------------------------------------------------------------------------------
## path should correspond to out.path from MLE_SIM_CPE_run.R
path <- "E:\\MLE_SIM\\RESULTS\\MLE_SIM_CPE.csv"

D <- readr::read_csv(path)

dim(D) # should be (480000, 61)


# Preparation ---------------------------------------------------------------------------------

## ggplot theme:
owntheme <- function(){
  theme(axis.text    = element_text(size=config$text.size),
        axis.title   = element_text(size=config$title.size,face="bold"),
        legend.text  = element_text(size=config$title.size),
        legend.title = element_text(size=config$title.size, face="bold"),
        strip.text.x = element_text(size=config$title.size, face="bold"),
        strip.text.y = element_text(size=config$title.size, face="bold", angle=90),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'),
        legend.background = element_rect(fill="white", size=0.75, linetype="solid", colour ="black"))}

## plotting options:
config <- list()
config$fig.dpi <- 600
config$fig.size <- 12
config$big.cex <- 5
config$small.cex <- 2
config$title.size <- 36
config$text.size <- 36

## colors:
cols <- scales::hue_pal()(25) # scales::show_col(scales::hue_pal()(25))

## info:
info <- rbind(c("oracle", "truly best model [CPE]", cols[1]),
              c("default", "empirically best validation model [CPE]", cols[3]),
              c("within1SE", "all models within 1 standard error of best validation model [CPE]", cols[9]), 
              c("optimalEFP", "optimized expected final performance (EFP) [CPE]", cols[17])) %>%
  as.data.frame() %>%
  mutate_all(as.character)
names(info) <- c("select.rule", "description", "col")
info$pch <- c(17, 16, 18, 15)

## description of learning tasks:
tasks <- data.frame(score  = LETTERS[1:6], 
                    score.type = c(rep("linear",2), rep("nonlinear", 4)),
                    features.tot = rep(50, 6),
                    features.act = c(5,5,5,5,9,9),
                    prev = c(0.5, 0.5, 0.3,0.15,0.3,0.15),
                    theta.opt = c(0.885, 0.860, 0.951, 0.966, 0.950, 0.963))

## data processing:
D <- D %>%
  select(- 'shift') %>% 
  mutate(select.rule = recode(select.method,
                              `best` ="default",
                              `close` ="within1SE",
                              `optimal` ="optimalEFP",
                              `oracle` ="oracle")) %>%
  mutate(select.rule = ordered(select.rule, 
                               levels = info$select.rule)) %>%
  mutate(score = recode(scenario,
                        EOMPM_A2 = "A",
                        EOMPM_B2 = "B",
                        MLE_SIM_F1_prev30 = "C",
                        MLE_SIM_F1_prev15 = "D",
                        MLE_SIM_F13_prev30 = "E",
                        MLE_SIM_F13_prev15 = "F")) %>%
  mutate(prev = as.numeric(recode(score, A="0.5", B="0.5", C="0.3", D="0.15", E="0.3", F="0.15")),
         redundancy = recode(red,
                             `0` = "[I]",
                             `1` = "[R]")) %>% 
  mutate(scenario = paste0(score, redundancy)) %>%
  mutate(se.abs.dev = final.se.hat-final.learn.se,
         se.rel.dev = se.abs.dev/final.learn.se,
         se.abs.dev.c = final.se.tilde-final.learn.se,
         se.rel.dev.c = se.abs.dev.c/final.learn.se,
         sp.abs.dev = final.sp.hat-final.learn.sp,
         sp.rel.dev = sp.abs.dev/final.learn.sp,
         sp.abs.dev.c = final.sp.tilde-final.learn.sp,
         sp.rel.dev.c = sp.abs.dev.c/final.learn.sp,
         maxS.sqrt = S == round(sqrt(n.eval))) %>%
  mutate(opt.theta = pmin(opt.se.cpe, opt.sp.cpe),
         reject00 = final.se.lower > opt.theta - 0.00 & final.sp.lower > opt.theta - 0.00,
         reject05 = final.se.lower > opt.theta - 0.05 & final.sp.lower > opt.theta - 0.05,
         reject10 = final.se.lower > opt.theta - 0.10 & final.sp.lower > opt.theta - 0.10,
         final.theta = final.learn.theta,
         final.theta.hat = pmin(final.se.hat, final.sp.hat),
         final.theta.tilde = pmin(final.se.tilde, final.sp.tilde),
         final.fwer = final.se.lower > final.theta & final.sp.lower > final.theta,
         final.se.75 = final.learn.se > 0.75,
         final.sp.75 = final.learn.sp > 0.75,
         final.theta.75 = final.se.75 & final.sp.75,
         dev.se = final.se.tilde - final.learn.se,
         dev.sp = final.se.tilde - final.learn.sp,
         theta.dev.hat = final.theta.hat - final.theta,
         theta.dev.tilde = final.theta.tilde - final.theta,
         abs.dev.cpe = abs(dev.se)/2 + abs(dev.sp)/2,
         abs.dev.cpe.naive = abs(final.se.hat - final.learn.se)/2 + abs(final.sp.hat - final.learn.sp)/2,
         oe.se = dev.se > 0,
         oe.sp = dev.sp > 0,
         oe1 = oe.se | oe.sp,
         oe2 = oe.se & oe.sp,
         oe.cpe.naive = final.se.hat > final.learn.se & final.sp.hat > final.learn.sp) %>% 
  tidyr::unite(eval.strat, select.rule, infer.method, sep="|", remove=FALSE)

## overview:
info

tasks

dim(D)


# Problem visualization -----------------------------------------------------------------------

## oplus: symbol("Å"); otimes: symbol("Ä")

png("MLE_SIM_CPE_EDA1.png", width=5000, height= 5000, res=600)
D %>%
  filter(select.rule == "oracle", n.eval==400) %>%
  ggplot(aes(factor(prev), pmin(opt.se.cpe, opt.sp.cpe), col=factor(n.learn))) +
  geom_boxplot(cex=1.5) +
  ylab(bquote(vartheta[symbol("Å")] == ~ "min("* Se[symbol("Å")]*","~ Sp[symbol("Å")]* ")")) +
  scale_x_discrete(labels=c("0.15"=bquote(rho==0.15),
                            "0.3"= bquote(rho==0.30),
                            "0.5"= bquote(rho==0.50))) + 
  scale_color_manual(values=rev(c('#41b6c4', '#253494'))) + 
  labs(color = bquote(n[L]~"")) +
  owntheme() %+replace% 
  theme(axis.title.x = element_blank())
dev.off()

png("MLE_SIM_CPE_EDA2.png", width=5000, height= 5000, res=600)
D %>%
  filter(select.rule == "oracle", n.eval==400)  %>%
  ggplot(aes(factor(prev), opt.se.cpe-opt.sp.cpe, col=factor(n.learn))) +
  geom_boxplot(cex=1.5) +
  ylab(bquote(Delta[symbol("Å")]==Se[symbol("Å")]-Sp[symbol("Å")])) +
  scale_x_discrete(labels=c("0.15"=bquote(rho==0.15),
                            "0.3"= bquote(rho==0.30),
                            "0.5"= bquote(rho==0.50))) +
  scale_color_manual(values=rev(c('#41b6c4', '#253494'))) + 
  labs(color = bquote(n[L]~"")) +
  owntheme() %+replace% 
  theme(axis.title.x = element_blank())
dev.off()


# Figure: final performance -------------------------------------------------------------------
nl <- c(400, 800)
ne <-  c(400, 800)

eps <- seq(0, 0.15, 0.015) 
delta <- 0

cu <- data.table::CJ(job.id=unique(D$job.id), shift=eps)
subs <- dplyr::inner_join(D, cu) %>%
  dplyr::mutate(succ = final.learn.se >= opt.se.cpe - shift & 
                  final.learn.sp >= opt.sp.cpe - shift + delta) %>%
  dplyr::group_by(shift, select.rule, n.learn, n.eval) %>%
  dplyr::summarize(sr=mean(succ), N=n())


png("MLE_SIM_CPE_FP.png", width=10000, height= 7500, res=600)
subs %>% 
  ggplot(aes(x=shift, y=sr, col=select.rule)) +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, lwd=1.5) +
  geom_line(lwd=2, alpha=0.8) + 
  geom_point(aes(x=shift, y=sr, col=select.rule, pch=select.rule), alpha=0.8, cex=6) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) + 
  scale_color_manual(values=info$col, name="Selection rule:  ",
                     labels=paste0(info$select.rule, "  ")) +
  scale_shape_manual(values=info$pch, name="Selection rule:  ",
                     labels=paste0(info$select.rule, "  ")) +
  guides(col = guide_legend(nrow=1, byrow=T)) +
  scale_x_continuous(name=bquote(delta == vartheta[symbol("Å")] - vartheta[0]),
                     breaks=c(0, 0.05, 0.1, 0.15, 0.2),
                     labels=c("0","0.05", "0.10", "0.15", "0.2"), limits=c(-0.005,0.15)) + 
  scale_y_continuous(name=bquote(bold("Final performance: ")*P(vartheta["m*"]>= vartheta[0]~"|"~delta)),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels=c("0.0","0.2", "0.4", "0.6", "0.8", "1.0"), limits=0:1) + 
  owntheme() %+replace% 
  theme(strip.text.x = element_text(size=40, face="bold"),
        strip.text.y = element_text(size=40, face="bold", angle=90),
        axis.text.x = element_text(size=config$text.size),
        axis.title   = element_text(size=40, face="bold")) 
dev.off()



# Figure: rejection rate ----------------------------------------------------------------------
y.breaks <- seq(0, 1, 0.20)

testd <- D %>% 
  SEPM.SIM::postproc_mle(analysis = "cpe",
                         vals = seq(-0.015, 0.15, 0.015)) %>% 
  group_by(n.learn, n.eval, select.rule, shift) %>% 
  summarize(rr = mean(reject))

png("MLE_SIM_CPE_RR.png", width=10000, height= 7500, res=600)
testd %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, lwd=1.5) +
  geom_vline(xintercept = 0,   col="black", lty=2, lwd=1.5) +
  geom_line( aes(shift, rr, color=select.rule), alpha=0.8, lwd=2) +
  geom_point(aes(shift, rr, color=select.rule, pch=select.rule), alpha=0.8, cex=6) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, lwd=1.5) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n[L]==.(n.learn)),
                                     cols= bold(n[E]==.(n.eval)))) +
  labs(x = bquote(delta == vartheta[symbol("Å")] - vartheta[0]),
       y = bquote(bold("Rejection rate: ")* rr(delta)==P(varphi["m*"]==1~"|"~delta))) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=info$pch, name="Selection rule:  ",
                     labels=paste0(info$select.rule, "  ")) +
  scale_color_manual(values=info$col, name="Selection rule:  ",
                     labels=paste0(info$select.rule, "  ")) +
  guides(shape = guide_legend(nrow=1, byrow=T)) +
  scale_x_continuous(name=bquote(delta == vartheta[symbol("Å")] - vartheta[0]),
                     breaks=c(0, 0.05, 0.1, 0.15, 0.2),
                     labels=c("0","0.05", "0.10", "0.15", "0.2"), limits=range(testd$shift)) + 
  owntheme() %+replace%
  theme(strip.text.x = element_text(size=40, face="bold"),
        strip.text.y = element_text(size=40, face="bold", angle=90),
        axis.text.x = element_text(size=config$text.size), 
        axis.title   = element_text(size=40, face="bold")) 
dev.off()


# Extended results table ----------------------------------------------------------------------
SEPM.SIM::summarize_results(data = D,
                            vars = c("final.theta", "final.theta.75", "reject10", "reject05", "reject00", 
                                     "final.fwer", "theta.dev.tilde", "abs.dev.cpe", "oe1", "oe2", "S"),
                            rules = info$select.rule,
                            comp = c(4, 3),
                            NL = list(c(400, 800)),
                            NE = list(400, 800))

