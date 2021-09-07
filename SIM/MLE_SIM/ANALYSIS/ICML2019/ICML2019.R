### ICML 19 POSTER figures

### ICML 19 figure 1:
config2 <- config
config$title.size = 32; config$text.size = 28; config$big.cex = 9; config$small.cex = 3
png("finaltheta_paired-2.png", width=2880, height= 960, res=240) # 1440
data$paired %>%
  filter(M==200, n.eval < 1000) %>%
  ggplot(aes(as.factor(n.eval), (within1SE-default) ), colour="darkgrey") +
  geom_hline(yintercept = 0, cex=1.5, color="blue") +
  geom_boxplot(cex=1.5, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  ylim(-0.1, 0.3) +
  facet_grid(. ~ n.learn,
             labeller = label_bquote(cols = bold(n["learn"]==.(n.learn)),
                                     rows = bold(M==.(M)))) +
  labs(y = bquote(bold("Performance gain")),
       x = bquote(bold(n["test"]))) +
  owntheme() %+replace%
  theme()
dev.off()
config = config2


### ICML 19 figure 2:
config2 <- config
config$title.size = 32; config$text.size = 28; config$big.cex = 9; config$small.cex = 3

data1n <- data$raw %>% filter(M==200) %>% mutate(type="naive")
data1c <- data1n %>% mutate(rel.dev = rel.dev.c, type="corrected")
mean(data1n$job.id == data1c$job.id)
data2 <- rbind(data1n, data1c) %>% mutate(type = factor(type, levels=c("naive", "corrected")))

png("bias_comparison.png", width=2880, height= 1440, res=240)
data2 %>%
  filter(select.rule %in% config$bias.methods) %>%
  filter(n.eval <= 800, n.eval>100, n.learn==400) %>%
  ggplot(aes(select.rule, rel.dev, colour=select.rule)) +
  geom_hline(yintercept=0, cex=1.5, col="blue") +
  geom_boxplot(cex=1.5, notch=TRUE, notchwidth=0.75, alpha=0.33, outlier.shape = 18, outlier.size=config$small.cex) +
  ylim(-.125, .125) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=config$big.cex, col="red") +
  facet_grid(type ~ n.eval,
             labeller = label_bquote(rows= bold(.(as.character(type))),
                                     cols= bold(n["test"]==.(n.eval)))) +
  labs(y = bquote(bold("Estimation bias")))+
    scale_color_manual(values = info$col[bias.active], name="Selection rule:  ",
                     labels = paste0(info$select.rule[bias.active], "  ")) +
  owntheme() %+replace%
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        legend.text  = element_text(size=36),
        legend.title = element_text(size=36, face="bold"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'))
dev.off()

config <- config2



### ICML 19 figure 3:
config2 <- config
config$title.size = 32; config$text.size = 24; config$big.cex = 9; config$small.cex = 3


y.breaks <- seq(0,1,0.2)
png("rr-2.png", width=2880, height= 1920, res=240)
data$test %>%
  filter(M==200, n.eval<=800, n.eval>100, select.rule %in% c("within1SE", "default")) %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1),    col=c("darkgrey"), lty=1, cex=1.5) +
  geom_vline(xintercept = 0,   col="black", lty=2, cex=1.5) +
  geom_line( aes(shift, rr, color=select.rule), alpha=0.8, cex=1.5) +
  geom_point(aes(shift, rr, color=select.rule, pch=select.rule), alpha=0.8, cex=config$small.cex*2) +
  geom_hline(yintercept = c(0.025), col=c("darkred"), lty=2, cex=1.5) +
  facet_grid(n.learn ~ n.eval,
             labeller = label_bquote(rows= bold(n["learn"]==.(n.learn)),
                                     cols= bold(n["test"]==.(n.eval)))) +
  labs(#x = bquote(delta == vartheta[max] - vartheta[0]),
       y = bquote(bold("False & true positive test decisions "))) +
  scale_x_continuous(breaks=c(-0.03, 0.03), labels = c("FPR", "TPR")) +
  scale_y_continuous(limits=range(y.breaks), breaks=y.breaks) +
  scale_shape_manual(values=shapes.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active[3:4]], "  ")) +
  scale_color_manual(values=cols.act, name="Selection rule:  ",
                     labels=paste0(info$select.rule[active[3:4]], "  ")) +
  owntheme() %+replace%
  theme(#axis.text.x = element_text(size=config$text.size, angle=45, vjust=0.85, hjust=0.85),
        axis.title.x = element_blank(),
        legend.text  = element_text(size=36),
        legend.title = element_text(size=36, face="bold"))#,legend.position="none")
dev.off()

config <- config2
