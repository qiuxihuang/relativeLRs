library(ggplot2)
library(grid)
library(RColorBrewer)
setwd("...") # set your working directory

# size, power, penalized power, adjusted power, positive and negative likelihood ratios
# read-in results
plotbase <- read.csv("simulations/plot_lrs_penpow_adjpow.csv",header=TRUE, sep=",")
base0 <- plotbase[plotbase$rho==0,]
base06 <- plotbase[plotbase$rho==-0.6,]
base1 <- plotbase[plotbase$rho==-1,]

base0$true_OR<-factor(base0$true_OR)
levels(base0$true_OR)[levels(base0$true_OR)=="0.25"] <- expression(OR*"="*0.25)
levels(base0$true_OR)[levels(base0$true_OR)=="0.5"] <- expression(OR*"="*0.5)
levels(base0$true_OR)[levels(base0$true_OR)=="0.67"] <- expression(OR*"="*0.67)
levels(base0$true_OR)[levels(base0$true_OR)=="1"] <- expression(OR*"="*1)
base06$true_OR<-factor(base06$true_OR)
levels(base06$true_OR)[levels(base06$true_OR)=="0.25"] <- expression(OR*"="*0.25)
levels(base06$true_OR)[levels(base06$true_OR)=="0.5"] <- expression(OR*"="*0.5)
levels(base06$true_OR)[levels(base06$true_OR)=="0.67"] <- expression(OR*"="*0.67)
levels(base06$true_OR)[levels(base06$true_OR)=="1"] <- expression(OR*"="*1)
base1$true_OR<-factor(base1$true_OR)
levels(base1$true_OR)[levels(base1$true_OR)=="0.25"] <- expression(OR*"="*0.25)
levels(base1$true_OR)[levels(base1$true_OR)=="0.5"] <- expression(OR*"="*0.5)
levels(base1$true_OR)[levels(base1$true_OR)=="0.67"] <- expression(OR*"="*0.67)
levels(base1$true_OR)[levels(base1$true_OR)=="1"] <- expression(OR*"="*1)

base0$tau2<-factor(base0$tau2)
levels(base0$tau2)[levels(base0$tau2)=="0"] <- expression(tau^2*"="*0)
levels(base0$tau2)[levels(base0$tau2)=="0.05"] <- expression(tau^2*"="*0.05)
levels(base0$tau2)[levels(base0$tau2)=="0.25"] <- expression(tau^2*"="*0.25)
base06$tau2<-factor(base06$tau2)
levels(base06$tau2)[levels(base06$tau2)=="0"] <- expression(tau^2*"="*0)
levels(base06$tau2)[levels(base06$tau2)=="0.05"] <- expression(tau^2*"="*0.05)
levels(base06$tau2)[levels(base06$tau2)=="0.25"] <- expression(tau^2*"="*0.25)
base1$tau2<-factor(base1$tau2)
levels(base1$tau2)[levels(base1$tau2)=="0"] <- expression(tau^2*"="*0)
levels(base1$tau2)[levels(base1$tau2)=="0.05"] <- expression(tau^2*"="*0.05)
levels(base1$tau2)[levels(base1$tau2)=="0.25"] <- expression(tau^2*"="*0.25)

f1=
  ggplot(data = base0, aes(x = k, y = power, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=0.1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Size",limits=c(0,0.25),breaks=seq(0,0.25,0.05)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figure2_rho0.pdf",width=7.9,height=5.9)
print(f1)
dev.off()



f1=
  ggplot(data = base06, aes(x = k, y = power, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Power",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figure3_rho06_pow.pdf",width=7.9,height=5.9)
print(f1)
dev.off()



f1=
  ggplot(data = base06, aes(x = k, y = pen_pow, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Penalized power",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figure4_rho06_pen_pow.pdf",width=7.9,height=5.9)
print(f1)
dev.off()



f1=
  ggplot(data = base06, aes(x = k, y = adj_pow, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Power adjusted for size",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figureS5_rho06_adj_pow.pdf",width=7.9,height=5.9)
print(f1)
dev.off()



vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

f1=
  ggplot(data = base06, aes(x = k, y = plr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=log10(10), slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_log10("Positive likelihood ratios",breaks=c(2,5,10,20), labels=c(2,5,10,20)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

f2=
  ggplot(data = base06, aes(x = k, y = nlr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=0.1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Negative likelihood ratios",breaks=c(0,0.1,0.5,1), labels=c(0,0.1,0.5,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

dev.new()
pdf("output/figure5_rho06_lr.pdf",width=10.8,height=5.9)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(f1, vp = vplayout(1, 1))
print(f2, vp = vplayout(1, 2))
dev.off()



f1=
  ggplot(data = base1, aes(x = k, y = power, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Power",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figureS1_rho1_pow.pdf", width=7.9,height=5.9)
print(f1)
dev.off()



f1=
  ggplot(data = base1, aes(x = k, y = pen_pow, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Penalized power",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figureS2_rho1_pen_pow.pdf", width=7.9,height=5.9)
print(f1)
dev.off()


f1=
  ggplot(data = base1, aes(x = k, y = adj_pow, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Power adjusted for size",limits=c(0,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90))

dev.new()
pdf("output/figureS6_rho1_adj_pow.pdf", width=7.9,height=5.9)
print(f1)
dev.off()



vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

f1=
  ggplot(data = base1, aes(x = k, y = plr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=log10(10), slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_log10("Positive likelihood ratios",breaks=c(2,5,10,20), labels=c(2,5,10,20)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

f2=
  ggplot(data = base1, aes(x = k, y = nlr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=0.1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Negative likelihood ratios",breaks=c(0,0.1,0.5,1), labels=c(0,0.1,0.5,1)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4,5),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A","#FFFF33","#E41A1C","#FF7F00","#984EA3"),breaks=c("rank","linreg","count","thom","tf","comb"),labels=c("Begg","Egger","Schwarzer","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

dev.new()
pdf("output/figureS3_rho1_lr.pdf", width=10.8,height=5.9)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(f1, vp = vplayout(1, 1))
print(f2, vp = vplayout(1, 2))
dev.off()



# relative positive and negative likelihood ratios
# read-in results
plotbase2 <- read.csv("simulations/plot_rlrs.csv",header=TRUE, sep=",")
base0_2 <- plotbase2[plotbase2$rho==0,]
base06_2 <- plotbase2[plotbase2$rho==-0.6,]
base1_2 <- plotbase2[plotbase2$rho==-1,]

base06_2$true_OR<-factor(base06_2$true_OR)
levels(base06_2$true_OR)[levels(base06_2$true_OR)=="0.25"] <- expression(OR*"="*0.25)
levels(base06_2$true_OR)[levels(base06_2$true_OR)=="0.5"] <- expression(OR*"="*0.5)
levels(base06_2$true_OR)[levels(base06_2$true_OR)=="0.67"] <- expression(OR*"="*0.67)
levels(base06_2$true_OR)[levels(base06_2$true_OR)=="1"] <- expression(OR*"="*1)
base1_2$true_OR<-factor(base1_2$true_OR)
levels(base1_2$true_OR)[levels(base1_2$true_OR)=="0.25"] <- expression(OR*"="*0.25)
levels(base1_2$true_OR)[levels(base1_2$true_OR)=="0.5"] <- expression(OR*"="*0.5)
levels(base1_2$true_OR)[levels(base1_2$true_OR)=="0.67"] <- expression(OR*"="*0.67)
levels(base1_2$true_OR)[levels(base1_2$true_OR)=="1"] <- expression(OR*"="*1)

base06_2$tau2<-factor(base06_2$tau2)
levels(base06_2$tau2)[levels(base06_2$tau2)=="0"] <- expression(tau^2*"="*0)
levels(base06_2$tau2)[levels(base06_2$tau2)=="0.05"] <- expression(tau^2*"="*0.05)
levels(base06_2$tau2)[levels(base06_2$tau2)=="0.25"] <- expression(tau^2*"="*0.25)
base1_2$tau2<-factor(base1_2$tau2)
levels(base1_2$tau2)[levels(base1_2$tau2)=="0"] <- expression(tau^2*"="*0)
levels(base1_2$tau2)[levels(base1_2$tau2)=="0.05"] <- expression(tau^2*"="*0.05)
levels(base1_2$tau2)[levels(base1_2$tau2)=="0.25"] <- expression(tau^2*"="*0.25)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
f1=
  ggplot(data = base06_2, aes(x = k, y = rplr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Relative positive likelihood ratios",breaks=c(0,1,2,3,4), labels=c(0,1,2,3,4)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A", "#E41A1C","#FF7F00","#984EA3"),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")
f2=
  ggplot(data = base06_2, aes(x = k, y = rnlr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Relative negative likelihood ratios",breaks=c(0,1,2,3,4,5), labels=c(0,1,2,3,4,5)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A", "#E41A1C","#FF7F00","#984EA3"),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

dev.new()
pdf("output/figure6_rho06_rlr.pdf", width=10.8,height=5.9)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(f1, vp = vplayout(1, 1))
print(f2, vp = vplayout(1, 2))
dev.off()




vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
f1=
  ggplot(data = base1_2, aes(x = k, y = rplr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Relative positive likelihood ratios",breaks=c(0,1,2,3), labels=c(0,1,2,3)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A", "#E41A1C","#FF7F00","#984EA3"),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")
f2=
  ggplot(data = base1_2, aes(x = k, y = rnlr, group = test, shape = test, colour = test)) +
  geom_point()+
  geom_line()+
  facet_grid(tau2 ~ true_OR, margins = FALSE, labeller = label_parsed)+ 
  geom_abline(intercept=1, slope=0, linetype="longdash", color="black") +
  scale_x_log10("Number of trials per meta-analysis", breaks=c(5,10,20,50), labels=c(5,10,20,50))+
  scale_y_continuous("Relative negative likelihood ratios",breaks=c(0,1,2,3), labels=c(0,1,2,3)) + 
  scale_shape_manual(name="",values=c(0,1,2,3,4),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  scale_colour_manual(name="",values=c("#377EB8","#4DAF4A", "#E41A1C","#FF7F00","#984EA3"),breaks=c("rank/count","linreg/count","thom/count","tf/count","comb/count"),labels=c("Begg","Egger","Arcsine-Thompson","Trim and Fill","Combined test"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_text(angle=-90),legend.position = "bottom")

dev.new()
pdf("output/figureS4_rho1_rlr.pdf", width=10.8,height=5.9)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(f1, vp = vplayout(1, 1))
print(f2, vp = vplayout(1, 2))
dev.off()



pdf("output/figure1_explore_lrs.pdf",width=10.8,height=5.9)

lrplus <- function(alpha, power){ power/alpha }
lrminus <- function(alpha, power){ (1-power)/(1-alpha) }

par(mfrow=c(1,2),las=1)

x <-seq(0.01,0.2,0.001)
y <-seq(0.01,1,0.001)
z <- outer(x,y,lrplus)
contour(x, y, z, levels=c(1,2,3,4,5,10,20,30,40,50), log="x", xlab="Size", ylab="Power", labcex = 0.6)

z <- outer(x,y,lrminus)
contour(x, y, z, levels=c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1),log="x", xlab="Size", ylab="Power", labcex = 0.6)

dev.off()


