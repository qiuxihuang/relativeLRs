library(dplyr)
setwd("...") # set your working directory
source("2_pp_ap_lrs.R")

ntrial <- c(5,10,20,50)
true_or <- c(0.25,0.5,0.67,1)
Tau2 <- c(0,0.05,0.25)
Rho <- c(0)

n1 <- 50000
n2 <- 5000
n <- n1+n2

size <- matrix(NA,ncol=9)
size <- as.data.frame(size)
names(size) <- c("k","true_OR","tau2",
                 "rank.size", "linreg.size", "count.size","thom.size","tf.size","comb.size")

scenario <- 0

for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts1 <- read.table(paste0("pval_n_", n1, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                   ".txt"), header=T, sep="")
        rslts2 <- read.table(paste0("pval_n_", n2, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                   ".txt"), header=T, sep="")
        rslts <- rbind(rslts1,rslts2) # build-up to 55,000 repetitions
        # set nominal = 0.1
        rank.size.di<-ifelse(rslts$rank.pval<0.1,1,0)
        linreg.size.di<- ifelse(rslts$linreg.pval<0.1,1,0)
        count.size.di<- ifelse(rslts$count.pval<0.1,1,0)
        thom.size.di<-ifelse(rslts$thom.pval<0.1,1,0)
        comb.size.di<-ifelse(rslts$comb.pval<0.1,1,0)
        tf.size.di<- ifelse(rslts$tf.pval<0.1,1,0)
        
        rank.size <- sum(rank.size.di)/n
        linreg.size <- sum(linreg.size.di)/n
        count.size <- sum(count.size.di)/n
        thom.size <- sum(thom.size.di)/n
        comb.size <- sum(comb.size.di)/n
        tf.size <- sum(tf.size.di)/n
        
        scenario <- scenario + 1
        size[scenario,] <- c(k, OR, tau2, 
                             rank.size, linreg.size, count.size, thom.size, tf.size, comb.size)	
      }
    }
  }
}

write.csv(size,file=paste0("size_", n, "_results.csv"), row.names = F)

Rho <- c(0,-0.6,-1)

power <- matrix(NA,ncol=10)
power <- as.data.frame(power)
names(power) <- c("k","true_OR","tau2","rho",
                  "rank.power", "linreg.power", "count.power","thom.power","tf.power","comb.power")

scenario <- 0

for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts1 <- read.table(paste0("pval_n_", n1, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                   ".txt"), header=T, sep="")
        rslts2 <- read.table(paste0("pval_n_", n2, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                    ".txt"), header=T, sep="")
        rslts <- rbind(rslts1,rslts2) # build-up to 55,000 repetitions
        # set nominal = 0.1
        rank.power.di<-ifelse(rslts$rank.pval<0.1,1,0)
        linreg.power.di<- ifelse(rslts$linreg.pval<0.1,1,0)
        count.power.di<- ifelse(rslts$count.pval<0.1,1,0)
        thom.power.di<-ifelse(rslts$thom.pval<0.1,1,0)
        comb.power.di<-ifelse(rslts$comb.pval<0.1,1,0)
        tf.power.di<- ifelse(rslts$tf.pval<0.1,1,0)
        
        rank.power <- sum(rank.power.di)/n
        linreg.power <- sum(linreg.power.di)/n
        count.power <- sum(count.power.di)/n
        thom.power <- sum(thom.power.di)/n
        comb.power <- sum(comb.power.di)/n
        tf.power <- sum(tf.power.di)/n
        
        scenario <- scenario + 1
        power[scenario,] <- c(k, OR, tau2, rho,
                              rank.power, linreg.power,count.power,thom.power,tf.power,comb.power)	
      }
    }
  }
}
write.csv(power,file=paste0("power_", n, "_results.csv"), row.names = F)

allrslts <- power %>% left_join(size,by=c("k", "true_OR", "tau2"))
# penalized power
allrslts$rank.pen_pow <- pen.pow(allrslts$rank.power,allrslts$rank.size)
allrslts$linreg.pen_pow <- pen.pow(allrslts$linreg.power,allrslts$linreg.size)
allrslts$count.pen_pow <- pen.pow(allrslts$count.power,allrslts$count.size)
allrslts$thom.pen_pow <- pen.pow(allrslts$thom.power,allrslts$thom.size)
allrslts$comb.pen_pow <- pen.pow(allrslts$comb.power,allrslts$comb.size)
allrslts$tf.pen_pow <- pen.pow(allrslts$tf.power,allrslts$tf.size)
# adjusted power
allrslts$rank.adj_pow <- adj.pow(allrslts$rank.power,allrslts$rank.size)
allrslts$linreg.adj_pow <- adj.pow(allrslts$linreg.power,allrslts$linreg.size)
allrslts$count.adj_pow <- adj.pow(allrslts$count.power,allrslts$count.size)
allrslts$thom.adj_pow <- adj.pow(allrslts$thom.power,allrslts$thom.size)
allrslts$comb.adj_pow <- adj.pow(allrslts$comb.power,allrslts$comb.size)
allrslts$tf.adj_pow <- adj.pow(allrslts$tf.power,allrslts$tf.size)
# positive likelihood ratio
allrslts$rank.plr <- plr(allrslts$rank.power,allrslts$rank.size)
allrslts$linreg.plr <- plr(allrslts$linreg.power,allrslts$linreg.size)
allrslts$count.plr <- plr(allrslts$count.power,allrslts$count.size)
allrslts$thom.plr <- plr(allrslts$thom.power,allrslts$thom.size)
allrslts$comb.plr <- plr(allrslts$comb.power,allrslts$comb.size)
allrslts$tf.plr <- plr(allrslts$tf.power,allrslts$tf.size)
# negative likelihood ratio
allrslts$rank.nlr <- nlr(allrslts$rank.power,allrslts$rank.size)
allrslts$linreg.nlr <- nlr(allrslts$linreg.power,allrslts$linreg.size)
allrslts$count.nlr <- nlr(allrslts$count.power,allrslts$count.size)
allrslts$thom.nlr <- nlr(allrslts$thom.power,allrslts$thom.size)
allrslts$comb.nlr <- nlr(allrslts$comb.power,allrslts$comb.size)
allrslts$tf.nlr <- nlr(allrslts$tf.power,allrslts$tf.size)

write.csv(allrslts,file=paste0("sim_", n, "_results.csv"),row.names = F)
