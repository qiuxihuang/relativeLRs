library(dplyr)
setwd("...") # set your working directory
source("1_lrs_fxn.R")

ntrial <- c(5,10,20,50)
true_or <- c(0.25,0.5,0.67,1)
Tau2 <- c(0,0.05,0.25)
Rho <- c(0,-0.6,-1)

n1 <- 50000
n2 <- 5000
n <- n1+n2

for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts1 <- read.table(paste0("simulations/pval_n_", n1, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                    ".txt"), header=T, sep="")
        rslts2 <- read.table(paste0("simulations/pval_n_", n2, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                    ".txt"), header=T, sep="")
        rslts <- rbind(rslts1,rslts2) # build-up to 55,000 repetitions
        write.table(rslts, file =
                      paste0("simulations/pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                             ".txt", sep = ""), row.names = FALSE)
      }
    }
  }
}

power <- matrix(NA,ncol=10)
power <- as.data.frame(power)
names(power) <- c("k","true_OR","tau2","rho",
                  "rank.power", "linreg.power", "count.power","thom.power","tf.power","comb.power")

scenario <- 0


for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts <- read.table(paste0("simulations/pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                    ".txt"), header=T, sep="")
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

write.csv(power,file=paste0("simulations/power_", n, "_results.csv"), row.names = F)



Rho <- c(0)


size <- matrix(NA,ncol=9)
size <- as.data.frame(size)
names(size) <- c("k","true_OR","tau2",
                 "rank.size", "linreg.size", "count.size","thom.size","tf.size","comb.size")

scenario <- 0


for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts <- read.table(paste0("simulations/pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                    ".txt"), header=T, sep="")
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

write.csv(size,file=paste0("simulations/size_", n, "_results.csv"), row.names = F)


allrslts <- power %>% left_join(size,by=c("k", "true_OR", "tau2"))
# penalized power
allrslts$rank.pen_pow <- lrs.pp.ap(allrslts$rank.power,allrslts$rank.size)$pen.pow
allrslts$linreg.pen_pow <- lrs.pp.ap(allrslts$linreg.power,allrslts$linreg.size)$pen.pow
allrslts$count.pen_pow <- lrs.pp.ap(allrslts$count.power,allrslts$count.size)$pen.pow
allrslts$thom.pen_pow <- lrs.pp.ap(allrslts$thom.power,allrslts$thom.size)$pen.pow
allrslts$comb.pen_pow <- lrs.pp.ap(allrslts$comb.power,allrslts$comb.size)$pen.pow
allrslts$tf.pen_pow <- lrs.pp.ap(allrslts$tf.power,allrslts$tf.size)$pen.pow
# adjusted power
allrslts$rank.adj_pow <- lrs.pp.ap(allrslts$rank.power,allrslts$rank.size)$adj.pow
allrslts$linreg.adj_pow <- lrs.pp.ap(allrslts$linreg.power,allrslts$linreg.size)$adj.pow
allrslts$count.adj_pow <- lrs.pp.ap(allrslts$count.power,allrslts$count.size)$adj.pow
allrslts$thom.adj_pow <- lrs.pp.ap(allrslts$thom.power,allrslts$thom.size)$adj.pow
allrslts$comb.adj_pow <- lrs.pp.ap(allrslts$comb.power,allrslts$comb.size)$adj.pow
allrslts$tf.adj_pow <- lrs.pp.ap(allrslts$tf.power,allrslts$tf.size)$adj.pow
# positive likelihood ratio
allrslts$rank.plr <- lrs.pp.ap(allrslts$rank.power,allrslts$rank.size)$plr
allrslts$linreg.plr <- lrs.pp.ap(allrslts$linreg.power,allrslts$linreg.size)$plr
allrslts$count.plr <- lrs.pp.ap(allrslts$count.power,allrslts$count.size)$plr
allrslts$thom.plr <- lrs.pp.ap(allrslts$thom.power,allrslts$thom.size)$plr
allrslts$comb.plr <- lrs.pp.ap(allrslts$comb.power,allrslts$comb.size)$plr
allrslts$tf.plr <- lrs.pp.ap(allrslts$tf.power,allrslts$tf.size)$plr
# negative likelihood ratio
allrslts$rank.nlr <- lrs.pp.ap(allrslts$rank.power,allrslts$rank.size)$nlr
allrslts$linreg.nlr <- lrs.pp.ap(allrslts$linreg.power,allrslts$linreg.size)$nlr
allrslts$count.nlr <- lrs.pp.ap(allrslts$count.power,allrslts$count.size)$nlr
allrslts$thom.nlr <- lrs.pp.ap(allrslts$thom.power,allrslts$thom.size)$nlr
allrslts$comb.nlr <- lrs.pp.ap(allrslts$comb.power,allrslts$comb.size)$nlr
allrslts$tf.nlr <- lrs.pp.ap(allrslts$tf.power,allrslts$tf.size)$nlr

write.csv(allrslts,file=paste0("simulations/sim_", n, "_results.csv"),row.names = F)
