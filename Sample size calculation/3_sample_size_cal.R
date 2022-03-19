setwd("...") # set your working directory
source("1_lrs_fxn.R")
source("1_n_rlr_fxn.R")
source("2_n_logrlr_fxn.R")

n <- 5000

ntrial <- c(5) 
true_or <- c(0.25,0.5,0.67,1) 
Tau2 <- c(0.25) 
Rho <- c(-0.6)

res <- matrix(NA,ncol=14)
res <- as.data.frame(res)
names(res) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                "sizeA","sizeB","tpfAB","fpfAB","rplr","rnlr")

for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts <- read.table(paste0("simulations/pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                                   ".txt"), header=T, sep="")
        # set nominal = 0.1
        rank.power.di<-ifelse(rslts$rank.pval<0.1,1,0)
        linreg.power.di<- ifelse(rslts$linreg.pval<0.1,1,0)
        thom.power.di<-ifelse(rslts$thom.pval<0.1,1,0)
        tf.power.di<- ifelse(rslts$tf.pval<0.1,1,0)
        comb.power.di<-ifelse(rslts$comb.pval<0.1,1,0)
        count.power.di<- ifelse(rslts$count.pval<0.1,1,0)
        
        tpf.rc.di <- ifelse(rslts$rank.pval<0.1 & rslts$count.pval<0.1,1,0)
        tpf.lc.di <- ifelse(rslts$linreg.pval<0.1 & rslts$count.pval<0.1,1,0)
        tpf.thc.di <- ifelse(rslts$thom.pval<0.1 & rslts$count.pval<0.1,1,0)
        tpf.tfc.di <- ifelse(rslts$tf.pval<0.1 & rslts$count.pval<0.1,1,0)
        tpf.cc.di <- ifelse(rslts$comb.pval<0.1 & rslts$count.pval<0.1,1,0)
        
        rank.power <- sum(rank.power.di)/n
        linreg.power <- sum(linreg.power.di)/n
        thom.power <- sum(thom.power.di)/n
        tf.power <- sum(tf.power.di)/n
        comb.power <- sum(comb.power.di)/n
        count.power <- sum(count.power.di)/n
        
        tpf.rc <- sum(tpf.rc.di)/n
        tpf.lc <- sum(tpf.lc.di)/n
        tpf.thc <- sum(tpf.thc.di)/n
        tpf.tfc <- sum(tpf.tfc.di)/n
        tpf.cc <- sum(tpf.cc.di)/n
        
        
        rho0 <- 0
        
        
        rslts <- read.table(paste0("simulations/pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho0, 
                                   ".txt"), header=T, sep="")
        # set nominal = 0.1
        rank.size.di<-ifelse(rslts$rank.pval<0.1,1,0)
        linreg.size.di<- ifelse(rslts$linreg.pval<0.1,1,0)
        thom.size.di<-ifelse(rslts$thom.pval<0.1,1,0)
        tf.size.di<- ifelse(rslts$tf.pval<0.1,1,0)
        comb.size.di<-ifelse(rslts$comb.pval<0.1,1,0)
        count.size.di<- ifelse(rslts$count.pval<0.1,1,0)
        
        fpf.rc.di <- ifelse(rslts$rank.pval<0.1 & rslts$count.pval<0.1,1,0)
        fpf.lc.di <- ifelse(rslts$linreg.pval<0.1 & rslts$count.pval<0.1,1,0)
        fpf.thc.di <- ifelse(rslts$thom.pval<0.1 & rslts$count.pval<0.1,1,0)
        fpf.tfc.di <- ifelse(rslts$tf.pval<0.1 & rslts$count.pval<0.1,1,0)
        fpf.cc.di <- ifelse(rslts$comb.pval<0.1 & rslts$count.pval<0.1,1,0)
        
        rank.size <- sum(rank.size.di)/n
        linreg.size <- sum(linreg.size.di)/n
        thom.size <- sum(thom.size.di)/n
        tf.size <- sum(tf.size.di)/n
        comb.size <- sum(comb.size.di)/n
        count.size <- sum(count.size.di)/n
        
        fpf.rc <- sum(fpf.rc.di)/n
        fpf.lc <- sum(fpf.lc.di)/n
        fpf.thc <- sum(fpf.thc.di)/n
        fpf.tfc <- sum(fpf.tfc.di)/n
        fpf.cc <- sum(fpf.cc.di)/n
        
        rank.plr <- lrs.pp.ap(rank.power, rank.size)$plr
        linreg.plr <- lrs.pp.ap(linreg.power, linreg.size)$plr
        thom.plr <- lrs.pp.ap(thom.power, thom.size)$plr
        tf.plr <- lrs.pp.ap(tf.power, tf.size)$plr
        comb.plr <- lrs.pp.ap(comb.power, comb.size)$plr
        count.plr <- lrs.pp.ap(count.power, count.size)$plr
        
        rank.nlr <- lrs.pp.ap(rank.power, rank.size)$nlr
        linreg.nlr <- lrs.pp.ap(linreg.power, linreg.size)$nlr
        thom.nlr <- lrs.pp.ap(thom.power, thom.size)$nlr
        tf.nlr <- lrs.pp.ap(tf.power, tf.size)$nlr
        comb.nlr <- lrs.pp.ap(comb.power, comb.size)$nlr
        count.nlr <- lrs.pp.ap(count.power, count.size)$nlr
        
        rplr.rc <- rank.plr/count.plr
        rplr.lc <- linreg.plr/count.plr
        rplr.thc <- thom.plr/count.plr
        rplr.tfc <- tf.plr/count.plr
        rplr.cc <- comb.plr/count.plr
        
        rnlr.rc <- rank.nlr/count.nlr
        rnlr.lc <- linreg.nlr/count.nlr
        rnlr.thc <- thom.nlr/count.nlr
        rnlr.tfc <- tf.nlr/count.nlr
        rnlr.cc <- comb.nlr/count.nlr
        
        sample <- NULL
        sample$k <- rep(k,5)
        sample$true_OR <- rep(OR,5)
        sample$tau2 <- rep(tau2,5)
        sample$rho <- rep(rho,5)
        sample$testa <- c("rank","linreg","thom","tf","comb")
        sample$testb <- rep("count",5)
        sample$powera <- c(rank.power,linreg.power,thom.power,tf.power,comb.power)
        sample$powerb <- rep(count.power,5)
        sample$sizea <- c(rank.size,linreg.size,thom.size,tf.size,comb.size)
        sample$sizeb <- rep(count.size,5)
        sample$tpfab <- c(tpf.rc,tpf.lc,tpf.thc,tpf.tfc,tpf.cc)
        sample$fpfab <- c(fpf.rc,fpf.lc,fpf.thc,fpf.tfc,fpf.cc)
        sample$rplr <- c(rplr.rc,rplr.lc,rplr.thc,rplr.tfc,rplr.cc)
        sample$rnlr <- c(rnlr.rc,rnlr.lc,rnlr.thc,rnlr.tfc,rnlr.cc)
        
        df <- as.data.frame(sample)
        
        names(df) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                       "sizeA","sizeB","tpfAB","fpfAB","rplr","rnlr")
        
        res <- rbind(res,df)	
      }
    }
  }
}

res <- res[-1,]

for (i in 1:nrow(res)){
  
  rplr <- res$rplr[i]
  rnlr <- res$rplr[i]
  powera <- res$powerA[i]
  powerb <- res$powerB[i]
  sizea <- res$sizeA[i]
  sizeb <- res$sizeB[i]
  tpfab <- res$tpfAB[i]
  fpfab <- res$fpfAB[i]
  # proposed formulas to calculate minimum required sample size based on Wald-type CI of relative likelihood ratios
  # relative precision = 0.1 on both relative positive and negative likelihood ratios
  n_rlr <- n.rlr(powera,powerb,sizea,sizeb,tpfab,fpfab,0.1,0.1)
  
  res$nplus[i] <- n_rlr$nplus
  res$nminus[i] <- n_rlr$nminus
  
  # solve for number of repetitions numerically based on log-transformed CI of relative likelihood ratios
  # increment = 100, relative precision = 0.1 on both relative positive and negative likelihood ratios
  n_logrlr <- n.log.rlr(rplr,rnlr,powera,powerb,sizea,sizeb,tpfab,fpfab,0.1,0.1,100)
  
  res$nplus_log[i] <- n_logrlr$nplus_log
  res$nminus_log[i] <- n_logrlr$nminus_log
}

names(res) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                "sizeA","sizeB","tpfAB","fpfAB","rplr","rnlr",
                "n_rplr","n_rnlr","n_log_rplr","n_log_rnlr")

write.csv(res,file=paste0("output/tableA1_sample_size_all.csv"), row.names = F) # Table A1
