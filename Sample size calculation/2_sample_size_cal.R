setwd("...") # set your working directory
source("1_pp_ap_lrs.R")

n <- 5000

ntrial <- c(5) 
true_or <- c(0.25,0.5,0.67,1) 
Tau2 <- c(0.25) 
Rho <- c(-0.6)

res <- matrix(NA,ncol=16)
res <- as.data.frame(res)
names(res) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                "sizeA","sizeB","tpfAB","fpfAB","rplr","rnlr",
                "n_rplr","n_rnlr")


for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        rslts <- read.table(paste0("pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
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
        
        
        rslts <- read.table(paste0("pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho0, 
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
        
        rank.plr <- plr(rank.power, rank.size)
        linreg.plr <- plr(linreg.power, linreg.size)
        thom.plr <- plr(thom.power, thom.size)
        tf.plr <- plr(tf.power, tf.size)
        comb.plr <- plr(comb.power, comb.size)
        count.plr <- plr(count.power, count.size)
        
        rank.nlr <- nlr(rank.power, rank.size)
        linreg.nlr <- nlr(linreg.power, linreg.size)
        thom.nlr <- nlr(thom.power, thom.size)
        tf.nlr <- nlr(tf.power, tf.size)
        comb.nlr <- nlr(comb.power, comb.size)
        count.nlr <- nlr(count.power, count.size)
        
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
        sample$tpf <- c(tpf.rc,tpf.lc,tpf.thc,tpf.tfc,tpf.cc)
        sample$fpf <- c(fpf.rc,fpf.lc,fpf.thc,fpf.tfc,fpf.cc)
        sample$betaa <- 1-sample$powera
        sample$betab <- 1-sample$powerb
        sample$rplr <- c(rplr.rc,rplr.lc,rplr.thc,rplr.tfc,rplr.cc)
        sample$rnlr <- c(rnlr.rc,rnlr.lc,rnlr.thc,rnlr.tfc,rnlr.cc)
        
        sample$nplus <- (1.96/0.1)^2*(sample$betaa/sample$powera+sample$betab/sample$powerb+
                                        (1-sample$sizea)/sample$sizea+(1-sample$sizeb)/sample$sizeb-
                                        2*sample$tpf/sample$powera/sample$powerb-2*sample$fpf/sample$sizea/sample$sizeb+4)
        
        sample$nminus <- (1.96/0.1)^2*(sample$powera/sample$betaa+sample$powerb/sample$betab+
                                         sample$sizea/(1-sample$sizea)+sample$sizeb/(1-sample$sizeb)-
                                         2*sample$tpf/sample$betaa/sample$betab-2*sample$fpf/(1-sample$sizea)/(1-sample$sizeb)+
                                         2*sample$powera*sample$powerb/sample$betaa/sample$betab+
                                         2*sample$sizea*sample$sizeb/(1-sample$sizea)/(1-sample$sizeb))
        
        df <- as.data.frame(sample)
        
        
        names(df) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                       "sizeA","sizeB","tpfAB","fpfAB","betaA","betaB","rplr","rnlr",
                       "n_rplr","n_rnlr")
        
        drops <- c("betaA","betaB")
        df <- df[ , !(names(df) %in% drops)]
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
  betaa <- 1-powera
  betab <- 1-powerb
  tpf <- res$tpfAB[i]
  fpf <- res$fpfAB[i]
  deltaplus <- rplr*0.1
  deltaminus <- rnlr*0.1
  
  dplus <- dminus <- 999
  nplus_log <- nminus_log <- 0
  vplus <- lplus <- uplus <- 0
  vminus <- lminus <- uminus <- 0
  
  while (dplus>deltaplus){
    nplus_log <- nplus_log+100
    vplus <- 1/nplus_log*((powera+powerb-2*tpf)/(powera*powerb)+(sizea+sizeb-2*fpf)/(sizea*sizeb))
    lplus <- log(rplr)-1.96*sqrt(vplus)
    uplus <- log(rplr)+1.96*sqrt(vplus)
    dplus <- (exp(uplus)-exp(lplus))/2
  }
  
  while (dminus>deltaminus){
    nminus_log <- nminus_log+100
    vminus <- 1/nminus_log*((powera+powerb-2*tpf)/(betaa*betab)+(sizea+sizeb-2*fpf)/((1-sizea)*(1-sizeb)))
    lminus <- log(rnlr)-1.96*sqrt(vminus)
    uminus <- log(rnlr)+1.96*sqrt(vminus)
    dminus <- (exp(uminus)-exp(lminus))/2
  }
  res$nplus_log[i] <- nplus_log
  res$nminus_log[i] <- nminus_log
}

names(res) <- c("k","true_OR","tau2","rho","testA","testB","powerA","powerB",
                "sizeA","sizeB","tpfAB","fpfAB","rplr","rnlr",
                "n_rplr","n_rnlr","n_log_rplr","n_log_rnlr")

write.csv(res,file=paste0("output/tableA1_sample_size_all.csv"), row.names = F) # Table A1
