#library("remotes")
#library("fs")
#remotes::install_github("guido-s/meta", ref = "R-book-first-edition")
library("meta") # an older version of the "meta" package is used
library("metafor")

setwd("...") # set your working directory
source("gen_meta.R")
source("skewtest.R")
source("tnf.R")

# Monte Carlo simulation
set.seed(123456)
ntrial <- c(5,10,20,50) #5,10,20,50
true_or <- c(0.25,0.5,0.67,1) # 0.25,0.5,0.67,1
Tau2 <- c(0,0.05,0.25) #0,0.05,0.25
Rho <- c(0,-0.6,-1) #0,-0.6,-1

n <- 50000

for (k in ntrial){
  for (OR in true_or){
    for (tau2 in Tau2){
      for (rho in Rho){
        
        i <- 0
        
        rslts <- matrix(NA, ncol=6)
        rslts <- as.data.frame(rslts)
        
        while (i < n){
          i <- i+1
          dat <- gen_meta(k,OR,tau2,rho)
          meta1 <- metabin(Xe, Ne, Xc, Nc, data=dat, sm="OR", method="Inverse", method.tau = "DL") 
          meta2 <- metabin(Xe, Ne, Xc, Nc, data=dat, sm="ASD", method="Inverse") 
          
          rank.pval <- metabias(meta1,method.bias="rank",k.min=5)$p.value # Begg's test
          linreg.pval <- metabias(meta1,method.bias="linreg",k.min=5)$p.value # Egger's test
          count.pval <- metabias(meta1,method.bias="count",k.min=5)$p.value # Schwarzer's test
          thom.pval <- metabias(meta2,method.bias="mm",k.min=5)$p.value # arcsine-Thompson's test
          
          # define y and s2
          y <- dat$logORi
          s2 <- dat$s2
          
          # trim-and-fill test
          rma1 <- rma(yi = y, vi = s2, method = ifelse(tau2 == 0, "FE", "DL"))
          tf <- tnf(rma1, estimator = "R0")
          tf.pval <- tf$p.k0
          
          # Lin and Chu's skewness-regression combined test
          comb.pval <- skewtest(y,s2,meta1$tau2,k)$combined.pval
          
          rslts[i,] <- c(rank.pval,linreg.pval,count.pval,thom.pval,tf.pval,comb.pval)
          colnames(rslts) <- c('rank.pval','linreg.pval','count.pval','thom.pval','tf.pval','comb.pval')
        }
        write.table(rslts, file =
                      paste0("pval_n_", n, "_k_", k, "_OR_", OR, "_tau2_", tau2, "_rho_", rho, 
                             ".txt", sep = ""), row.names = FALSE)
      }
    }
  }
}

