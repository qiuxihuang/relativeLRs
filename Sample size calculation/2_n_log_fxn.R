# Function to solve for number of repetitions numerically based on log-transformed CI of relative likelihood ratios
# Takes estimated power and size of the two tests, as well as TPFab, FPFab, the relative likelihood ratios, and the required relative precision
# 'dplus' and 'dminus' are relative precision on relative positive and negative likelihood ratios respectively
# Sample size increases by inc each time until the required precision is obtained
# Returns minimum number of repetitions based on relative positive and negative likelihood ratios respectively

n.log.rlr <- function(rplr,rnlr,powera,powerb,sizea,sizeb,tpfab,fpfab,drplr,drnlr,inc){
  
  dplus <- dminus <- 999
  nplus_log <- nminus_log <- 0
  vplus <- lplus <- uplus <- 0
  vminus <- lminus <- uminus <- 0
  deltaplus <- rplr*drplr
  deltaminus <- rnlr*drnlr
  
  while (dplus>deltaplus){
    nplus_log <- nplus_log+inc
    vplus <- 1/nplus_log*((powera+powerb-2*tpfab)/(powera*powerb)+(sizea+sizeb-2*fpfab)/(sizea*sizeb))
    lplus <- log(rplr)-1.96*sqrt(vplus)
    uplus <- log(rplr)+1.96*sqrt(vplus)
    dplus <- (exp(uplus)-exp(lplus))/2
  }
  
  while (dminus>deltaminus){
    nminus_log <- nminus_log+inc
    vminus <- 1/nminus_log*((powera+powerb-2*tpfab)/((1-powera)*(1-powerb))+(sizea+sizeb-2*fpfab)/((1-sizea)*(1-sizeb)))
    lminus <- log(rnlr)-1.96*sqrt(vminus)
    uminus <- log(rnlr)+1.96*sqrt(vminus)
    dminus <- (exp(uminus)-exp(lminus))/2
  }
  out <- NULL
  out$nplus_log <- nplus_log
  out$nminus_log <- nminus_log
  return(out)
}

