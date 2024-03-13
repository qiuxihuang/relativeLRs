# Function to calculate the minimum required sample size based on the proposed closed formulas
# Takes estimated power and size of the two tests, as well as TPFab, FPFab, and the required relative precision
# 'drplr' and 'drnlr' are relative precision on relative positive and negative likelihood ratios respectively
# Returns minimum number of repetitions based on relative positive and negative likelihood ratios respectively

n.rlr <- function(powera,powerb,sizea,sizeb,tpfab,fpfab,drplr,drnlr){
  nplus <- (1.96/drplr)^2*((1-powera)/powera+(1-powerb)/powerb+(1-sizea)/sizea+(1-sizeb)/sizeb-
                             2*tpfab/powera/powerb-2*fpfab/sizea/sizeb+4)
  
  nminus <- (1.96/drnlr)^2*(powera/(1-powera)+powerb/(1-powerb)+sizea/(1-sizea)+sizeb/(1-sizeb)-
                              2*tpfab/(1-powera)/(1-powerb)-2*fpfab/(1-sizea)/(1-sizeb)+
                              2*powera*powerb/(1-powera)/(1-powerb)+
                              2*sizea*sizeb/(1-sizea)/(1-sizeb))
  out <- NULL
  out$nplus <- nplus
  out$nminus <- nminus
  return(out)
}
