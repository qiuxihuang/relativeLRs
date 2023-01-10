# Function taking power and size and nominal level, and yielding penalized power, adjusted power, positive and negative likelihood ratios

lrs.pp.ap <- function(power,size,nominal){
  out <- NULL
  out$pen.pow <- power/sqrt(1+abs(1-size/nominal))
  out$adj.pow <- ifelse(power!=size,pnorm(qnorm(power)-qnorm(size)+qnorm(nominal)),NA)
  out$plr <- ifelse(power!=size,power/size,NA)
  out$nlr <- ifelse(power!=size,(1-power)/(1-size),NA)
  return(out)
}
