# Function taking power and size and yielding penalized power, adjusted power, positive and negative likelihood ratios

lrs.pp.ap <- function(power,size){
  out <- NULL
  out$pen.pow <- power/sqrt(1+abs(1-size/0.1))
  out$adj.pow <- ifelse(power!=size,pnorm(qnorm(power)-qnorm(size)+qnorm(0.1)),NA)
  out$plr <- ifelse(power!=size,power/size,NA)
  out$nlr <- ifelse(power!=size,(1-power)/(1-size),NA)
  return(out)
}
