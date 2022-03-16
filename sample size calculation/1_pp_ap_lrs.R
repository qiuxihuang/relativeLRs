# Functions yielding penalized power, adjusted power, positive and negative likelihood ratios

pen.pow <- function(power,size){
  pen.pow <- power/sqrt(1+abs(1-size/0.1))
}

adj.pow<-function(power,size){
  adj.pow <- ifelse(power!=size,pnorm(qnorm(power)-qnorm(size)+qnorm(0.1)),NA)
}

plr <- function(power,size){
  plr <- ifelse(power!=size,power/size,NA)
}

nlr <- function(power,size){
  nlr <- ifelse(power!=size,(1-power)/(1-size),NA)
}
