# Based on Lin and Chu's function for skewness test
# Author: L. Lin and H. Chu (Lin and Chu, 2018)

skewtest <- function(y,s2,tau2,k){
  y.reg <- y/sqrt(s2 + tau2)
  x.reg <- 1/sqrt(s2 + tau2)
  
  reg <- lm(y.reg ~ x.reg)
  reg.coef <- summary(reg)$coef
  reg.pval <- reg.coef["(Intercept)", "Pr(>|t|)"]
  
  std.dev <- as.numeric(summary(reg)$residuals)
  cm2 <- var(std.dev)
  cm3 <- mean((std.dev - mean(std.dev))^3)
  cm4 <- mean((std.dev - mean(std.dev))^4)
  cm5 <- mean((std.dev - mean(std.dev))^5)
  cm6 <- mean((std.dev - mean(std.dev))^6)
  skewness <- cm3/(cm2^(1.5))
  
  var0.skew <- 6
  skewness.pval <- 2*(1 - pnorm(sqrt(k/var0.skew)*abs(skewness)))
  combined.pval <- 1 - (1 - min(c(reg.pval, skewness.pval)))^2
  out <- NULL
  out$reg.pval <- reg.pval
  out$skewness.pval <- skewness.pval
  out$combined.pval <- combined.pval
  return(out)
}
