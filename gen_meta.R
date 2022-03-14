#fuction generating k selected trials using copas model
gen_meta<-function(k,true_OR, tau2,rho){
  base <- data.frame(matrix(nrow=k, ncol=21)) 
  names(base)<-c("N.tmp", "Ne", "Nc","eta","p","logitp","logitpe","logitpc","Pe","Pc","Xe","Xc","Ye","Yc","logORi","s2","sigma2","u","prob","sel","trzero")
  base$sel=0
  base$trzero=0
  while (!all(base$sel==1)){
    base$N.tmp[base$sel==0]<-exp(rnorm(sum(base$sel==0), 4.62, 1.29))
    base$N.tmp[base$sel==0]<-sapply(base$N.tmp[base$sel==0], function(x) max(x,20))
    base$N.tmp[base$sel==0]<-floor(base$N.tmp[base$sel==0])+floor(base$N.tmp[base$sel==0])%%2
    base$Ne[base$sel==0]<-base$N.tmp[base$sel==0]/2
    base$Nc[base$sel==0]<-base$N.tmp[base$sel==0]/2
    base$eta[base$sel==0]<-rnorm(sum(base$sel==0), 0,1)
    base$p[base$sel==0]<-runif(sum(base$sel==0),0.3,0.70)
    base$logitp[base$sel==0]<-log(base$p[base$sel==0]/(1-base$p[base$sel==0]))
    base$logitpe[base$sel==0]<-base$logitp[base$sel==0] + log(true_OR) + sqrt(tau2)*base$eta[base$sel==0]/2
    base$logitpc[base$sel==0]<-base$logitp[base$sel==0] - sqrt(tau2)*base$eta[base$sel==0]/2
    base$Pe[base$sel==0]<-1/(1+exp(-base$logitpe[base$sel==0]))
    base$Pc[base$sel==0]<-1/(1+exp(-base$logitpc[base$sel==0]))
    base$Xe[base$sel==0]<-rbinom(sum(base$sel==0), base$Ne[base$sel==0], base$Pe[base$sel==0])
    base$Xc[base$sel==0]<-rbinom(sum(base$sel==0), base$Nc[base$sel==0], base$Pc[base$sel==0])
    base$Ye[base$sel==0]<-base$Ne[base$sel==0]-base$Xe[base$sel==0]
    base$Yc[base$sel==0]<-base$Nc[base$sel==0]-base$Xc[base$sel==0]
    
    base$trzero[base$sel==0 & (base$Xe==0|base$Xc==0|base$Ye==0|base$Yc==0)]<-1
    base$trzero[base$sel==0 & base$Xe==0 & base$Xc==0]<-2
    base$trzero[base$sel==0 & base$Ye==0 & base$Yc==0]<-2
    base$Xe[base$sel==0 & base$trzero==1]<-base$Xe[base$sel==0 & base$trzero==1]+0.5
    base$Xc[base$sel==0 & base$trzero==1]<-base$Xc[base$sel==0 & base$trzero==1]+0.5
    base$Ye[base$sel==0 & base$trzero==1]<-base$Ye[base$sel==0 & base$trzero==1]+0.5
    base$Yc[base$sel==0 & base$trzero==1]<-base$Yc[base$sel==0 & base$trzero==1]+0.5
    base$Ne[base$sel==0 & base$trzero==1]<-base$Ne[base$sel==0 & base$trzero==1]+1
    base$Nc[base$sel==0 & base$trzero==1]<-base$Nc[base$sel==0 & base$trzero==1]+1
    
    base$logORi[base$sel==0]<-log((base$Xe[base$sel==0]*base$Yc[base$sel==0])/(base$Ye[base$sel==0]*base$Xc[base$sel==0]))
    base$s2[base$sel==0]<-1/base$Xe[base$sel==0]+1/base$Ye[base$sel==0]+1/base$Xc[base$sel==0]+1/base$Yc[base$sel==0]
    base$sigma2[base$sel==0]<-(2/base$N.tmp[base$sel==0])*(1/base$Pe[base$sel==0]+1/(1-base$Pe[base$sel==0])+1/base$Pc[base$sel==0]+1/(1-base$Pc[base$sel==0]))
    base$u[base$sel==0]<- -1.656+0.157/sqrt(base$s2[base$sel==0])
    base$prob[base$sel==0]<- pnorm((base$u[base$sel==0]+rho*sqrt(base$sigma2[base$sel==0])*(base$logORi[base$sel==0]-log(true_OR))/(base$sigma2[base$sel==0]+tau2))/sqrt(1-rho**2*base$sigma2[base$sel==0]/(base$sigma2[base$sel==0]+tau2)))
    base$sel[base$sel==0]<- rbinom(sum(base$sel==0),1, base$prob[base$sel==0]) 
    base$sel[base$trzero==2]<-0
    base$trzero=0
  }
  return(base)
}
