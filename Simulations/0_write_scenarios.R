# set the "code and data supplements" folder as the working directory
#setwd(".../code and data supplements")

# Summarizing considered scenarios in a table 
ntrial <- c(5,10,20,50)
true_or <- c(0.25,0.5,0.67,1)
Tau2 <- c(0,0.05,0.25)
Rho <- c(0,-0.6,-1)

scenarios <- data.frame(expand.grid(ntrial, true_or, Tau2, Rho))
colnames(scenarios) <- c("k", "true_OR", "tau2", "rho")
scenarios$scenario <- 1:nrow(scenarios)
write.csv(scenarios, file=paste0("intermediate results/table_scenarios.csv"), row.names=FALSE)
