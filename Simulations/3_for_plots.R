library(plyr)
library(dplyr)
setwd("...") # set your working directory

n <- 55000

res <- read.csv(paste0("sim_", n, "_results.csv"), header=T, sep=",")
scenarios <- read.csv(paste0("table_scenarios.csv"), header=T, sep=",")

# Begg's test
rank.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','rank.size','rank.power','rank.adj_pow','rank.pen_pow','rank.plr','rank.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(rank.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
rank.all$test <- "rank"

# Egger's test
linreg.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','linreg.size','linreg.power','linreg.adj_pow','linreg.pen_pow','linreg.plr','linreg.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(linreg.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
linreg.all$test <- "linreg"

# Schwarzer's test
count.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','count.size','count.power','count.adj_pow','count.pen_pow','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(count.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
count.all$test <- "count"

# Lin and Chu's skewness-regression combined test
comb.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','comb.size','comb.power','comb.adj_pow','comb.pen_pow','comb.plr','comb.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(comb.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
comb.all$test <- "comb"

# arcsine-Thompson's test
thom.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','thom.size','thom.power','thom.adj_pow','thom.pen_pow','thom.plr','thom.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(thom.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
thom.all$test <- "thom"

# trim-and-fill test
tf.all <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','tf.size','tf.power','tf.adj_pow','tf.pen_pow','tf.plr','tf.nlr')],by=c("k", "true_OR", "tau2", "rho"))
names(tf.all) <- c('k','true_OR','tau2','rho','scenario','size','power','adj_pow','pen_pow','plr','nlr')
tf.all$test <- "tf"

# output results
results <- rbind(rank.all,linreg.all,count.all,thom.all,tf.all,comb.all)
write.csv(results, "plot_lrs_penpow_adjpow.csv", row.names=FALSE)

# relative positive and negative likelihood ratios
rankcount <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','rank.plr','rank.nlr','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
rankcount$test <- "rank/count"
rankcount$rplr <- rankcount$rank.plr/rankcount$count.plr
rankcount$rnlr <- rankcount$rank.nlr/rankcount$count.nlr

tfcount <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','tf.plr','tf.nlr','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
tfcount$test <- "tf/count"
tfcount$rplr <- tfcount$tf.plr/tfcount$count.plr
tfcount$rnlr <- tfcount$tf.nlr/tfcount$count.nlr

thomcount <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','thom.plr','thom.nlr','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
thomcount$test <- "thom/count"
thomcount$rplr <- thomcount$thom.plr/thomcount$count.plr
thomcount$rnlr <- thomcount$thom.nlr/thomcount$count.nlr

linregcount <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','linreg.plr','linreg.nlr','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
linregcount$test <- "linreg/count"
linregcount$rplr <- linregcount$linreg.plr/linregcount$count.plr
linregcount$rnlr <- linregcount$linreg.nlr/linregcount$count.nlr

combcount <- scenarios %>% left_join(res[,c('k','true_OR','tau2','rho','comb.plr','comb.nlr','count.plr','count.nlr')],by=c("k", "true_OR", "tau2", "rho"))
combcount$test <- "comb/count"
combcount$rplr <- combcount$comb.plr/combcount$count.plr
combcount$rnlr <- combcount$comb.nlr/combcount$count.nlr

# output results
results2 <- rbind.fill(rankcount, linregcount, tfcount, thomcount, combcount)
write.csv(results2, "plot_rlrs.csv", row.names=FALSE)
