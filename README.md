In this project, we provide R functions to calculate positive and negative likelihood ratios that incorporate both power and size in simulation studies comparing the performance two or more statistical tests of the same null hypothesis. We also share code to calculate the minimum number of repetitions to obtain a desired level of precision on the relative positive and negative likelihood ratios. Finally, we share R code to replicate Monte Carlo simulation studies comparing 6 tests for small-study effects in meta-analysis of randomized trials, in which we illustrate the proposed approach based on likelihood ratios and compare it to power adjusted, or penalized, for empirical size.

#### master.R
```
Master script to set working directory and source subordinate R scripts.
Please notice that '1_sim_meta.R' requires deprecated package versions for 'meta' and 'metafor' to run. 
``` 
#### Simulations

##### 0_gen_meta.R
```
R function used to generate data of meta-analyses of randomized trials using Copas and Shi's model.
```
##### 0_tnf.R
```
R function used to perform the trim-and-fill test. 
```
##### 0_skewtest.R
```
R function used to perform Lin and Chu's skewness-regression combined test. 
```
##### 1_sim_meta.R
```
R code used to perform 6 different tests for small-study effects on generated meta-analyses and save p-values separately for each scenario. Scenarios can be defined at line 13-16. Number of repetitions can be specified at line 18. 
```
##### 1_lrs_fxn.R
```
R function used to calculate penalized power, adjusted power, and likelihood ratios at a specified nominal level. 
```
##### 2_compile_rslts.R
```
R code used to compile all simulation results. Nominal level is set at 0.1. 
```
##### 3_for_plots.R
```
R code used to prepare simulation results for creating plots.
```
##### 4_gen_plots.R
```
R code used to present simulation results in plots.  
```
#### Sample size calculation

##### 1_n_rlr_fxn.R
```
R function used to solve for minimum required number of repetitions based on proposed closed-formed formulas. 
```
##### 2_n_logrlr_fxn.R
```
R function used to solve for minimum required number of repetitions numerically based on log-transformed CI of relative likelihood ratios. 
```
##### 3_sample_size_cal.R
```
R code used to calculate the minimum required sample size to achieve a certain level of relative precision on the relative positive and negative likelihood ratios by using the proposed closed-formed formulas and the numerical method, and present results in table.
The precision level can be specified at line 154 and 161. Nominal level is set at 0.1.  
```
#### Version information
```
R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS:   /share/pkg.7/r/4.0.2/install/lib/libopenblasp-r0.3.7.so
LAPACK: /share/pkg.7/r/4.0.2/install/lib/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] RColorBrewer_1.1-2 ggplot2_3.3.2      plyr_1.8.6         dplyr_1.0.1       
[5] metafor_3.0-2      Matrix_1.2-18      meta_4.20-2       

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6         compiler_4.0.2     pillar_1.4.6       nloptr_1.2.2.2    
 [5] mathjaxr_1.4-0     tools_4.0.2        boot_1.3-25        lme4_1.1-27.1     
 [9] lifecycle_0.2.0    tibble_3.0.3       nlme_3.1-148       gtable_0.3.0      
[13] lattice_0.20-41    pkgconfig_2.0.3    rlang_0.4.10       rstudioapi_0.13   
[17] CompQuadForm_1.4.3 withr_2.2.0        xml2_1.3.2         generics_0.0.2    
[21] vctrs_0.3.2        tidyselect_1.1.0   glue_1.4.1         R6_2.4.1          
[25] minqa_1.2.4        purrr_0.3.4        magrittr_1.5       scales_1.1.1      
[29] ellipsis_0.3.1     MASS_7.3-53.1      splines_4.0.2      colorspace_1.4-1  
[33] munsell_0.5.0      crayon_1.3.4
```
