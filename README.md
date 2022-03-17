In this project, we share R code to conduct Monte Carlo simulation studies to compare 6 different tests for small-study effects: 
Begg’s rank correlation test, Egger’s regression test, Schwarzer’s rank correlation test, the trim-and-fill test, the arcsine-Thompson test, and the skewness-regression combined test introduced by Lin and Chu. We also share code to jusitify the minimum required number of repetitions in our simulations to obtain a desired level of precision on the relative positive and negative likelihood ratios. 

### Simulations

0_gen_meta.R
```
R function used to generate data of meta-analyses of randomized trials using Copas and Shi's model.
```
0_tnf.R
```
R function used to perform the trim-and-fill test. 
```
0_skewtest.R
```
R function used to perform Lin and Chu's skewness-regression combined test. 
```
1_sim_meta.R
```
R code used to perform 6 different tests for small-study effects on generated meta-analyses and save p-values separately for each scenario. Scenarios can be defined at line 14-17. Number of repetitions can be specified at line 19. 
```
2_pp_ap_lrs.R
```
R function used to calculate penalized power, adjusted power, and likelihood ratios. 
```
3_compile_rslts.R
```
R code used to compile all simulation results. Nominal level is set at 0.1. 
```
3_write_scenarios.R
```
R code used to summarizing all considered scenarios in a table.
```
4_for_plots.R
```
R code used to prepare simulation results for making plots.
```
5_gen_plots.R
```
R code used to present simulation results in plots.  
```

### Sample size calculation

1_sim_sample_size.R
```
R code used to perform 6 different tests for small-study effects on generated meta-analyses and save p-values separately for each scenario with a smaller sample size. Scenarios can be defined at line 14-17. Number of repetitions can be specified at line 19. 
```
2_sample_size_cal.R
```
R code used to calculate the minimum required sample size to achieve a certain level of relative precision on the relative positive and negative likelihood ratios.
The precision level can be specified at line 128, 132, 167, 168. Nominal level is set at 0.1.  
```
