# set the "code and data supplements" folder as the main working directory
setwd(".../code and data supplements") 

# install deprecated versions for 'meta' and 'metafor'
devtools::install_version("metafor", "3.0-2")
library("remotes")
library("fs")
remotes::install_github("guido-s/meta", ref = "R-book-first-edition")

# simulate data for 55,000 repetitions
# number of repetitions specified at line 18
# results will appear in the "intermediate results" folder
source("R program/Simulations/1_sim_meta.R")

# compile intermediate results for the simulated 55,000 repetitions
# number of repetitions specified at line 12
# results will appear in the "intermediate results" folder
source("R program/Simulations/2_compile_rslts.R")

# summarize considered scenarios in a table 
# the table will appear in the "intermediate results" folder
source("R program/Simulations/0_write_scenarios.R")

# prepare intermediate results for plots generation
# number of repetitions specified at line 7
# results will appear in the "intermediate results" folder
source("R program/Simulations/3_for_plots.R")

# generate plots
# plots will appear in the "output" folder
source("R program/Simulations/4_gen_plots.R")

# create table for sample size calculation results based on 5,000 repetitions
# number of repetitions specified at line 8
# table will appear in the "output" folder
source("R program/Sample size calculation/3_sample_size_cal.R")


