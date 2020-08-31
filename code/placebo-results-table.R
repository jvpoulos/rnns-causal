### Load placebo test results ###

# Education spending

#load("results/table-results/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_1.rds")
#load("results/table-results/educ_pc_N_16_T_148_numruns_60_num_treated_8_simultaneuous_1.rds") # simultaneous
load("results/table-results/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_1.rds") # simultaneous

educ <- df1

# MNIST
#load("results/table-results/mnist_N_500_T_784_numruns_60_num_treated_250_simultaneuous_1.rds") # simultaneous
load("results/table-results/mnist_N_500_T_784_numruns_100_num_treated_250_simultaneuous_1.rds") # simultaneous

mnist <- df1

# Stock

#load("results/table-results/stock_N_1000_T_500_numruns_100_num_treated_500_simultaneuous_1.rds")
#load("results/table-results/stock_N_500_T_1000_numruns_60_num_treated_250_simultaneuous_1.rds")
#load("results/table-results/stock_N_500_T_2000_numruns_60_num_treated_250_simultaneuous_1.rds")
load("results/table-results/stock_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds")

stock <- df1

#load("results/table-results/stock_N_500_T_1000_numruns_100_num_treated_250_simultaneuous_1.rds")

#stock2 <- df1

# RBF

#load("results/table-results/rbf_N_1500_T_600_numruns_100_num_treated_750_simultaneuous_1.rds")
#load("results/table-results/rbf_N_500_T_1000_numruns_60_num_treated_250_simultaneuous_1.rds")
#load("results/table-results/rbf_N_500_T_2000_numruns_60_num_treated_250_simultaneuous_1.rds") # simultaneous
load("results/table-results/rbf_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds") # simultaneous

rbf <- df1

# load("results/table-results/rbf_N_600_T_1500_numruns_100_num_treated_300_simultaneuous_1.rds")
#load("results/table-results/rbf_N_500_T_1000_numruns_60_num_treated_250_simultaneuous_0.rds")

#rbf2 <- df1

# Sine

#load("results/table-results/sine_N_1500_T_600_numruns_100_num_treated_750_simultaneuous_1.rds")
#load("results/table-results/sine_N_500_T_1000_numruns_60_num_treated_250_simultaneuous_1.rds")
#load("results/table-results/sine_N_500_T_2000_numruns_60_num_treated_250_simultaneuous_1.rds")
load("results/table-results/sine_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds")

sine <- df1

#load("results/table-results/sine_N_600_T_1500_numruns_100_num_treated_300_simultaneuous_1.rds")
#load("results/table-results/sine_N_500_T_1000_numruns_60_num_treated_250_simultaneuous_0.rds")

#sine2 <- df1