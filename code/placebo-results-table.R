### Load placebo test results ###

# Education spending

load("results/plots/educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1.rds") # simultaneous

educ.sim <- df1

load("results/plots/educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_0.rds") # staggered

educ.stag <- df1

# Stock

load("results/table-results/stock_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds")

stock.sim <- df1

load("results/table-results/stock_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_0.rds")

stock.stag <- df1

# RBF

load("results/table-results/rbf_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds") # simulatneous

rbf.sim <- df1

load("results/table-results/rbf_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_0.rds") # staggered

rbf.stag <- df1

# Sine

load("results/table-results/sine_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_1.rds") 

sine.sim <- df1

load("results/table-results/sine_N_500_T_2000_numruns_100_num_treated_250_simultaneuous_0.rds") 

sine.stag <- df1