### Load placebo test results ###

# Sine

load("results/table-results/sine_N_1500_T_500_numruns_60_num_treated_750_simultaneuous_1.rds")

sine <- df1

# RBF

load("results/table-results/rbf_N_1500_T_500_numruns_60_num_treated_750_simultaneuous_1.rds")

rbf <- df1

# Stock

load("results/table-results/stock_N_1500_T_500_numruns_60_num_treated_750_simultaneuous_1.rds")

stock <- df1

# Education spending

load("results/table-results/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_1.rds")

educ <- df1