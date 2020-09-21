### Placebo test results ###

# Education spending

load("results/plots/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_1.rds") # simultaneous

educ.sim <- df1

load("results/plots/educ_pc_N_16_T_148_numruns_100_num_treated_8_simultaneuous_0.rds") # staggered

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

### Benchmark estimator comparison ###

load("results/table-results/educ-benchmark-compare-linear.rds") 
benchmark.estimators.linear <- df1

load("results/table-results/educ-benchmark-compare-locf.rds") 
benchmark.estimators.locf <- df1

load("results/table-results/educ-benchmark-compare-none.rds") 
benchmark.estimators.none <- df1

load("results/table-results/educ-benchmark-compare-median.rds") 
benchmark.estimators.median <- df1

load("results/table-results/educ-benchmark-compare-random.rds") 
benchmark.estimators.random <- df1

load("results/table-results/educ-benchmark-compare-svd.rds") 
benchmark.estimators.svd <- df1
