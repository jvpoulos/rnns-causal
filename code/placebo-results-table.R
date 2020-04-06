### Load placebo test results ###

# Basque Country
basque_N_16_T_43_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/table-results/basque_N_16_T_43_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=2)

# California smoking
california_N_38_T_31_numruns_100_num_treated_19_simultaneuous_1 <- 
  load("results/table-results/california_N_38_T_31_numruns_100_num_treated_19_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=4)

# W. German reunification
germany_N_16_T_44_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/table-results/germany_N_16_T_44_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=4)

# Education spending

educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/table-results/educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=2)

# Sales

sales <- readRDS("results/table-results/sales_N_500_T_1000_numruns_100_num_treated_250_simultaneuous_1.rds")

sales$stderror <- (sales$y -sales$lb)/1.96

print(sales, digits=3)

# Stock

stock <- readRDS("/media/jason/Dropbox/github/rnns-causal/results/table-results/stock_N_100_T_1000_numruns_100_num_treated_50_simultaneuous_1.rds")

stock$stderror <- (stock$y -stock$lb)/1.96

print(stock, digits=1)
