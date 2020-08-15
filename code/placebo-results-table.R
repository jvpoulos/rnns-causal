### Load placebo test results ###

# Education spending

educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/plots/educ_pc_N_16_T_130_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1[df1$x==0.5,], digits=4)

# Stock

stock <- readRDS("results/plots/stock_N_800_T_125_numruns_100_num_treated_400_simultaneuous_1.rds") 

stock$stderror <- (stock$y -stock$lb)/1.96

print(stock, digits=2)