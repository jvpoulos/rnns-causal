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

print(df1, digits=3)

# W. German reunification
germany_N_16_T_44_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/table-results/germany_N_16_T_44_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=3)

# Education spending

educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1 <- 
  load("results/table-results/educ_pc_N_16_T_156_numruns_100_num_treated_8_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=2)

# Covid

#covid  <- readRDS("results/table-results/covid_N_2710_T_60_numruns_18_num_treated_1355_simultaneuous_1.rds")
covid  <- readRDS("results/table-results/covid_N_1200_T_60_numruns_100_num_treated_600_simultaneuous_1.rds")

covid$stderror <- (covid$y -covid$lb)/1.96

print(covid, digits=4)

# Stock

stock <- readRDS("results/table-results/stock_N_1500_T_100_numruns_100_num_treated_750_simultaneuous_1.rds")

stock$stderror <- (stock$y -stock$lb)/1.96

print(stock, digits=4)