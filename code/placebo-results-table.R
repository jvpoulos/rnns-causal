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

# Mayoral elections

votediff_N_910_T_66_numruns_100_num_treated_455_simultaneuous_1 <- 
  load("results/table-results/votediff_N_910_T_66_numruns_100_num_treated_455_simultaneuous_1.rds")

df1$stderror <- (df1$y -df1$lb)/1.96

print(df1, digits=3)

# Covid-19
covid19 <- readRDS("results/table-results/covid_N_1000_T_60_numruns_100_num_treated_500_simultaneuous_1.rds")

covid19$stderror <- (covid19$y -covid19$lb)/1.96

print(covid19*100, digits=2)
