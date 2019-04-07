/* analysis of Mayors 2005-2006 experiment Panagopoulos and Green March 3, 2006 */  

gen year=year_exp 

gen dummy2006=year-2005  

gen x70=strata70*dummy2006 

gen x90=strata90*dummy2006  

/* Randomization check */  

xi: regress grp_buy  strata70 strata90 dummy2006 if contested_2006==1 & inc_running_2006==1 

xi: regress grp_buy  strata70 strata90 x70 x90 dummy2006 inc_voteshare_prev partisan to_prev statewide_2005 if contested_2006==1  & inc_running_2006==1    

xi: regress grp_buy strata70 strata90 dummy2006 inc_voteshare_prev partisan to_prev statewide_2005 if contested_2006==1  & inc_running_2006==1
testparm inc_voteshare_prev partisan to_prev statewide_2005

/* analyze Incumbents' vote share */  

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 if contested_2006==1 & year==2005  

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 if contested_2006==1 & year==2006  

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 if contested_2006==1  

xi: regress votesharechange grp_buy  strata70 strata90 dummy2006 if contested_2006==1     

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1 & year==2005  

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1 & year==2006  

xi: regress votesharechange grp_buy  strata70 strata90 x70 x90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1  

xi: regress votesharechange grp_buy  strata70 strata90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1    

/* show SEs using bootstrapping */  

bootstrap "xi: regress votesharechange grp_buy  strata70 strata90               dummy2006  if contested_2006==1 " _b _se, reps(100000) saving(bsout) replace bca 

bootstrap "xi: regress votesharechange grp_buy  strata70 strata90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1 " _b _se, reps(10000) saving(bsout) replace bca  

/* show the results using multiplicative heteroskedastic regression */

eq lpn: votesharechange grp_buy  strata70 strata90 dummy2006  

eq lpn: votesharechange grp_buy  strata70 strata90 dummy2006 partisan to_prev statewide_2005 

/* show IRLS results */  

rreg votesharechange grp_buy strata70 strata90 dummy2006 if contested_2006==1, tune(4.69) 

rreg votesharechange grp_buy strata70 strata90 dummy2006 partisan to_prev statewide_2005 if contested_2006==1, tune(4.69)    

/* show quantile regression results */ 

bsqreg votesharechange grp_buy strata90 strata70 year_exp if contested_2006==1, quantile(50) reps(1000)   

/* analyze Incumbents' vote margin */  

xi: regress votesharediff grp_buy  strata70 strata90 x70 x90 dummy2006 if contested_2006==1 & year==2005 & inc_running_2006==1 

xi: regress votesharediff grp_buy  strata70 strata90 x70 x90 dummy2006 if contested_2006==1 & year==2006 & inc_running_2006==1 

xi: regress votesharediff grp_buy  strata70 strata90 dummy2006 if contested_2006==1 & inc_running_2006==1   

xi: regress votesharediff grp_buy  strata70 strata90 x70 x90 dummy2006 inc_voteshare_prev partisan to_prev statewide_2005 if contested_2006==1 & year==2005 & inc_running_2006==1 

xi: regress votesharediff grp_buy  strata70 strata90 x70 x90 dummy2006 inc_voteshare_prev partisan to_prev statewide_2005 if contested_2006==1 & year==2006 & inc_running_2006==1 

xi: regress votesharediff grp_buy  strata70 strata90 dummy2006 inc_voteshare_prev partisan to_prev statewide_2005 if contested_2006==1 & inc_running_2006==1  

