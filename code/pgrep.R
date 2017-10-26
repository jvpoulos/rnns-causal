###################################
# Replication of Panagopoulos & Green (2008)   #
# Appendix Table 2  #
###################################

pg.rep <- fg.ads[fg.ads$year==2005 | fg.ads$year==2006,]

pg.rep <- pg.rep[!is.na(pg.rep$treat),] #subset to experimental cities 

# Recode data
pg.rep$year <- pg.rep$year_exp

# LM estimates 

# Continuous treatment

votediff.lm <- lm(votediff ~ grp_buy + factor(strata) + factor(year), data=pg.rep)

summary(votediff.lm)

# Binary treatment

votediff.lm.binary <- lm(votediff ~ treat + factor(strata) + factor(year), data=pg.rep)

summary(votediff.lm.binary)