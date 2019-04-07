**load dataset
use "~/mayoralelections_final_full.dta"

** in the case that they are present, drop variables to be generated later 
drop difftest2,unemploquin1,unemploquin2,unemploquin3,unemploquin4,unemploquin5,DIFFusmonthcitymonth5*, group2x*

** Table 2
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988   i.year if cityinds==1, cluster(citystabbr)
eststo month

reg incumbentpercent DIFFuspcpincomecitypcpincomerev incomepcpus unemployannualus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 i.year if cityinds==1, cluster(citystabbr) 
eststo pcpincome

reg incumbentpercent  DIFFushpic hpiannualus  unemployannualus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr) 
eststo hpi

reg incumbentpercent DIFFusviolcrimecitycrime crimeviolcrimerateus unemployannualus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo violcrime

reg incumbentpercent DIFFmurderuscity crimemurderandnonmanslaugus unemployannualus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo murders

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus unemployannualus wonws wonsb investmayor logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr) 
eststo wonanyi

estout month pcpincome hpi violcrime murders wonanyi using newtable1.tex, replace cells(b (star fmt(3)) se(par fmt(3))) stats(r2 N, labels($R^{2}$ "N")) label legend varlabels(_cons Constant) style(tex)

** Table 3 

reg incumbentpercent unemployCITY1yearch2 unemploymonthlycity unemploymonthlyus  logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1, cluster(citystabbr)
eststo oneyrch

reg incumbentpercent pcpincCITYch1yr incomepcpcity incomepcpus unemployannualus  logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1, cluster(citystabbr) 
eststo oneyrincch

reg incumbentpercent hpiCITY1yrch hpiannualus hpiannualcity unemployannualus  logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1, cluster(citystabbr) 
eststo oneyrhpich

reg incumbentpercent violentcrimerateCITY1yrch crimeviolcrimeratecity crimeviolcrimerateus unemployannualus  logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1, cluster(citystabbr)
eststo oneyrviolentch

reg incumbentpercent homiciderateCITY1yrch crimemurderandnonmanslaugcity crimemurderandnonmanslaugus unemployannualus  logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1, cluster(citystabbr)
eststo oneyrhomicidech

estout oneyrch oneyrincch oneyrhpich oneyrviolentch oneyrhomicidech using newtable23.tex, replace cells(b (star fmt(3)) se(par fmt(3))) stats(r2 N, labels($R^{2}$ "N")) label legend varlabels(_cons Constant) style(tex)

** Table 4
*mean
sum dailypaper2 

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &dailypaper2==0, cluster(citystabbr)
eststo dailypaper1

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &dailypaper2==1, cluster(citystabbr)
eststo dailypaper2

clear

set seed 1124
set obs 10000
matrix m=(-.013, .016)
matrix sd=(.006, .007)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear

use "~/mayoralelections_final_full.dta"
*mean
sum mainm

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &mainm==0, cluster(citystabbr)
eststo media1

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &mainm==1, cluster(citystabbr)
eststo media2

clear

set seed 1125
set obs 10000
matrix m=(.003, .017)
matrix sd=(.005, .006)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear

**2. Table 5:robustness of effect
use "~/mayoralelections_final_full.dta"
reg incumbentpercentch DIFFusmonthcitymonth unemploymonthlyus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo altdv

reg incumbentpercent incumbentpercentlag DIFFusmonthcitymonth unemploymonthlyus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo monthlag

reg incumbentpercent incumbentpercentlag DIFFusmonthcityyear unemployannualus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo yearlag

reg incumbentpercent unemploymonthlycity unemploymonthlyus logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo absol

reg incumbentpercent unemploymonthlycity logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  i.year if cityinds==1, cluster(citystabbr)
eststo justcity

estout altdv monthlag yearlag absol justcity using newtable2_3.tex, replace cells(b (star fmt(3)) se(par fmt(3))) stats(r2 N, labels(R^{2} "N")) label legend varlabels(_cons Constant) style(tex)
eststo clear

**appendix:

**summary stats:
sum incumbentpercent incumbentpercentch newretiredef retired2 retired unemploymonthlycity unemployannualus unemploymonthlyus DIFFusmonthcitymonth unemployCITY1yearch2 incomepcpcity incomepcpus DIFFuspcpincomecitypcpincomerev DIFFushpicityhpi pcpincCITYch1yr hpiannualus hpiannualcity hpiCITY1yrch crimeviolcrimeratecity  crimeviolcrimerateus   violentcrimerateCITY1yr DIFFusviolcrimecitycrime crimemurderandnonmanslaugcity crimemurderandnonmanslaugus DIFFmurderuscity homiciderateCITY1yrch logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 mainmediamarket dailypaper2 incumbentpartyd investmayor wonws wonsb
**Table 6 descriptive stats 
sutex, minmax
**Table 7
gen nolim=1 if incumbentpercent!=.
sutex if nolim==1, minmax
**Table 8
sutex if cityinds==1, minmax

** Table 10: difference in subsets
***** subsets
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & demvs19882==1, cluster(citystabbr)
eststo demlow

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1 & demvs19882==2, cluster(citystabbr)
eststo demhigh 

clear

set seed 1112
set obs 10000
matrix m=(.016, .005)
matrix sd=(.005, .007)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  year if cityinds==1 & incumbentparty=="R", cluster(citystabbr)
eststo repincumb

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & incumbentparty=="D", cluster(citystabbr)
eststo demincumb

clear

set seed 1115
set obs 10000
matrix m=(.016, .013)
matrix sd=(.011, .006)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &mayorcouncildum==0, cluster(citystabbr)
eststo mayorcouncildum0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & mayorcouncildum ==1, cluster(citystabbr)
eststo mayorcouncildum1

clear

set seed 1117
set obs 10000
matrix m=(.011, .009)
matrix sd=(.006, .005)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & atlarge ==0, cluster(citystabbr)
eststo atlarge0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & atlarge ==1, cluster(citystabbr)
eststo atlarge1

clear

set seed 1118
set obs 10000
matrix m=(.026, -.019)
matrix sd=(.007, .034)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & bywardordist ==0, cluster(citystabbr)
eststo wardordist0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 & bywardordist ==1, cluster(citystabbr)
eststo wardordist1

clear

set seed 1119
set obs 10000
matrix m=(.004, .039)
matrix sd=(.009, .011)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & partyballot2 ==0, cluster(citystabbr)
eststo pb1
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & partyballot2 ==1, cluster(citystabbr)
eststo pb2

clear

set seed 1120
set obs 10000
matrix m=(.013, -.002)
matrix sd=(.006, .010)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & partisan2 ==0, cluster(citystabbr)
eststo partisan20

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & partisan2 ==1, cluster(citystabbr)
eststo partisan21

clear

set seed 1121
set obs 10000
matrix m=(.011, .022)
matrix sd=(.005, .022)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & term==0, cluster(citystabbr)
eststo term0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988 year if cityinds==1 & term==1, cluster(citystabbr)
eststo term1

clear

set seed 1122
set obs 10000
matrix m=(.011, .011)
matrix sd=(.009, .005)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &nov==0, cluster(citystabbr)
eststo nov0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &nov==1, cluster(citystabbr)
eststo nov1

clear

set seed 1122
set obs 10000
matrix m=(.014, .008)
matrix sd=(.007, .005)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear
use "~/mayoralelections_final_full.dta"
reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &localandnov==0, cluster(citystabbr)
eststo localnov0

reg incumbentpercent DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988 year if cityinds==1 &localandnov==1, cluster(citystabbr)
eststo localnov1

clear

set seed 1123
set obs 10000
matrix m=(.011, .008)
matrix sd=(.005, .009)
drawnorm xvar xvar2, n(10000) means(m) sds(sd)

**summarize the simulated coefficients
summ xvar if xvar <0
summ xvar2 if xvar2 <0

**flag observations where x1 > x2 and summarize
gen x1_xvar = 0
replace x1_xvar = 1 if xvar > xvar2
*p value
summ x1_xvar

clear

**table 11 predicting retirements
use "~/mayoralelections_final_full.dta"
logit newretiredef DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  victories i.year, cluster(citystabbr)
eststo newretire

logit retired2 DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum demvs1988  victories retireracewhite retirepartyd i.year, cluster(citystabbr)
eststo unspecmonann

logit retired DIFFusmonthcitymonth unemploymonthlyus CENSUS2000medhhinc logCENSUS2000pop CENSUS2000hispanicpercent CENSUS2000blackpercent CENSUS2000bachdegplus CENSUS2000medhomevalue CENSUS1990medhhinc mayorcouncildum  demvs1988  victories retireracewhite retirepartyd i.year, cluster(citystabbr)
eststo allretiremonann

estout newretire unspecmonann allretiremonann using retire32.tex, replace cells(b (star fmt(3)) se(par fmt(3))) stats(r2 N, labels(R^{2} "N")) label legend varlabels(_cons Constant) style(tex)
eststo clear


** export csv
export delimited using "mayoralelections_final_full", replace
