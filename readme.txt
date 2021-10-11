Demographic Research #5265
Food Insecurity among Homeless and Precariously Housed Children in the 
United States: Lessons from the Past

Authors: Barrett A. Lee & Adam M. Lippert

Data used: National Survey of Homeless Assistance Providers and Clients (NSHAPC) 

Software used: Stata v.16; RStudio v.1.4.1717-3

What's included here:
1. Stata syntax file recoding study variables from NSHAPC data set, implementing multiple imputation, estimating regression models, and retaining marginal mean predicted probabilities of the outcome (child food insecurity) for graphing.

2. Stata syntax file recoding CPS 1996 data for comparison of food insecurity between low-income domiciled families and those from the NSHAPC study. 

3. R Markdown file used to graph predicted probabilities of the outcome (see Figs 3 & 4).


Users can obtain the NSHAPC data from the following (select 'client data'):
https://www.census.gov/data/datasets/1996/demo/nshapc/nshapc-datasets.html

Users can obtain CPS data from the following (select September 1996):
https://www.nber.org/research/data/reading-current-population-survey-cps-data-sas-spss-or-stata


Raw variables used from the NSHAPC file:
H2105-H2705 revdef H0023 H0025 H1185 H1186 H1188 H1184 H1305 H1183 H1307 H1309 H1304 homlss H2101-H2701 H2102-H2702 H2111-H2711 H2113-H2713 H2129 H2729 H0420 H0427 H0434 H1600 H1601 H1602 ALC_NOW DRG_NOW MH_ASI H1596 H1597 H1598 H1599 H1594 H1592 H1131 H1130 H1127 H0574-H0579 H1446 H1448 H1451 educ H1311-H1316 H1344 H1360-H1363 H1134 H1135 H1374 H1375 GBX_NOW H1377 H1387 H1405-H1416 H1583 H1559 screen urbrural H0021 H0022 H1084 race incgrp H0297 H0305 H0297 H0799 H0807 H1364-H1367 H1399 H1467 H1814 

Raw variables used from the CPS file:
hwhhwgt hufaminc hrpoor peage gestcen hrhhid hes30 hes28 hes16 hes11 hes11a hes35 hes36 hes26 hes24 hes45 hes43 hes47 hes48 hes51 hes50 rnumhou perrp pemaritl perace prhspnon

