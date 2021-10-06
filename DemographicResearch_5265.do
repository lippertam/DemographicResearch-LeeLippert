/***********************************************

Code for Demographic Research #5265:
Food Insecurity among Homeless and Precariously Housed Children in the 
United States: Lessons from the Past

Authors: Barrett A. Lee & Adam M. Lippert

Data used: National Survey of Homeless Assistance Providers and Clients (NSHAPC) 

What this file does:
1. Apply sample selection criteria
2. Recode study variables
3. Multiply impute data via chained equations
4. Perform descriptive and multivariable analyses
5. Estimate marginal mean predicted probabilities of outcomes for graphing in R

***********************************************/

*Set pathname to project folder with data
*cd "directory name"

use "nshapc.dta", clear /*read in raw NSHAPC data*/


*Sample selection-----------------------------------

  
*Tabulate number of child dependents present with R (via self-reports)
recode H2105 H2205 H2305 H2405 H2505 H2605 H2705 (2=0)
egen kidcount=rowtotal(H2105 H2205 H2305 H2405 H2505 H2605 H2705)
gen kidpresent=(kidcount !=0)
tab kidpresent, m /*761 report having 1+ kids in their care*/

*Remove Rs with no children present
drop if kidpresent==0

*Remove Rs apart of rural oversample (and for whome sample weights unavailable)
drop if revdef==1

*Remove Rs who had previously completed the survey or could not finish interview
drop if H0023 !=2 | H0025==3


*Recodes---------------------------------------------


/*---------------------------------------

FOOD INSECURITY 

Individual items tapping types of food insecurity
are coded first, and combined into a single indicator
of *any* form of insecurity. Item-specific
experiences must have occured in the past month.

---------------------------------------*/

*----------------------------------------
*Adult food insecurity - AFI

*felt hungry
gen afi_felthung=1 if H1185==1 & H1186==1 
replace afi_felt=0 if H1185==2 | (H1185==1 & H1186==2)

*went whole day without eating
gen afi_whday=1 if H1188==1
replace afi_wh=0 if H1188==2

*typically consumes fewer than 3 meals per day
gen afi_3x=1 if inlist(H1184,1,2,3)
replace afi_3x=0 if inlist(H1184,4,5,6,7)

gen afi_composite=1 if afi_felt==1 | afi_whday==1 | afi_3x==1
replace afi_comp=0 if afi_felt==0 & afi_whday==0 & afi_3x==0
*


*Child food insecurity - CFI
*has skipped meal b/c couldn't get them enough to eat
gen cfi_skip=1 if H1305==1
replace cfi_skip=0 if H1305==2
replace cfi_skip=0 if H1183==1 & cfi_skip==.
*was hungry but couldn't get food
gen cfi_hungry=1 if H1307==1
replace cfi_hungry=0 if H1307==2
replace cfi_hung=0 if H1183==1 & cfi_hung==.
*went whole day without eating
gen cfi_whday=1 if H1309==1
replace cfi_wh=0 if H1309==2
replace cfi_wh=0 if H1183==1 & cfi_wh==.
*typically eats fewer than 3x per day (not used in composite CFI indicator)
gen cfi_3x=1 if inlist(H1304,1,2,3)
replace cfi_3x=0 if inlist(H1304,4,5,6,7)

gen cfi_comp=1 if cfi_skip==1 | cfi_whday==1 | cfi_hungry==1
replace cfi_comp=0 if cfi_skip==0 & cfi_whday==0 & cfi_hungry==0


* AFI-CFI CONCORDANCE VAR

egen fi_concord=concat(afi_comp cfi_comp)
tab fi_concord, m
destring fi_concord, force replace
tab fi_concord, m
recode fi_concord (10=1) (1=2) (11=2)
replace fi_concord=. if cfi_comp==.
tab fi_concord, m


*----------------------------------------

*Define homelessness status
recode homlss (1=0) (2=1) (3=2)
lab def homlss 0"currently" 1"formerly" 2"never"
lab val homlss homlss
tab homlss, m

  gen currently=(homlss==0)
  replace currently=. if homlss==.
  tab currently, m

*----------------------------------------  
/*Determine the age(s) of children in R's care 
  H2105 =1 if child present
  H2101-H2701 =age (in years) of children 1-7 
  H2102-H2702 =age (in mos) of children 1-7
    These are only nonmissing when a child is <1 year, in which case any
	valid integer indicates presence of an infant <1 year old
*/

  gen kidage1 = H2101 if H2102==.
    replace kidage1 = 0 if H2102 !=.
	replace kidage1 = . if H2105 !=1
  gen kidage2 = H2201 if H2202==.
    replace kidage2 = 0 if H2202 !=.	
	replace kidage2 = . if H2205 !=1
  gen kidage3 = H2301 if H2302==.
    replace kidage3 = 0 if H2302 !=.
	replace kidage3 = . if H2305 !=1
  gen kidage4 = H2401 if H2402==.
    replace kidage4 = 0 if H2402 !=.	
	replace kidage4 = . if H2405 !=1
  gen kidage5 = H2501 if H2502==.
    replace kidage5 = 0 if H2502 !=.
	replace kidage5 = . if H2505 !=1
  gen kidage6 = H2601 if H2602==.
    replace kidage6 = 0 if H2602 !=.
	replace kidage6 = . if H2605 !=1
  gen kidage7 = H2701 if H2702==.
    replace kidage7 = 0 if H2702 !=.
	replace kidage7 = . if H2705 !=1
	
egen kidmax=rowmax(kidage1 - kidage7)
egen kidmin=rowmin(kidage1 - kidage7)

gen kidages=.
replace kidages=0 if kidmax <=5 & kidmin <=5
replace kidages=1 if kidmax >5 & kidmin >5
replace kidages=2 if kidmax >5 & kidmin <=5
replace kidages=. if kidmin==. | kidmax==.
lab def kidages 0"all <=5" 1">5" 2"mix"
lab val kidages kidages 
tab kidages, m

*----------------------------------------
/*Determine school attendance status of children in R's care 
  We define 'school' as elementary, middle, or high school; pre-school,, k, or 
  pre-k; or daycare. Per NSHAPC questionnaire design, Rs were asked about 
  k/pre-k/pre-school attendance for children ages 3, 4, or 5 only. Questions re:
  elementary/middle/high school were asked for children 6+. A question re: 
  daycare attendance was asked in reference to all kids <18.
  
  Examples:
  H2111 - elementary/middle/high school attendance
  H2113 - pre-school/k/pre-k/Head Start
  H2129 - Daycare
  
  Note: Rs were asked about the frequency of their children's attendance at
  school. We record in the affirmative those responses indicating frequent 
  attendance (as opposed to 'Enrolled - does not attend' or 'Attends but not
  regularly , misses a lot')
*/

gen kid1sch=1 if H2105==1 & kidage1>5 & H2111==1
replace kid1sch=0 if H2105==1 & kidage1>5 & inlist(H2111,2,3)
replace kid1sch=1 if H2105==1 & kidage1 <6 & inlist(H2113,1,2,3,4)
replace kid1sch=0 if H2105==1 & kidage1 <6 & inlist(H2113,5,6)
replace kid1sch=1 if H2105==1 & H2129==1 & kid1sch==.
replace kid1sch=0 if H2105==1 & H2129==2 & kid1sch==.
replace kid1sch=. if H2105 !=1
tab kid1sch, m

gen kid2sch=1 if H2205==1 & kidage2>5 & H2211==1
replace kid2sch=0 if H2205==1 & kidage2>5 & inlist(H2211,2,3)
replace kid2sch=1 if H2205==1 & kidage2 <6 & inlist(H2213,1,2,3,4)
replace kid2sch=0 if H2205==1 & kidage2 <6 & inlist(H2213,5,6)
replace kid2sch=1 if H2205==1 & H2229==1 & kid2sch==.
replace kid2sch=0 if H2205==1 & H2229==2 & kid2sch==.
replace kid2sch=. if H2205==0
tab kid2sch, m

gen kid3sch=1 if H2305==1 & kidage3>5 & H2311==1
replace kid3sch=0 if H2305==1 & kidage3>5 & inlist(H2311,2,3)
replace kid3sch=1 if H2305==1 & kidage3 <6 & inlist(H2313,1,2,3,4)
replace kid3sch=0 if H2305==1 & kidage3 <6 & inlist(H2313,5,6)
replace kid3sch=1 if H2305==1 & H2329==1 & kid3sch==.
replace kid3sch=0 if H2305==1 & H2329==2 & kid3sch==.
replace kid3sch=. if H2305==0
tab kid3sch, m

gen kid4sch=1 if H2405==1 & kidage4>5 & H2411==1
replace kid4sch=0 if H2405==1 & kidage4>5 & inlist(H2411,2,3)
replace kid4sch=1 if H2405==1 & kidage4 <6 & inlist(H2413,1,2,3,4)
replace kid4sch=0 if H2405==1 & kidage4 <6 & inlist(H2413,5,6)
replace kid4sch=1 if H2405==1 & H2429==1 & kid4sch==.
replace kid4sch=0 if H2405==1 & H2429==2 & kid4sch==.
replace kid4sch=. if H2405==0
tab kid4sch, m

gen kid5sch=1 if H2505==1 & kidage5>5 & H2511==1
replace kid5sch=0 if H2505==1 & kidage5>5 & inlist(H2511,2,3)
replace kid5sch=1 if H2505==1 & kidage5 <6 & inlist(H2513,1,2,3,4)
replace kid5sch=0 if H2505==1 & kidage5 <6 & inlist(H2513,5,6)
replace kid5sch=1 if H2505==1 & H2529==1 & kid5sch==.
replace kid5sch=0 if H2505==1 & H2529==2 & kid5sch==.
replace kid5sch=. if H2505==0
tab kid5sch, m

gen kid6sch=1 if H2605==1 & kidage6>5 & H2611==1
replace kid6sch=0 if H2605==1 & kidage6>5 & inlist(H2611,2,3)
replace kid6sch=1 if H2605==1 & kidage6 <6 & inlist(H2613,1,2,3,4)
replace kid6sch=0 if H2605==1 & kidage6 <6 & inlist(H2613,5,6)
replace kid6sch=1 if H2605==1 & H2629==1 & kid6sch==.
replace kid6sch=0 if H2605==1 & H2629==2 & kid6sch==.
replace kid6sch=. if H2605==0
tab kid6sch, m

gen kid7sch=1 if H2705==1 & kidage7>5 & H2711==1
replace kid7sch=0 if H2705==1 & kidage7>5 & inlist(H2711,2,3)
replace kid7sch=1 if H2705==1 & kidage7 <6 & inlist(H2713,1,2,3,4)
replace kid7sch=0 if H2705==1 & kidage7 <6 & inlist(H2713,5,6)
replace kid7sch=1 if H2705==1 & H2729==1 & kid7sch==.
replace kid7sch=0 if H2705==1 & H2729==2 & kid7sch==.
replace kid7sch=. if H2705==0
tab kid7sch, m

*Create categorical indicator of school attendance
*if ratio of kids present to kids in school = 1:1 then kidschoolcat=2
*if ratio of kids present to kids in school = 1:0 then kidschoolcat=0
*all else with nonmissing = 1 (some in school, some not)

egen kidsccount=rowtotal(kid1sch - kid7sch)
gen temp=kidsccount/kidcount
gen kidschoolcat=0 if temp==0
replace kidschoolcat=2 if temp==1
replace kidschoolcat=1 if temp >0 & temp <1
*set missing for kidschoolcat if kid# is present with R but school data =.
replace kidschoolcat=. if H2105==1 & kid1sch==.
replace kidschoolcat=. if H2205==1 & kid2sch==.
replace kidschoolcat=. if H2305==1 & kid3sch==.
replace kidschoolcat=. if H2405==1 & kid4sch==.
replace kidschoolcat=. if H2505==1 & kid5sch==.
replace kidschoolcat=. if H2605==1 & kid6sch==.
replace kidschoolcat=. if H2705==1 & kid7sch==.
drop temp
lab def kidschoolcat 0"none" 1"some" 2"all"
lab val kidschoolcat kidschoolcat
tab kidschoolcat, m


gen stayedwith=1 if H0420==1 | H0427==1 | H0434==1
replace stayedwith=0 if H0420==2 & H0427==2 & H0434==2 & stayed==.
replace stayedwith=1 if H0930==1 | H0937==1 | H0944==1 & stayed==.
replace stayedwith=0 if H0930==2 & H0937==2 & H0944==2 & stayed==.
replace stayedwith=0 if homlss==2
tab stayed, m

*----------------------------------------

*Parent's exposure to abuse in childhood
*Deprived of food as a child
gen child_food=1 if H1600==1
replace child_food=0 if H1600==2
tab child_food, m

*Physically abused as a child
gen child_pabu=1 if H1601==1
replace child_pabu=0 if H1601==2
tab child_pabu, m

*Sexually abused as a child
gen child_sabu=1 if H1602==1
replace child_sabu=0 if H1602==2
tab child_sabu, m

*Binary indicator =1 if experienced any of the above
gen abuse=1 if child_food==1 | child_pabu==1 | child_sabu==1
replace abuse=0 if child_food==0 & child_pabu==0 & child_sabu==0

*----------------------------------------

*Gen binary indicator of any alcohol or drug use problems
gen adprob=1 if ALC_NOW==1 | DRG_NOW==1
replace adprob=0 if ALC_NOW==0 & DRG_NOW==0

*----------------------------------------
*Mental health problems index (needs no recoding - just checking the mean here)
mean MH_ASI

*----------------------------------------
*Victimization while homeless (includes either property crime or assault)
gen adult_theft=1 if H1596==1 | H1597==1
replace adult_theft=0 if H1596==2 & H1597==2
replace adult_theft=0 if homlss==3

gen adult_aslt=1 if H1598==1 | H1599==1
replace adult_aslt=0 if H1598==2 & H1599==2
replace adult_aslt=0 if homlss==3

gen victim=(adult_as==1 | adult_th==1) 
replace victim=. if adult_as==. | adult_th==.
replace victim=0 if homlss==2

*----------------------------------------
*Ever incarcerated (=1 if so)
gen incarcerate=H1592==1 | H1594==1
replace incarcer=0 if H1592==2 & H1592==2
replace incarcer=. if H1592==. & H1592==.

*----------------------------------------
*Indicator of parent's educational adversity (held back, expelled, or suspended)
gen educadv=0 if (H1131==2 & H1130==2 & H1127==1)
replace educadv=1 if (H1131==1 | H1130==1 | inlist(H1127,2,3))


*----------------------------------------
*Dummy indicator of parent having lived in foster care as a child 
*Note: items used for this measure were separated in q'aire by homeless status
gen foster=1 if (H0574==1 | H0577==1 | H0575==1 | H0576==1 | H0578==1 | H0579==1) & homlss==0
replace foster=1 if (H1039==1 | H1040==1 | H1041==1 | H1042==1 | H1043==1 | H1044==1) & homlss==1
replace foster=0 if (H0574==2 & H0577==2 & H0575==2 & H0576==2 & H0578==2 & H0579==2) & homlss==0 
replace foster=0 if (H1039==2 & H1040==2 & H1041==2 & H1042==2 & H1043==2 & H1044==2) & homlss==1
replace foster=0 if homlss==2

*----------------------------------------
*HEALTH PROBLEMS

gen diabetes=1 if H1446==1
replace diabetes=0 if H1446==2

gen hyperten=1 if H1448==1
replace hyperten=0 if H1448==2

gen arthritis=1 if H1451==1
replace arth=0 if H1451==2

gen healthsum=diabetes + hyper + arth
mean healthsum

*----------------------------------------

**recode Urban Institute's education var
gen educ=highsch-1
lab def educ 0"<hs" 1"hs or ged" 2"more than hs"
lab val educ educ
tab educ, m

*----------------------------------------
**employment
gen working=0 if H1311==2
replace working=2 if H1311==1 & (H1312==1 | H1313==2) & working==.
replace working=1 if H1311==1 & (H1314==3 | H1315==4 | H1316==5) & working==.
lab def working 0"not working" 1"temp job" 2"steady job"
lab val working working

*----------------------------------------
*worklife (=1 if R reported working half or more of their life)
gen worklife=(H1344 <=2)
replace worklife =. if H1344==.

*----------------------------------------
*socsupport - $ help from spouse, parents, relatives, friends in past month
gen socsupport=(H1360==1 | H1361==1 | H1362==1 | H1363==1)
replace socsupport=. if H1360==. | H1361==. | H1362==. | H1363==.

*----------------------------------------
*relationship status
gen spousepart=(H1134==1 | H1135==2) /*accompanied by spouse or partner*/

*----------------------------------------
**food stamp amount in $25 increments
gen x=0 if H1374==2
replace x=H1375 if H1374==1
gen foodstamp25=x/25
drop x
tab foodstamp25, m

*----------------------------------------
*current gov't assistance (no need to recode - just checking distribution here)

tab GBX_NOW, m

*----------------------------------------
* housing subsidy
*H1377 (CURRENTLY RECEIVES)
*h1387 (EVER RECIEVED HOUSING ASST - SECTION 8, PUB HOUSING, TENANT ASST, VOUCHERS)
gen housingsub=1 if H1377==1 | H1387==1
replace housingsub=0 if H1377==2 & H1387==2
replace housingsub=0 if H1387==2 & housingsub==. /* just fills in missing for currently homeless, who obvi don't currently receive housing subs*/

*----------------------------------------
*Assistance in-kind
gen helpclothing=(H1405==1)
gen helptransit=(H1406==2)
gen helplegal=(H1407==3)
gen helpdomviol=(H1408==4)
gen helpmoneymgmt=(H1409==5)
gen helpgovasst=(H1410==6)
gen helpfindjob=(H1411==7)
gen helpjobtrain=(H1412==8)
gen helphousing=(H1413==9)
gen helpbills=(H1414==10)
gen helpparenting=(H1415==11)
gen helpresolvedispute=(H1416==12)
gen helpjob2=(helpfind==1 | helpjob==1)
gen helplegal2=(helpleg==1 | helpres==1)

egen helpsum=rowtotal(helpclothing helptransit helplegal helpdomviol helpmoneymgmt helpgovasst helpfindjob helpjobtrain helphousing helpbills helpparenting helpresolvedispute)
mean helpsum

*----------------------------------------
*met dental/medical needs of children
gen metdentmed=1 if H1583==2 & H1559==2
replace metdentmed=0 if H1583==1 | H1559==1

*----------------------------------------
*Specific living situation (e.g., if homeless, R in: shelter, other setting)

gen livesitch=1 if inlist(screen,1,2)
replace livesitch=0 if inlist(screen,10,11)
replace livesitch=2 if inlist(screen,3,4,5,6,7,8,9,12)
lab def livesitch 0"own, incl transitional housing" 1"emerg or tran shel" 2"other"
lab val livesitch livesitch

*replace missing on livesitch=1 (shelter) using 'frame' (applies to 2 cases)
replace livesitch=1 if frame==2 & livesitch==.

*----------------------------------------

**recode Urban Institute's urbn vs rural var
recode urbrural (1=0) (2=1) (3=2)
lab def urbrural 0"central city" 1"suburban/urban fringe" 2"rural"
lab val urbrural urbrural

*----------------------------------------
**Age in years
/* Note: One case reported no knowing own age. Interviewer asked to estimate R 
age from a categorical list of age ranges. We use the mid-point for this one
case, which the interviewer estimated to be between 18-29 years old (the mid-
point of '24' is used for this resopndent)*/

gen age=H0021
replace age=24 if H0021==97 & H0022==2

*----------------------------------------
**Sex
gen female=1 if H1084==2
replace female=0 if H1084==1

*----------------------------------------
**Race/ethnicity
tab race, gen(r)
rename r1 white
rename r2 black
rename r3 hispanic
rename r4 other
replace other=1 if r5==1 & other==0
drop r5
tab white, m
tab black, m
tab hispanic, m
tab other, m
gen race2=race-1
  recode race2 (3/4=3)
  tab race2, m

*replace missing on race2 (set one missing case to '1' (black) b/c evidence
*from other questionnaire items indicate '1')
replace race2=1 if race2==.

*----------------------------------------
**income, from all sources
gen income2=incgrp
recode income2 (5/8=5) (97=.)
lab def income2 0"none" 1"<100" 2"100-299" 3"300-499" 4"500-699" 5"700+"
lab val income2 income2


*----------------------------------------

* Auxilliary variables for imputation

*----------------------------------------

* visited outreach worker or drop-in center past 7 days
gen outreachdropin=1 if (H0297==1 | H0305==1)
replace outreach=0 if outreach==. & (H0297==2 & H0305==2) 
replace outreach=1 if outreach==. & (H0799==1 | H0807==1)
replace outreach=0 if outreach==. & (H0799==2 & H0807==2)


*----------------------------------------
* extreme income sources - peddling, plasma, begging, illegal activities
gen extremeinc=1 if H1364==1 | H1365==1 | H1366==1 | H1367==1
replace extremeinc=0 if H1364==2 & H1365==2 & H1366==2 & H1367==2

*----------------------------------------
* medicaid receipt
gen medicaid=1 if H1399==1 
replace medicaid=0 if H1399==2

*----------------------------------------
* last checkup (ordinal - higher values = longer time to last checkup)
gen lastcheckup=H1467-1
recode lastcheckup (80/100=.)

*----------------------------------------
* top needs

gen needs=0 if inlist(H1814,15,16,17,18)
replace needs=1 if inlist(H1814,10,13,14)
replace needs=2 if H1814==1
replace needs=3 if needs==.
lab def needs 0"housing related" 1"educ/job training" 2"food assist" 3"other"
lab val needs needs

*----------------------------------------

* IMPUTATION

*----------------------------------------

*Direct to path where the imputed data should be saved
cd "C:\Users\adam\Documents\Research\Homelessness\R03 - Food Insecurity\data"

mi set flong

gen cfimiss=(cfi_comp==.)
tab cfimiss, m


mdesc cfi_comp afi_comp kidcount kidages spousepart ///
educ working worklife socsupport stayedwith MH_ASI adprob victim incarcer ///
abuse educadv foster chronic foodstamp25 housingsub GBX_NOW helpsum metdentmed ///
kidschoolcat female age race2 currently livesitch urbrural

mark nomiss
markout nomiss cfi_comp afi_comp kidcount kidages spousepart ///
educ working worklife socsupport stayedwith MH_ASI adprob victim incarcer ///
abuse educadv foster chronic foodstamp25 housingsub GBX_NOW helpsum metdentmed ///
kidschoolcat female age race2 currently livesitch urbrural
tab nomiss, m 
/*33% missing some data, only five covariates with more than 5% missing 
(never more than 11.6%, the fraction observed for our measure of adult 
victimization*/


#delimit ;
mi register imputed educ working worklife socsupport stayedwith victim educadv 
foster abuse foodstamp25 metdentmed housingsub kidschoolcat female outreach 
livesitch race2 incarcerate afi_comp cfi_comp kidmin kidmax  extreme ;

mi register regular MH_ASI age kidcount helpsum homlss urbrural 
spousepart adprob GBX_NOW chronic currently needs; 



mi impute chained 
(regress) foodstamp25 kidmin kidmax
(mlogit)  educ working kidschoolcat livesitch race2 
(logit) worklife socsupport stayedwith  
victim educadv foster abuse metdentmed incarcerate  
housingsub female cfi_comp afi_comp extreme
= MH_ASI age kidcount helpsum  urbrural spousepart adprob GBX_NOW 
chronic currently needs,
add(50) rseed(364334) augment noisily ;


#delimit cr
save nshapcimputed_paper1_FIimpute.dta, replace


*----------------------------------------

* POST-IMPUTATION PROCESSING AND ANALYSIS

*----------------------------------------


cd "C:\Users\adam\Documents\Research\Homelessness\R03 - Food Insecurity\data"
use nshapcimputed_paper1_FIimpute.dta, clear


*----------------------------------------
*Difficulty imputing 'kidages' directly, so 'kidmin' and 'kidmax were used.
*These are used below to replace missingness on 'kidages'

replace kidages=0 if kidmax <=5 & kidmin <=5 & kidages==.
replace kidages=1 if kidmax >5 & kidmin >5 & kidages==.
replace kidages=2 if kidmax >5 & kidmin <=5 & kidages==.
replace kidages=2 if kidmax <=5 & kidmin >5 & kidages==.
tab kidages if _mi_m==2, m

*----------------------------------------
*Recode out-of-bounds imputed values on foodstamp25
recode foodstamp25 (-10/0=0) (16/25=16)

*----------------------------------------
*Apply probability weight
mi svyset [pweight=cliwgt]


*----------------------------------------
*Univariate descriptives

*FI outcomes (Table 2)
mi estimate, esampvaryok: svy: mean afi_felthung 
mi estimate, esampvaryok: svy: mean afi_whday 
mi estimate, esampvaryok: svy: mean afi_3x 
mi estimate, esampvaryok: svy: mean afi_comp 
mi estimate, esampvaryok: svy: mean cfi_skip 
mi estimate, esampvaryok: svy: mean cfi_hungry 
mi estimate, esampvaryok: svy: mean cfi_whday 
mi estimate, esampvaryok: svy: mean cfi_comp
*again, by homeless status (currently unhoused vs never/previously homeless)
mi estimate, esampvaryok: svy: mean afi_felthung, over(currently)
mi estimate, esampvaryok: svy: mean afi_whday , over(currently)
mi estimate, esampvaryok: svy: mean afi_3x , over(currently)
mi estimate, esampvaryok: svy: mean afi_comp , over(currently)
mi estimate, esampvaryok: svy: mean cfi_skip , over(currently)
mi estimate, esampvaryok: svy: mean cfi_hungry , over(currently)
mi estimate, esampvaryok: svy: mean cfi_whday , over(currently)
mi estimate, esampvaryok: svy: mean cfi_comp, over(currently)


*----------------------------------------
*Covariates (Table 1)

mi estimate, esampvaryok: svy: mean kidcount spousepart worklife socsupport ///
stayedwith chronic MH_ASI adprob victim incarcerate abuse educadv foster ///
foodstamp housingsub GBX_NOW helpsum metdentmed female age currently 

mi estimate, esampvaryok: svy: proportion kidages educ working kidschoolcat ///
race2 livesitch urbrural

*again, by homelessness status
mi estimate, esampvaryok: svy: mean cfi_comp afi_comp kidcount spousepart worklife socsupport ///
stayedwith chronic MH_ASI adprob victim incarcerate abuse educadv foster ///
foodstamp stampbi housingsub GBX_NOW helpsum metdentmed female age, over(homlss)

mi estimate, esampvaryok: svy: proportion kidages educ working kidschoolcat ///
race2 livesitch urbrural, over(homlss)

*Bivariate tests on outcomes/covariates by homelessness status
mi estimate, esampvaryok: svy: logit cfi_comp  b2.homlss
mi estimate, esampvaryok: svy: logit afi_comp  b2.homlss
mi estimate, esampvaryok: svy: reg kidcount  b2.homlss
mi estimate, esampvaryok: svy: logit spousepart  b2.homlss
mi estimate, esampvaryok: svy: logit worklife   b2.homlss
mi estimate, esampvaryok: svy: logit  socsupport b2.homlss
mi estimate, esampvaryok: svy: logit  stayedwith b2.homlss
mi estimate, esampvaryok: svy: reg chronic b2.homlss
mi estimate, esampvaryok: svy: reg MH_ASI  b2.homlss
mi estimate, esampvaryok: svy: logit adprob  b2.homlss
mi estimate, esampvaryok: svy: logit victim  b2.homlss
mi estimate, esampvaryok: svy: logit incarcer  b2.homlss
mi estimate, esampvaryok: svy: logit abuse  b2.homlss
mi estimate, esampvaryok: svy: logit  educadv b2.homlss
mi estimate, esampvaryok: svy: logit  foster b2.homlss
mi estimate, esampvaryok: svy: reg foodstamp  b2.homlss
mi estimate, esampvaryok: svy: logit housingsub  b2.homlss
mi estimate, esampvaryok: svy: logit GBX_NOW  b2.homlss
mi estimate, esampvaryok: svy: reg helpsum  b2.homlss
mi estimate, esampvaryok: svy: logit metdentmed  b2.homlss
mi estimate, esampvaryok: svy: logit female  b2.homlss
mi estimate, esampvaryok: svy: reg age   b2.homlss

mi estimate, esampvaryok: svy: mlogit kidages b2.homlss, baseout(0)
mi estimate, esampvaryok: svy: mlogit kidages b2.homlss, baseout(1)
mi estimate, esampvaryok: svy: mlogit educ b2.homlss, baseout(0)
mi estimate, esampvaryok: svy: mlogit educ b2.homlss, baseout(1)
mi estimate, esampvaryok: svy: mlogit working b2.homlss, baseout(0)
mi estimate, esampvaryok: svy: mlogit working b2.homlss, baseout(1)
mi estimate, esampvaryok: svy: mlogit kidschoolcat b2.homlss, baseout(0)
mi estimate, esampvaryok: svy: mlogit kidschoolcat b2.homlss, baseout(1)
mi estimate, esampvaryok: svy: mlogit urbrural b2.homlss, baseout(0)
mi estimate, esampvaryok: svy: mlogit urbrural b2.homlss, baseout(1)


*----------------------------------------
*Fig 2 estimates
mi estimate, esampvaryok: svy: mean cfi_comp
mi estimate, esampvaryok: svy: mean cfi_comp, over(spousepart)
mi estimate, esampvaryok: svy: mean cfi_comp, over(race2)

*create summative measure of CFI + AFI indicators
gen afi_sum = afi_felthung + afi_whday +afi_3x
mi estimate, esampvaryok: svy, subpop(if afi_comp==1): proportion afi_sum

gen cfi_sum = cfi_skip + cfi_hungry + cfi_whday
mi estimate, esampvaryok: svy, subpop(if cfi_comp==1): proportion cfi_sum

*----------------------------------------
*Regression model shown in Table 3

mi estimate, esampvaryok: svy: logit cfi_comp afi_comp kidcount i.kidages spousepart ///
i.educ i.working worklife socsupport MH_ASI adprob incarcer ///
abuse educadv foster chronic foodstamp25 GBX_NOW helpsum metdentmed ///
i.kidschoolcat female age i.race2 currently i.urbrural

*margins estimation - export these estimates to csv file for plotting in R

mimrgns, predict(pr) at(foodstamp25=(1(1)17)) atmeans
mimrgns kidschoolcat, predict(pr) atmeans



**END**
