/*
CPS 1996 Estimates
Basic demographics and poverty status
Adult and child FI + concordance
Sampling weights
*/

* Household weight: hwhhwgt
* Family income:    hufaminc
* Above/below 185%: hrpoor
* Person age: 	 	peage

use "C:\Users\adam\Documents\Research\Homelessness\R03 - Food Insecurity\data\CPS\cpssep1996.dta"

*drop missing cases from sample
drop if hrpoor==-1

*drop if not contiguous US
drop if inlist(gestcen, 94,95)

/*-----------------
Recodes 
------------------*/

*Determine presence and number of children present in HH
*first, household size
sort hrhhid
by hrhhid: gen hhsize=_N
*now, kid count and 1/0 presence of kids in HH
gen kiddummy=1 if peage<=17
  replace kiddummy=. if peage <0 | peage==.
egen kidcount=sum(kiddummy), by(hrhhid)
  sum kidcount
gen kidpresent=(kidcount>=1)
  tab kidpresent, m
  
* Omit households with no children present
drop if kidpresent==0

*FOOD SECURITY AND FOOD RELATED OUTCOMES

*AFI

gen afi_whday=1 if hes30==1
replace afi_wh=0 if hes30==2
replace afi_wh=0 if hes28==2
replace afi_wh=0 if hes16==2
replace afi_wh=0 if hes11a==1 | hes11==1
tab hes16 afi_wh, m 

gen afi_felthung=1 if hes35==1 & hes36==1
replace afi_felthung=0 if hes35==2
replace afi_felthung=0 if hes35==1 & hes36==2 & afi_felthung==.
replace afi_felthung=0 if hes11a==1 | hes11==1
replace afi_felthung=0 if hes16==2 & afi_felthung==.
tab afi_felthung, m 

gen afi_skip=1 if hes26==1
replace afi_skip=0 if hes24==2 | (hes24==1 & hes26==2)
replace afi_skip=0 if hes11a==1 | hes11==1
replace afi_skip=0 if hes16==2 & afi_skip==.
tab hrpoor afi_skip, m row

*CHILD FI

gen cfi_skip=1 if hes45==1
replace cfi_skip=0 if hes43==2 | (hes43==1 & hes45==2)
replace cfi_skip=0 if (hes11a==1 | hes11==1) & cfi_skip==.
replace cfi_skip=0 if hrpoor==2 & cfi_skip==.
tab cfi_skip, m

gen cfi_hungry=1 if hes48==1
replace cfi_hungry=0 if hes47==2 | (hes47==1 & hes48==2)
replace cfi_hungry=0 if (hes11a==1 | hes11==1) & cfi_hungry==.
replace cfi_hungry=0 if hrpoor==2 & cfi_hungry==.
tab cfi_hungry, m

gen cfi_whday=1 if hes51==1
replace cfi_whday=0 if hes50==2 | (hes50==1 & hes51==2)
replace cfi_whday=0 if (hes11a==1 | hes11==1) & cfi_whday==.
replace cfi_whday=0 if hrpoor==2 & cfi_whday==.
tab cfi_whday, m

mark nomiss
markout nomiss afi* cfi*
tab nomiss, m
drop if nomiss==0

gen cfi_comp=1 if cfi_skip==1 | cfi_whday==1 | cfi_hungry==1
replace cfi_comp=0 if cfi_skip==0 & cfi_whday==0 & cfi_hungry==0
tab cfi_comp, m

gen afi_composite=1 if afi_felt==1 | afi_whday==1 | afi_skip==1
replace afi_comp=0 if afi_felt==0 & afi_whday==0 & afi_skip==0
tab afi_comp, m


* FI CONCORDANCE VAR

egen fi_concord=concat(afi_comp cfi_comp)
tab fi_concord, m
destring fi_concord, force replace
tab fi_concord, m
recode fi_concord (10=1) (1=2) (11=2)
replace fi_concord=. if cfi_comp==.
tab fi_concord, m
*------------------------

*create new measures of extreme poverty

*flag those missing on income
gen incmissflag=1 if inlist(hufaminc, -2,-3)
tab incmiss, m


gen atpov=1 if inlist(hufaminc,1,2) & rnumhou==1
replace atpov=1 if inlist(hufaminc,1,2,3) & rnumhou==2
replace atpov=1 if inlist(hufaminc,1,2,3,4) & rnumhou==3
replace atpov=1 if inlist(hufaminc,1,2,3,4,5) & rnumhou==4
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6) & rnumhou==5
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6) & rnumhou==6
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7) & rnumhou==7
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7) & rnumhou==8
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8) & rnumhou==9
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8) & rnumhou==10
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9) & rnumhou==11
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9) & rnumhou==12
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9,10) & rnumhou==13
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9,10) & rnumhou==14
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9,10) & rnumhou==15
replace atpov=1 if inlist(hufaminc,1,2,3,4,5,6,7,8,9,10) & rnumhou==16
replace atpov=0 if atpov==.
replace atpov=. if incmiss==1 | rnumhou==.
tab atpov, m
tab hrpoor atpov, row

**Determine household "head"

gen head=1 if inlist(perrp,1,2)
replace head=0 if inlist(perrp,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
tab head, m

*gen newvar characterizing household type: presence of kids and spouse/partner

gen spousepart=1 if inlist(perrp,3,13,14)

egen head_spousecount=max(spousepart), by(hrhhid)
tab head_spouse, m

gen head_spousepres=1 if head_spousecount==1 & head==1
replace head_spousepres=0 if head_spousecount !=1 & head==1
drop if head==0
tab head_spousepres, m

*recode marital status
gen twoparent=1 if pemaritl==1
replace twoparent=0 if inlist(pemaritl,2,3,4,5,6)
tab twoparent, m

svyset [pweight=hwhhwgt]
svy: proportion fi_concord if atpov !=.
svy: proportion fi_concord, over(atpov)
svy: mean cfi_comp, over(atpov)

tab hrpoor atpov, m
svy: mean afi_whday afi_felthung afi_skip afi_composite cfi_skip cfi_hungry cfi_whday cfi_comp 
svy: proportion  fi_concord
svy: mean afi_whday afi_felthung afi_skip afi_composite cfi_skip cfi_hungry cfi_whday cfi_comp, over(atpov)
svy: proportion  fi_concord, over(atpov)

svy: mean cfi_comp, over(atpov)
svy: mean cfi_comp, over(atpov twoparent)
svy: mean cfi_comp, over(atpov prhspnon)
svy: mean cfi_comp if prhspnon==2, over(atpov perace)

*END



