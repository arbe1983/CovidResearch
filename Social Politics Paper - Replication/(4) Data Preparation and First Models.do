********************************
*** SOCIAL POLITICS PAPER ***
******************************

* code by Ariane Bertogg & Nevena Kulic *



*****************************
*** Load and Prepare data ***
*****************************

use "C:\Users\xxx\couple_labelled.dta", clear
cd "C:\Users\xxx\Social politics\Results 2201"

set scheme s1mono

* set weights
svyset [pw=gewicht]

* Only women *
drop if sex==1

* Exclude retired *
drop if erw1==10



********************************************************
*** Create or Recode the Variables Used for Analysis ***
********************************************************

*** Recode the work status for the respondent (our treatment variable) ***
ta erw1, nolabel

recode erw1 (1=1 "Full-time")(2=2 "Part-time")(3/11=3 "Inactive and unemployed"), gen (erw1_3)
recode erw1_3 (1/2=1 "Work") (3=2 "Not working"), gen (erw1_2)

* Three categories of work status before Corona *
ta erw1_3 erw1

* The same for the partner *
ta erwp1, nolabel
recode erwp1 (1=1 "Full-time")(2=2 "Part-time")(3/11=3 "Inactive and unemployed"), gen (erwp1_3)
ta erwp1_3



*** Gender Role Attitudes ***
recode geschl4_2 (1 2=1 "agree")(3 4=2 "disagree"), gen (geschl4_2_2)
lab var geschl4_2_2 "Working Mother can have warm relationship"



*** Recode housework and childcare ***
recode housework (1/3=1 "Decreased") (4=2 "The same") (5/7= 3 "Increased"), gen (housework_3) 
recode childcare (1/3=1 "Decreased") (4=2 "The same") (5/7= 3 "Increased"), gen (childcare_3) 

ge childcare_neu=childcare
replace childcare_neu=0 if children==0
recode childcare_neu (1/3=1 "Decreased") (4=2 "The same") (5/7= 3 "Increased"), gen (childcareneu_3) 

lab var childcare "Changes in Childcare due to Covid"
lab var childcare_neu "Changes in Childcare due to Covid (Childless to zero)"
lab var housework "Changes in Housework due to Covid"



*** Three categories of life Satisfaction **
ta zufr2, gen (zufr2_)
lab var zufr2_1 "LS decreased"
lab var zufr2_2 "LS the same"
lab var zufr 2_3 "LS increased"
* use zufr2_1 (decrease) as our dependent variable! *



*** Define Control Variables ***
* Age, civil status (married and cogabiting), east versus west, education, partner's work status household income and children, plus contribution change and relative earnings before crisis *

* with children 
global controls "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income i.children i.contribution_change_3 i.relative_earning ib2.geschl4_1_2 ib1.geschl4_2_2"

* without children
global controls2 "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income i.contribution_change_3 i.relative_earning ib2.geschl4_1_2 ib1.geschl4_2_2"

** Why so? Because we need children as the mediating mechanism, so we want to exclude them and see whether the effects of employment change when we include children (time-availability argument ) **

save, replace



*****************
*** Modelling ***
*****************

* Satisfaction of working and non-working women, all sample


*** Start with Wave 1 Data (Reported May 2020) ***
* Inactive serve as reference cat
* Step 1 (model with all controls and values, but no childcare and housework)
logit zufr2_1 ib3.erw1_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression1

* Step 2 (adding housework and childcare - mediators / time availability)
logit zufr2_1 ib3.erw1_3 ib2.housework_3 ib2.childcare_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression2

coefplot regression1 regression2, xline(0) xtitle(Average marginal effects)


* Change of reference category: Full-time *
* Step 1: Main model (controls plus values)
logit zufr2_1 ib1.erw1_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression3

* Step 2: With housework 
logit zufr2_1 i.erw1_3 i.housework_3 i.childcare_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression4

coefplot regression3 regression4, xline(0) xtitle(Average marginal effects)


* Part-time as a reference in the main model (controls plus values)
* Step 1
logit zufr2_1 ib2.erw1_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression5

* Step 2: With housework 
logit zufr2_1 ib2.erw1_3 ib2.housework_3 ib2.childcareneu_3 $controls [pw=gewicht] if mysample==1&wave==1
margins, dydx(*) post
estimates store regression6
* Yes, this makes the most sense, stick to part-time as the reference category *



*** Now the same for Wave 2 ***
* Keep Part-Time as the Reference *
* Step 1
logit zufr2_1 ib2.erw1_3 $controls2 [pw=gewicht] if mysample==1&wave==2
margins, dydx(*) post
estimates store w2

* Step 2
logit zufr2_1 ib2.erw1_3 ib2.housework_3 ib2.childcareneu_3 $controls2 [pw=gewicht] if mysample==1&wave==2
margins, dydx(*) post
estimates store w2_hwcc



*** Both Waves, Pooled ***
* Step 1
logit zufr2_1 ib2.erw1_3##i.wave $controls2 [pw=gewicht] if mysample==1&wave==2
margins, dydx(*) post
estimates store w1w2

* Step 2
logit zufr2_1 ib2.erw1_3##i.wave ib2.housework_3 ib2.childcareneu_3 $controls2 [pw=gewicht] if mysample==1&wave==2
margins, dydx(*) post
estimates store w1w2_hwcc



*** Check the significance between part time and full time ***
* In wave 1
logit zufr2_1 ib3.erw1_3 i.housework_3 i.childcare_3 $controls i.wave ib3.erw1_3#i.wave [pw=gewicht] if mysample==1

margins, at(erw1_3=1 wave=1) at(erw1_3=2 wave=1) post
test _b[1._at] = _b[2._at]

* In wave 2
logit zufr2_1 ib3.erw1_3 i.housework_3 i.childcare_3 $controls i.wave ib3.erw1_3#i.wave [pw=gewicht] if mysample==1

margins, at(erw1_3=1 wave=2) at(erw1_3=2 wave=2) post
test _b[1._at] = _b[2._at]



***  Plots and Tables ***

*  Estimates for Figure 1 (Wave 1 only) *
coefplot regression5 regression6 || regression1 regression2, drop(_cons) keep(*erw1_3 *housework_3 *childcare_neu3 ) xline(0) xtitle(Average marginal effects) baselevels title("Wave 1")
gr save graph1.gph

* Estimates for Figure 2 (Wave 2 only) * 
coefplot regression6 w2_hwcc, keep(*erw1_3 *housework_3 *childcareneu_3 ) omitted xline(0) xtitle(Average marginal effects) baselevels title("Wave 2")
gr save graph2.gph, replace

* Make nicer graph *
coefplot regression6 w2_hwcc (, keep(*1.erw1_3 :1._predict) label(Full time)) (, keep(*2.erw1_3:1._predict) label(Part time)) (, keep(*3.erw1_3:1._predict) label(Inactive)) , swapnames xline(0) legend(rows(1))  title("Waves 1 and 2")
gr save graph2_withlabels.gph, replace


* Estimates for pooled waves *
coefplot w1w2 w1w2_hwcc (, keep(*1.erw1_3 :1._predict) label(Full time)) (, keep(*2.erw1_3:1._predict) label(Part time)) (, keep(*3.erw1_3:1._predict) label(Inactive)) , swapnames xline(0) legend(rows(1)) title("Pooled waves - with and without time availability"")



************************
*** APPENDIX TABLES ****
************************


*** Stepwise Model Building ***
 (log odds, not AME because of interactions) ***
global controls_tables "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income i.children i.contribution_change_3 i.relative_earning"

* Bring in Gender Role Attitudes separately before Time-Availabiltity *
* First wave 1 than extend sample and interactions *

logit zufr2_1 ib3.erw1_3 $controls_tables [pw=gewicht] if mysample==1&wave==1
outreg using steps, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**)varlabels  replace 

logit zufr2_1 ib3.erw1_3 $controls_tables ib2.geschl4_1_2 ib1.geschl4_2_2[pw=gewicht] if mysample==1&wave==1
outreg using steps, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**) varlabels merge replace 

logit zufr2_1 ib3.erw1_3 $controls_tables ib2.geschl4_1_2 ib1.geschl4_2_2 i.housework_3 i.childcare_3 [pw=gewicht] if mysample==1&wave==1
outreg using steps, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**) varlabels merge replace 

logit zufr2_1 ib3.erw1_3 $controls_tables ib2.geschl4_1_2 ib1.geschl4_2_2 i.housework_3 i.childcare_3 i.wave [pw=gewicht] if mysample==1
outreg using steps1, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**) varlabels replace 

logit zufr2_1 ib3.erw1_3 $controls_tables ib2.geschl4_1_2 ib1.geschl4_2_2 i.housework_3 i.childcare_3 i.wave i.wave#ib3.erw1_3 [pw=gewicht] if mysample==1
outreg using steps1, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**) varlabels merge replace 

logit zufr2_1 ib3.erw1_3 $controls_tables ib2.geschl4_1_2 ib1.geschl4_2_2 i.housework_3 i.childcare_3 i.wave i.wave#i.housework_3 i.wave#i.childcare_3 [pw=gewicht] if mysample==1
outreg using steps6, bdec(2) nocons  starlevels(10 5 1) sigsymbols(+,*,**) varlabels  replace 



*** Descriptives ***

* Table 1: variables by wave
global CATVARS"age_gr6 civ_stat3 ost isced3 erwp1_3 income children contribution_change_3 relative_earning geschl4_1_2 geschl4_2_2 housework_3 childcare_3"

tabout $CATVARS using "C:\Users\nkulic\Desktop\Social politics\Results 1401\des1.xls" if mysample==1&wave==1, replace ///
	cells(col) oneway npos(col) svy percent
	
	tabout $CATVARS using "C:\Users\nkulic\Desktop\Social politics\Results 1401\des2.xls" if mysample==1&wave==2, replace ///
	cells(col) oneway npos(col) svy percent
	
	
* FIG 0: zufr2_1 by wave (only for women)
tabplot zufr2 wave [aw=gewicht] if mysample==1, ///
		percent(wave) showval(format(%2.1f)) ///
		ytitle("") xtitle("")	plotr(lc(white)) scale(1.2)

save, replace


* Figure 1: satisfaction drop by gender in wave 1 and 2	
*men and women (start with the dataset with both genders, run the first part of the syntax without excluding men)

use couple_labelled.dta

set scheme s1mono

tabplot zufr2 sex [aw=gewicht] if mysample==1, ///
 by(wave, subtitle(% by sex and wave, place(w)) note("")) percent(sex wave) showval(format(%2.1f)) ///
 ytitle("") xtitle("") plotr(lc(white)) scale(1.2)
