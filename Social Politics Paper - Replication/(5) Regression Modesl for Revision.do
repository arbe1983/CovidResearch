*********************************
*** Do-File for the Revisions ***
*********************************

* by Ariane Bertogg and Nevena Kulic *
* Note: In the new version I keep only the parts of the do file that we also use for the article *

use "C:\xxx\Social politics\Analyses_last version\prova_ab2\prova_ab2.dta", clear 


* Comparison: Change in Life Satisfaction (self-reported): Waves 1 (May 2020) and 2 (November 2020)

* For the Comparison of Wave 1 and Wave 3, see separate file *



************************************************
*** First Step: Additional Data Preparation ***
************************************************

** Recodings **
* (1) Generate children and childcare variables *
ge minorkidshh=.
replace minorkidshh=1 if stat14>0&stat14<6
replace minorkidshh=0 if minorkidshh==.

ge kidsneu=.
replace kidsneu=1 if minorkidshh==1
replace kidsneu=2 if minorkidshh==0&children==1
replace kidsneu=0 if kidsneu==.
lab define kidsneu 0"no kids" 1"kids <16 in hh" 2"has own kids, but not in hh or not <16"
lab val kidsneu kidsneu
lab var kidsneu "children variable with three categories (by age)"

* Generate New Childcare variable *
recode childcare (1/3=1) (4=2) (5/7=3) (0=4), ge (childcare_4)
ge childcareneu_4=childcare_4
replace childcareneu_4=9 if childcareneu_4<9&kidsneu==0
lab var childcare_4 "only item non-response as missing"
lab var childcareneu_4 "item non-reponse AND childless as"

* How much overlap? *
ta childcare_4 kidsneu, m
ta childcareneu_4 kidsneu, m
* looks good*

* age of youngest child and family size *
ta stat13  // the size of the family including the respondent
ta stat15  // the age of the smallest child
* Note children's age was an open field, hence no discrete variable *

*new kids variable
gen newkid=.

replace newkid=0 if kidsneu==0  // set childless to zero 

replace newkid=1 if kidsneu==1&stat15==0|kidsneu==1&stat15==0.5|kidsneu==1&stat15==0.74|kidsneu==1&stat15==1|kidsneu==1&stat15==1.25|kidsneu==1&stat15==2|kidsneu==1&stat15==1.5
replace newkid=2 if kidsneu==1&stat15==3|kidsneu==1&stat15==4|kidsneu==1&stat15==5
replace newkid=3 if kidsneu==1&stat15==6|kidsneu==1&stat15==7|kidsneu==1&stat15==8|kidsneu==1&stat15==9|kidsneu==1&stat15==10|kidsneu==1&stat15==11|kidsneu==1&stat15==12
replace newkid=4 if kidsneu==1&stat15==13|kidsneu==1&stat15==14|kidsneu==1&stat15==15|kidsneu==1&stat15==16
replace newkid=5 if kidsneu==2&stat15==22|kidsneu==2

label define newkid 0 "No kids" 1 "Kids below 2" 2 "Kids 3-5" 3 "Kids 6-12" 4 "Kids 13-16" 5 "Has own kids, but not in hh or not <16" , replace
label value newkid newkid


* (2) Hours of work before Corona for wave 1 respondents and after *
* tab1 tab1 az1_1_SQ001 az1_1_SQ002

* Cleaning the hours of work before Corona
ge work_hours=subinstr(az1_2_SQ001,"Hours of work","",.)
replace work_hours=subinstr(work_hours,"(","",.)
replace work_hours=trim(subinstr(work_hours,")","",.))
replace work_hours="15" if work_hours=="ca 15  Stunden"
replace work_hours="5" if work_hours=="5 Stunden"
replace work_hours="-99" if work_hours=="k.A"
replace work_hours="40" if work_hours=="40 -45 Std"|work_hours=="40h"

replace work_hours="25" if work_hours=="25 - 26"
replace work_hours="25" if work_hours=="25-30"
replace work_hours="25" if work_hours=="25-32"
replace work_hours="4" if work_hours=="4-10"

replace work_hours="45" if work_hours=="45-50"
replace work_hours="50" if work_hours=="50-55"
replace work_hours="55" if work_hours=="54-55"
replace work_hours="-99" if work_hours=="J"
replace work_hours="22" if work_hours=="22 Stunden"

replace work_hours="45" if work_hours=="42-45"
replace work_hours="40" if work_hours=="40 - 50"|work_hours=="40-43"|work_hours=="40-45"|work_hours=="40-50"|work_hours=="40-60"
browse if missing(real(work_hours))
destring work_hours, dpcomma replace 

* Hours after Corona
ge work_hours_a=subinstr(az1_1_SQ002,"Hours of work","",.)
replace work_hours_a=subinstr(work_hours_a,"(","",.)
replace work_hours_a=trim(subinstr(work_hours_a,")","",.))
replace work_hours_a="10" if work_hours_a=="10 Stunden"
replace work_hours_a="15" if work_hours_a=="15-20"
replace work_hours_a="20" if work_hours_a=="15-37"|work_hours_a=="20 Stunden"
replace work_hours_a="2" if work_hours_a=="2-3"

replace work_hours_a="25" if work_hours_a=="25-,30"
replace work_hours_a="28" if work_hours_a=="28 - 30"
replace work_hours_a="37" if work_hours_a=="33-37,5 "
replace work_hours_a=" " if work_hours_a=="4040"

replace work_hours_a=" " if work_hours_a=="H"
replace work_hours_a=" " if work_hours_a=="Heimarbeit"
replace work_hours_a="6" if work_hours_a=="ca 6 Stunden"
replace work_hours_a="2" if work_hours_a=="ca.2 "
replace work_hours_a="0" if work_hours_a=="keine"

replace work_hours_a="0" if work_hours_a=="keine"
replace work_hours_a="45" if work_hours_a=="homeoffice dito"
replace work_hours_a="2" if work_hours_a=="ca.2"
replace work_hours_a=" " if work_hours_a==":"
replace work_hours_a=" " if work_hours_a=="."

replace work_hours_a="16" if work_hours_a=="16,25"
replace work_hours_a="17" if work_hours_a=="17,5"
replace work_hours_a="18" if work_hours_a=="18,5"
replace work_hours_a="19" if work_hours_a=="19,0"
replace work_hours_a="25" if work_hours_a=="25,5"

replace work_hours_a="27" if work_hours_a=="27,5"
replace work_hours_a="29" if work_hours_a=="29,25"
replace work_hours_a="32" if work_hours_a=="32,5"
replace work_hours_a="37" if work_hours_a=="33-37,5"
replace work_hours_a="37" if work_hours_a=="37.5"
replace work_hours_a="29" if work_hours_a=="29.25"

replace work_hours_a="37" if work_hours_a=="37,5"
replace work_hours_a="38" if work_hours_a=="38,5"
replace work_hours_a="38" if work_hours_a=="38.5"
replace work_hours_a="39" if work_hours_a=="39,5"
replace work_hours_a="40" if work_hours_a=="40,1"
replace work_hours_a="40" if work_hours_a=="40,2"
replace work_hours_a="42" if work_hours_a=="42"
replace work_hours_a="7" if work_hours_a=="7,5"
replace work_hours_a="42" if work_hours_a=="42,5"

browse if missing(real(work_hours_a))
destring work_hours_a, dpcomma replace 


* Hours of work before Corona and after for wave 2 respondents//the variables have two different names
tab1 erw14_1a erw14_1b

ge work_hours_2=subinstr(erw14_1a,"Hours of work","",.)
replace work_hours_2=subinstr(work_hours_2,"(","",.)
replace work_hours_2=trim(subinstr(work_hours_2,")","",.))
replace work_hours_2="15" if work_hours_2=="15 Std."
replace work_hours_2="20" if work_hours_2=="20 Stunden"
replace work_hours_2="20" if work_hours_2=="22,5"
replace work_hours_2="37" if work_hours_2=="37,5"

replace work_hours_2="38" if work_hours_2=="38,5"
replace work_hours_2="39" if work_hours_2=="39,5"
replace work_hours_2="42" if work_hours_2=="42,5"
browse if missing(real(work_hours_2))
destring work_hours_2, dpcomma replace 

ge work_hours_2a=subinstr(erw14_1b,"Hours of work","",.)
replace work_hours_2a=subinstr(work_hours_2a,"(","",.)
replace work_hours_2a=trim(subinstr(work_hours_2a,")","",.))
replace work_hours_2a="20" if work_hours_2a=="20 Stunden"
replace work_hours_2a="22" if work_hours_2a=="22,5"
replace work_hours_2a="37" if work_hours_2a=="37,5"
replace work_hours_2a="38" if work_hours_2a=="38,5"

replace work_hours_2a="42" if work_hours_2a=="42,5"
replace work_hours_2a="0" if work_hours_2a=="O"

browse if missing(real(work_hours_2a))
destring work_hours_2a, dpcomma replace 

replace work_hours_a=work_hours_2a if work_hours_a==.&wave==2
replace work_hours=work_hours_2 if work_hours==.&wave==2


* recode work hours in part time and full time
replace work_hours_a=. if work_hours_a==-99
replace work_hours=. if work_hours==-99

recode work_hours_a (0/15=1) (16/34=2) (35/160=3), gen (work_hours_a_)
recode work_hours (0/15=1) (16/34=2) (35/160=3), gen (work_hours_)

ta work_hours_a_ work_hours_

lab var work_hours_a "Work hours after Covid"
lab var work_hours "Work hours during Covid"
 lab define work_hours 0"Marginal" 1"Part-time" 2"Fulltime"
 lab val work_hours_a work_hours
 lab var work_hours work_hours
 
 
* Change in Working Hours *
gen change_=.
replace change=1 if work_hours_a_==work_hours_
replace change=2 if work_hours_a_!=work_hours_
lab var change "Change in Working Hours"
lab define change 1"No" 2"Yes"

* New variable representing change in working hours (less / the same / more / loss of job) 

gen change=.
replace change=1 if work_hours_a==work_hours
replace change=2 if work_hours_a<work_hours
replace change=3 if work_hours_a>work_hours
recode change (1=1 "the same") (2=2 "reduced")(3=3 "increased"), gen(change_l) 

*Change with some flexibility
gen change_=.
replace change_=1 if (work_hours_a-work_hours)<=5
replace change_=2 if (work_hours_a-work_hours)>5
recode change (1=1 "the same") (2=2 "reduced")(3=3 "increased"), gen(change_l) 


* (3) Employment Variable *
recode erw1 (1=1 "Full-time")(2=2 "Part-time")(3=3 "Part-time <15")(4/11=4 "Inactive and unemployed"), gen (erw1_4)

* Change in Leisure Time *
recode leisure (1/3=1 "Decreased") (4=2 "The same") (5/7=3 "Increased"), gen (leisure_3)

* Income loss due to corona has more than 90 percent of missings
ta erw16



*****************************************
*** Second Step: Models for Revision ***
*****************************************
 
*** Revised models ***
global newcontrols "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2"


** Test Effects of the New Childcare and new Children Variable **

* W1 only *
logit zufr2_1 ib2.erw1_3 $newcontrols [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m1
eststo margins_m1: margins, dydx(*) post

* Control for Childcare and Housework *
logit zufr2_1 ib2.erw1_3 $newcontrols ib2.childcareneu_4 ib2.housework_3 [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m2
eststo margins_m2: margins, dydx(*) post

esttab m1 m2  using "Robust.rtf", b(3) nogap not mti replace


* W1 and W2 *
logit zufr2_1 ib2.erw1_3##wave $newcontrols [pw=gewichtKANTAR] if mysample==1
est sto m4
eststo margins_m4: margins, dydx(*) post

* Control for Childcare and Housework *
logit zufr2_1 ib2.erw1_3##wave  $newcontrols ib2.childcareneu_4 ib2.housework_3 [pw=gewichtKANTAR] if mysample==1
est sto m5
eststo margins_m5: margins, dydx(*) post

esttab m4 m5 using "Robust2.rtf", b(3) nogap not mti replace


* AME for both Waves *
esttab margins_m1 margins_m2 margins_m4 margins_m5 using "AME Robust.rtf", b(3) nogap mti se replace

* AME from interaction models, separate by wave *
est restore m4
eststo margins_m4_ad: margins, dydx(*) at(wave=1)post
est restore m4
eststo margins_m4_ad2: margins, dydx(*) at(wave=2)post

est restore m5
eststo margins_m5_ad: margins, dydx(*) at(wave=1)post
est restore m5
eststo margins_m5_ad2: margins, dydx(*) at(wave=2)post

esttab margins_m4_ad margins_m4_ad2 margins_m5_ad margins_m5_ad2 using "AME by Wave.rtf", b(3) nogap mti se replace



*** Graphing the Results ***

* For waves 1 and 2 *
est restore m5 
eststo mar5_1: margins , at(erw1_3=(1 2 3) wave=(1)) post
est restore m5 
eststo mar5_2: margins , at(erw1_3=(1 2 3) wave=(2)) post

coefplot mar5_1 mar5_2 , nolabel




*************************
*** Robustness Checks ***
**************************

** Adding change in working hours (as compared to before corona) **
global newcontrols_1 "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2 i.change_l"

* W1 only *
logit zufr2_1 ib2.erw1_3 $newcontrols_1 [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m1_c
margins, dydx(*)
est sto margins_m1_c

logit zufr2_1 ib2.erw1_3 $newcontrols_1 ib2.childcareneu_4 ib2.housework_3 [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m2_c
margins, dydx(*)
est sto margins_m2_c 

esttab m1 m2  using "Robust_change_contrib.rtf", b(3) nogap not mti replace


* W1 and W2 *
logit zufr2_1 ib2.erw1_3##wave $newcontrols_1 [pw=gewichtKANTAR] if mysample==1
est sto m4_c
margins, dydx(*)
est sto margins_m4_c

logit zufr2_1 ib2.erw1_3##wave  $newcontrols_1 ib2.childcareneu_4 ib2.housework_3 [pw=gewichtKANTAR] if mysample==1
est sto m5_c
margins, dydx(*)
est sto margins_m5_c


esttab m4_c m5_c using "Robust_change_2.rtf", b(3) nogap not mti replace

esttab margins_m1_c margins_m2_c margins_m4_c margins_m5_c using "AME Robust w/Change.rtf", b(3) nogap mti replace



** Add Leisure as control **
global newcontrols_2 "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2"

* W1 only *
logit zufr2_1 ib2.erw1_3 $newcontrols_2 [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m1_leis
margins, dydx(*)
est sto margins_m1_leis

logit zufr2_1 ib2.erw1_3 $newcontrols_" ib2.childcareneu_4 ib2.housework_3 ib2.leisure_3 [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m2_leis
margins, dydx(*)
est sto margins_m2_leis

esttab m1_leis m2_leis  using "Robust_Leisure.rtf", b(3) nogap not mti replace


* W1 and W2 *
logit zufr2_1 ib2.erw1_3##wave $newcontrols_2 [pw=gewichtKANTAR] if mysample==1
est sto m4_leis
margins, dydx(*)
est sto margins_m4_leis

logit zufr2_1 ib2.erw1_3##wave  $newcontrols_2 ib2.childcareneu_4 ib2.housework_3 ib2.leisure_3 [pw=gewichtKANTAR] if mysample==1
est sto m5_leis
margins, dydx(*)
est sto margins_m5_leis

esttab m4 m5 using "Robust_Leisure (Waves 1 and 2).rtf", b(3) nogap not mti replace

esttab margins_m1_leis margins_m2_leis margins_m4_leis margins_m5leis using "AME Robust Leisure.rtf", b(3) nogap mti replace



*** Division of part time to differentiate between regular part-time and marginal employment ***

global newcontrols "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2"

* W1 only *
logit zufr2_1 ib2.erw1_4 $newcontrols [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m1_ptneu
margins, dydx(*)
est sto margins_m1_ptneu

logit zufr2_1 ib2.erw1_4 $newcontrols ib2.childcareneu_4 ib2.housework_3  [pw=gewichtKANTAR] if mysample==1&wave==1
est sto m2_ptneu
margins, dydx(*)
est sto margins_m2_ptneu 

esttab m1_ptneu m2_ptneu  using "Robust Part-Time New.rtf", b(3) nogap not mti replace


* W1 and W2 *
logit zufr2_1 ib2.erw1_4##wave $newcontrols [pw=gewichtKANTAR] if mysample==1
est sto m4_ptneu
margins, dydx(*)
est sto margins_m4_ptneu

logit zufr2_1 ib2.erw1_4##wave  $newcontrols ib2.childcareneu_4 ib2.housework_3  [pw=gewichtKANTAR] if mysample==1
est sto m5_ptneu
margins, dydx(*)
est sto margins_m5_ptneu

esttab m4_ptneu m5_ptneu using "Robust Part-Time New (Waves 1 and 2).rtf", b(3) nogap not mti replace

esttab margins_m1_ptneu margins_m2_ptneu margins_m4_ptneu margins_m5_ptneu using "AME Robust Part-Time New.rtf", b(3) nogap mti replace




********************
*** For Appendix ***
********************

** Descriptives for Appendix **
* with new children variable *
global CATVARS"age_gr6 civ_stat3 ost isced3 erwp1_3 income newkid stat13  contribution_change_3 relative_earning geschl4_1_2 geschl4_2_2 housework_3 childcareneu_4"

tabout $CATVARS using "descriptives wave1.xls" if mysample==1&wave==1, replace cells(col) oneway npos(col) svy percent
	
tabout $CATVARS using "descriptives wave2.xls" if mysample==1&wave==2, replace cells(col) oneway npos(col) svy percent

svy: mean stat13 if  mysample==1&wave==1
svy: mean stat13 if  mysample==1&wave==2
