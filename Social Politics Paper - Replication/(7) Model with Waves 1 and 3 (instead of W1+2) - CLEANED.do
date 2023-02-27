************************************************
*** Do-File for the Revisions ******************
*** Analyze Wave 3 in Comparison with Wave 1 ***
************************************************

* by Ariane Bertogg & Nevena Kulic *



*** Prepare for Modelling ****
use panelfile w1w3.dta


** Set up the Panel **
xtset panel_id wave

* Balanced panel
by panel_id: gen nyear=[_N]

gen longweight=gewicht_W3 if wave==3
replace longweight=f2.gewicht if wave==1&obs==2
replace longweight=gewicht if wave==1&obs==1

svyset longweight

** CORRECTED FROM HERE **
ge balanced13=0
replace balanced13=1 if _N==2
* Valid observations in Waves 1 and 3 (the same persons)


** Set new controls **
global control2sneu "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_4 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2"

global newcontrols "i.age_gr6 i.civ_stat3 i.ost i.isced3 i.erwp1_3 i.income ib1.newkid stat13 i.contribution_change_3 i.relative_earning ib1.geschl4_1_2 ib1.geschl4_2_2"


****************
*** Re-Model ***
****************

* Not all of the models from the paper, only the main ones *


*** Models from Revision ***

* W1 only *
logit zufr2_1 ib2.erw1_3 $newcontrols [pw=longweight] balanced13==1&mysample==1&wave==1
est sto m1
eststo margins_m1: margins, dydx(*) post

* Control for Childcare and Housework *
logit zufr2_1 ib2.erw1_3 $newcontrols ib2.childcareneu_4 ib2.housework_3 [pw=longweight] if balanced13==1&mysample==1&wave==1
est sto m2
eststo margins_m2: margins, dydx(*) post

esttab m1 m2  using "W1 (Balanced Panel).rtf", b(3) nogap not mti replace


* W1 and W3 *
logit zufr2_1 ib2.erw1_3##wave $newcontrols [pw=longweight] balanced13==1&mysample==1
est sto m4
eststo margins_m4: margins, dydx(*) post

* Control for Childcare and Housework *
logit zufr2_1 ib2.erw1_3##wave  $newcontrols ib2.childcareneu_4 ib2.housework_3 [pw=longweight] if balanced13==1&mysample==1
est sto m5
eststo margins_m5: margins, dydx(*) post

esttab m4 m5 using "W1 and W3 (Balanced Panel).rtf", b(3) nogap not mti replace


* AME for both Waves *
esttab margins_m1 margins_m2 margins_m4 margins_m5 using "AME Robust.rtf", b(3) nogap mti se replace

* AME from interaction models, separate by wave *
est restore m4
eststo margins_m4_ad: margins, dydx(*) at(wave=1)post
est restore m4
eststo margins_m4_ad2: margins, dydx(*) at(wave=3)post

est restore m5
eststo margins_m5_ad: margins, dydx(*) at(wave=1)post
est restore m5
eststo margins_m5_ad2: margins, dydx(*) at(wave=3)post

esttab margins_m4_ad margins_m4_ad2 margins_m5_ad margins_m5_ad2 using "AME by Wave.rtf", b(3) nogap mti se replace



*** Graphing the Results ***

* For waves 1 and 2 *
est restore m5 
eststo mar5_1: margins , at(erw1_3=(1 2 3) wave=(1)) post
est restore m5 
eststo mar5_3: margins , at(erw1_3=(1 2 3) wave=(3)) post

coefplot mar5_1 mar5_3
gr save AME_w1w3.gph, replace




******************
*** Robustness ***
******************

*** Models from First Submission  ***
* W1 and W3 *
svy: logit zufr2_1 ib2.erw1_3##i.wave $controls2neu if balanced13==1
est sto m6p
margins, dydx(*) post
est sto margins_m6p

svy: logit zufr2_1 ib2.erw1_3##i.wave  $controls2neu ib2.ccneu ib2.change_hw if balanced13==1
est sto m7p 
margins, dydx(*) post
est sto margins_m7p

* Graph *
est restore m7p 
eststo mar5_1: margins , at(erw1_3=(1 2 3) wave=(1)) post
est restore m7p 
eststo mar5_2: margins , at(erw1_3=(1 2 3) wave=(3)) post

coefplot mar5_1 mar5_2 , nolabel

esttab m6p m7p  using "Robust Waves 1 and 3.rtf", b(3) nogap not mti replace


** Robust: Four categories in employment variable **
svy: logit zufr2_1 ib2.erw1_4##i.wave $controls2neu if balanced13==1
est sto m6p4
margins, dydx(*) post
est sto margins_m6p4

svy: logit zufr2_1 ib2.erw1_4##i.wave  $controls2neu ib2.childcare_3 ib2.change_hw if balanced13==1
est sto m7p4
margins, dydx(*) post
est sto margins_m7p4

est restore m7p4
margins erw1_4##i.wave
marginsplot, x(wave)
gr save w1w3margins_time.gph, replace

est restore m7p4
margins erw1_4##i.wave
marginsplot, x(erw1_4)
gr save w1w3margins_empl4.gph, replace
