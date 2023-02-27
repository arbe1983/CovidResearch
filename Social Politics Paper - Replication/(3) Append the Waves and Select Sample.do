*****************************
*** Social Politics Paper ***
*****************************

* Nevena Kulic & Ariane Bertogg *


*** Append the two Waves ***

* Merge the cleaned data in one file
* Select our sample 
clear 
set more off

cd "C:\Users\xxx\Social politics\Results 1401"

use refresher_labelled.dta, clear
count
append using wave1_labelled.dta,force
count

* Create a unique Id
sort wave id_new
gen seq_numb=_n
sort seq_numb

save Kantar_append, replace



*** Sample selection ***
* Final analytical sample *

* First select only heterosexual marrried and cohabiting couples *
* then drop everyone beyond retirement age *
keep if hete==1&civ_stat3!=3
drop if age>=65

global ALLVARS1 "zufr2 age_gr6 civ_stat3 ost isced3 income children contribution_change_3 relative_earning erwp1 erw1 childcare housework geschl4_1 geschl4_2"

qui reg zufr2 $ALLVARS1
gen mysample=1 if e(sample) 

tab1 $ALLVARS1 if mysample==1


* Only couples with children below age of 16 *
replace children_16=. if children_16==0
replace children_16=0 if children_16==.&stat14==0

global ALLVARS2 "zufr2 age_gr6 civ_stat3 ost isced3 income children contribution_change_3 relative_earning erwp1 erw1 childcare housework geschl4_1 geschl4_2 children_16"
qui reg zufr2 $ALLVARS2
gen mysample1=1 if e(sample) 

tab1 $ALLVARS2 if mysample1==1

save couple_labelled.dta, replace


