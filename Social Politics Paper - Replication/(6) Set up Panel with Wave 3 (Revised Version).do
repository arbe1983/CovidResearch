*********************************
*** Do-File for the Revisions ***
*** Add Wave 3 for Additional ***
*********************************

* by Ariane Bertogg and Nevena Kulic *

use "C:\xxx\COVID-19 and Social Inequality B - Welle 3.dta"



*****************************
*** Data Cleaning: Wave 3 ***
*****************************

*** Recode Variables ***
clonevar gewicht=gewicht_W3 

ge age=2021-stat2

recode stat1 (0=1 "Man")(1=2 "Woman") (2=.), gen (sex)
label val sex sex
label var sex "Sex"

recode stat1 (0=1 "Man")(1=2 "Woman") (2=.), gen (psex)
label val psex sex
label var psex "Partner's sex"

ge hete=.
replace hete=1 if sex!=psex
replace hete=0 if sex==psex

clonevar civil_status=stat8
label def civil_status 1 "Married, with spouse" 2"Registered partnership (live together)" 3"Married, permanently separated" 4"Registered partnership (separated)" 5"Single, was never married" 6"Divorced / registered partnership dissolved" 7"Widowed / life partner died", replace
label val civil_status civil_status
label var civil_status "Civil status"
ta civil_status

clonevar regions=stat4  

* Commited partnership ("Feste Partnerschaft")
clonevar partnership=stat9
label def partnership 0"No" 1 "Yes", replace 
label val partnership partnership
label var partnership "Committed partnership"
ta partnership

* Do you live with your partner in the same household? / Cohabitation *
clonevar in_partner=stat10
label def in_partner 0"No" 1 "Yes", replace 
label val in_partner in_partner
label var in_partner "Partner lives in the same household"
ta in_partner

* Crosstab strong partnership and living together 
gen cohab=.
replace cohab=1 if partnership==1&in_partner==1
replace cohab=0 if partnership==1&in_partner==0
replace cohab=2 if partnership==0&in_partner==0
replace cohab=3 if partnership==0&in_partner==1

* Civil Status *
gen civ_stat3=.
replace civ_stat3=1 if civil_status==1|civil_status==2
replace civ_stat3=2 if (civil_status==3|civil_status==4|civil_status==5|civil_status==6|civil_status==7)&cohab==1
replace civ_stat3=3 if (civil_status==3|civil_status==4|civil_status==5|civil_status==6|civil_status==7)&cohab==0
lab define civ_stat3 1"Married" 2"Cohabiting" 3"Single"
lab val civ_stat3 civ_stat3

* Children (do you have children)
ta stat12
ta stat12, nolabel
clonevar children=stat12
label def children 0 "No" 1"Yes", replace
label val children children
label var children "Children"

* Children below 16
ta stat14
ta stat14, nolabel
clonevar children_16=stat14
label def children_16 0 "No" 1"1" 5"More than 5", modify
label val children_16 children_16
label var children_16 "Children below 16"
ta children_16

* Age of the youngest kid
ta stat15
clonevar young_kid=stat15
label var young_kid "The age of youngest kid"
label val young_kid young_kid
sum young_kid

* Generate adjusted children and childcare variables (to match codings in Wave 1 from revision file) *
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

*new kids variable
gen newkid=.
replace newkid=0 if kidsneu==0  // set childless to zero 
replace newkid=1 if kidsneu==1&stat15==0|kidsneu==1&stat15>1&stat15<2
replace newkid=2 if kidsneu==1&stat15==3|kidsneu==1&stat15==4|kidsneu==1&stat15==5
replace newkid=3 if kidsneu==1&stat15==6|kidsneu==1&stat15==7|kidsneu==1&stat15==8|kidsneu==1&stat15==9|kidsneu==1&stat15==10|kidsneu==1&stat15==11|kidsneu==1&stat15==12
replace newkid=4 if kidsneu==1&stat15==13|kidsneu==1&stat15==14|kidsneu==1&stat15==15|kidsneu==1&stat15==16
replace newkid=5 if kidsneu==2&stat15==22|kidsneu==2

label define newkid 0 "No kids" 1 "Kids below 2" 2 "Kids 3-5" 3 "Kids 6-12" 4 "Kids 13-16" 5 "Has own kids, but not in hh or not <16" , replace
label value newkid newkid

* Generate New Childcare variable *
recode childcare (1/3=1) (4=2) (5/7=3) (0=4), ge (childcare_4)
ge childcareneu_4=childcare_4
replace childcareneu_4=9 if childcareneu_4<9&kidsneu==0
lab var childcare_4 "only item non-response as missing"
lab var childcareneu_4 "item non-reponse AND childless as"

* How much overlap? *
ta childcare_4 kidsneu, m
ta childcareneu_4 kidsneu, m


* Education: 3  groups /// up to elementary school, high school, post-secondary
ta stat16_gr3
ta stat16_gr3, nolabel
clonevar edu_gr3=stat16_gr3
label def edu_gr3 1 "Low" 2"Medium" 3"High", replace
label val edu_gr3 edu_gr3
label var edu_gr3 "Education in three groups"
ta edu_gr3


* Isced education (a combination of regular education and vocational education)

gen isced=.
replace isced=1 if (stat16==2&stat17==11)|(stat16==2&stat17==.)
replace isced=2 if (stat16==3|stat16==4) & (stat17==1|stat17==5|stat17==12 | stat17==11) | ///
                     (stat16==2 &inlist(stat17,1,5,12))

replace isced=3 if (inlist(stat16,3,4) & inlist(stat17,2,3,4,6,7)) | ///
                     (inlist(stat16,5,6) & stat17==11) | ///
					 (stat16==2&inlist(stat17,2,3,4,6,7))

replace isced=4 if ((stat16==5|stat16==6)&(stat17==2 |stat17==3|stat17==4|stat17==6|stat17==7)) | stat17==8
replace isced=5 if stat17==9|stat17==10

lab def isced 1 "primary education" ///
             2 "lower secondary education" ///
			 3 "upper secondary education" ///
			 4 "post secondary education" ///
			 5 "tertiary education"
label values isced isced


* Isced3 (with three categories, 0/2, 34, 5) 
recode isced (1 2=1)(3 4=2)(5=3), gen (isced3)

* Income *
recode income_categories (1 2=1)(3=2)(4=3)(5 6=4), gen (hincome_cat_4)
recode inc (0/1500=1)(1500/2600=2)(2600/4000=3)(4000/100000=4), gen (inc_4)

* Put the two together (624 missings)
gen income=.
replace income=1 if inc_4==1|hincome_cat==1
replace income=2 if inc_4==2|hincome_cat==2
replace income=3 if inc_4==3|hincome_cat==3
replace income=4 if inc_4==4|hincome_cat==4

label def income 1 " Under 1500" 2 "1500-2600" 3 "2600-4000" 4 "Above 4000", replace
label val income income
label var income "Household income in four categories"
ta income


* Who earns more before the Corona crisis
clonevar relative_earning=erw8a
label def relative_earning  1 "I earn much more than my partner" 2 "I earn a little more than my partner" 3 "I earn about the same as my partner" 4 "My partner earns a little more than me" 5 "My partner earns much more than I do" , replace
label val relative_earning relative_earning
label var relative_earning "Relative earnings"
ta relative_earning

clonevar contribution_change=erw8b
label def contribution_change 1 "Dropped sharply" 2 "Dropped somewhat" 3 "Stayed roughly the same" 4 "Slightly increased" 5 "Increased strongly", replace
label val contribution_change contribution_change
label var contribution_change "Change in contributions to the household income"
ta contribution_change

recode contribution_change (1 2=1)(3=2)(4 5=3), gen (contribution_change_3)

label def contribution_change_3 1 "Dropped" 2 "Remained the same" 3 "Increased", replace
label val contribution_change_3 contribution_change_3

ren geschl4a geschl4_1 
 recode geschl4_1 ( 1 2=1 "Agree") (3 4=2 "Disagree"), gen(geschl4_1_2)
ta geschl4_1 geschl4_1_2

label val geschl4_1_2 geschl4_1_2
label var geschl4_1_2 "Employment women when jobs are scarce"

ren geschl1a geschl1_1 
clonevar housework=geschl1_1
label def housework 1 "Dicreased significantly" 7 "Increased significantly", modify
label val housework housework
label var housework "Housework"

ren geschl1b geschl1_2 
clonevar childcare=geschl1_2
label def childcare 1 "Dicreased significantly" 7 "Increased significantly", modify
label val childcare childcare
label var childcare "Childcare"
ta childcare

ren geschl1c geschl1_3
clonevar care=geschl1_3 
label def care 1 "Dicreased significantly" 7 "Increased significantly", modify
label val care care
label var care "Care of people in need of care"

ren geschl1d geschl1_4
 ta care 

 gen housework_m=housework-4
 gen leisure_m=leisure-4
 gen childcare_m=childcare-4


* Housework Before Covid *
clonevar rel_housework_bc=geschl2a
label def rel_housework_bc  1 "I do all the housework" 2 "I do more housework than my partner" 3"We take over about the same amount of house " 4 "My partner does more" 5 "My partner takes over the entire housework", replace
label val rel_housework_bc rel_housework_bc
label var rel_housework_bc "Relative housework before Corona"
ta rel_housework_bc
 
 * After Corona crisis
clonevar rel_housework_ac=geschl2b
label def rel_housework_ac  1 "I do all the housework" 2 "I do more housework than my partner" 3"We take over about the same amount of house " 4 "My partner does more" 5 "My partner takes over the entire housework", replace
label val rel_housework_ac rel_housework_ac
label var rel_housework_ac "Relative housework after Corona"
ta rel_housework_ac


** Money management in couples
* Before Corona
 clonevar management_bc=geschl3a
 label def management_bc 1 "Everyone manages their own money" 2 "I manage all the money and give me" 3 "My partner manages all the money" 4 "We put all the money together" 5 "We pool one part of money together and everyone keeps one part separate", replace
 label val management_bc management_bc
 label var management_bc "Money management before Corona"
 ta management_bc
 
* After Corona
 clonevar management_ac=geschl3b
 label def management_ac 1 "Everyone manages their own money" 2 "I manage all the money and give me" 3 "My partner manages all the money" 4 "We put all the money together" 5 "We pool one part of money together and everyone keeps one part separate", replace
 label val management_ac management_ac
 label var management_ac "Money management before Corona"
 ta management_ac
 
 * Age three groups*
clonevar age_gr3=stat2_gr3

* Age recoding - six groups *
recode age (0/18=1 "Below 18")(18/34=2 "18-34")(35/44=3 "35-44")(45/54=4 "45-54")(55/64=5 "55-64")(65/110=6 "Above 65"), gen (age_gr6)

ren erw1now erw1
ren erwp1now erwp1 

recode erw1 (1=1 "Full-time")(2=2 "Part-time")(3/11=3 "Inactive and unemployed"), gen (erw1_3)
recode erw1_3 (1/2=1 "Work") (3=2 "Not working"), gen (erw1_2)

* Three categories of work status before Corona *
ta erw1_3 erw1

* The same for the partner *
ta erwp1, nolabel
recode erwp1 (1=1 "Full-time")(2=2 "Part-time")(3/11=3 "Inactive and unemployed"), gen (erwp1_3)
ta erwp1_3

ren geschl4b geschl4_2
recode geschl4_2 (1 2=1 "agree")(3 4=2 "disagree"), gen (geschl4_2_2)
lab var geschl4_2_2 "Working Mother can have warm relationship"

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

save w3_labelled.dta, replace

 

*** Sample Selection ***
keep id wave gewicht zufr2 hete female partner_female stat1 stat11 age management_ac management_bc  rel_housework_ac rel_housework_bc isced  housework childcare leisure care housework_m childcare_m leisure_m geschl1_1 geschl1_2 contribution_change  hincome_cat_4 inc_4 edu_gr3 civil_status sex psex regions partnership in_partner cohab  civ_stat3 stat12 stat14 stat15 young_kid children_16 children relative_earning geschl4_1  geschl4_1_2 geschl1_1  geschl1_2 geschl1_3 geschl1_4 erw1now erwp1now age_gr3 age_gr6 erw1 erwp1 erw1_3 erwp1_3 zufr2_1 zufr2_2 zufr2_3 childcare housework childcare_neu childcare_3 childcareneu_3 geschl4_2_2 erw1_2 sample gewicht_W3 childcareneu_4 newkid kidsneu minorkidshh


keep if female==1
keep if age<65
keep if hete==1
drop if erw1now==10


* Add Panel ID *
clonevar id_original=id 

sort id_original wave
merge 1:m id_original wave using idconverter.dta 

ta _merge wave
ta _merge sample 

* Kick out empty observations and refreshers *
drop if _merge==2 &wave<3 
drop if _merge==1& wave==3
drop _merge 

duplicates report panel_id wave 
sort panel_id wave 

save w3_labelled_selected.dta, replace
clear 



*************************************
*** Bring together waves 1 and 3 ***
*************************************

*** Load data file with waves 1 and 2 ****

* Drop wave 2 observations *
use "C:\Users\xxx\couple_labelled.dta", clear
drop if wave==2

duplicates report id_new wave 
* there should be no duplicates! if so: 

/*
duplicates tag id_new wave, ge(dup)
ta dup 
ta id_new if dup>0
*id should be missing --> empty cases *
ta wave if dup>0 
* wave should be missing --> empty cases *
drop if dup>0
*/

* Merge Panel ID for joining with Wave 3 *
gen id_original=id_new
sort id_original wave

merge m:1 id_original wave using idconverter 
ta _merge wave
drop if _merge==2 

save w1_couple_labelled_panel.dta 


* Now create longfile with waves 1 and 3 only *
sort panel_id wave 
append using w3_labelled_selected

egen count=_n
egen obs=_N
ta wave count if obs==1
 
save panelfile w1w3.dta

