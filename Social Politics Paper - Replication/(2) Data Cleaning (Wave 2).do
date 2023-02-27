*------------------------------------*
*  Kantar COVID-19 Gender module (refresher)   *
*------------------------------------*
* 		    14 01 2021           *
*------------------------------------*

* by Nevena Kulic *


use "C:\Users\xxx\Refresher.dta", clear
cd "C:\Users\xxx\Social politics\Results 1401"



*** Rename all the variables that have different names in the wave 2 ***

**Variable wave
gen wave=2
gen id_new=id_w2



*** Useful variables ***                                         

* Socio-demographics
tab1 stat1 stat2 stat2_gr3 stat8 stat9 stat10 stat11 stat12 stat14 stat15 stat16 stat16_gr3

* Income variables (erw3_other, erw4a and erw4b are present in wave 1 and not in wave 2, erw7 is the income variable erw4 from the previous wave)
tab1 erw7 
ren erw7 erw4

* Change in income (erw4a and erw4b in the old dataset)
tab1 erw8a erw8b
ren erw8a erw4a 
ren erw8b erw4b 


*** Gender module variables 
* Unpaid work in the household in terms of hours (geschl1_1 geschl1_2 geschl1_3 geschl1_4 in the previous wave, we need to rename new ones to be the same as the old ones)

tab1 geschl1a geschl1b geschl1c geschl1d
rename geschl1a geschl1_1
rename geschl1b geschl1_2
rename geschl1c geschl1_3
rename geschl1d geschl1_4


* Distribution of housework in the couple (geschl2_SQ001_0 geschl2_SQ001_1 in the old data)
tab1 geschl2a geschl2b
rename geschl2a geschl2_SQ001_0
rename geschl2b geschl2_SQ001_1

* Division of income between partners (geschl3_SQ001_0 geschl3_SQ001_1 in the old dataset)
tab1 geschl3a geschl3b
rename geschl3a geschl3_SQ001_0
rename geschl3b geschl3_SQ001_1

*** Gender Role Attitudes (geschl4_1 geschl4_2 geschl4_3 geschl4_4 in the old dataset)
tab1 geschl4a geschl4b geschl4c geschl4d

rename geschl4a geschl4_1
rename geschl4b geschl4_2 
rename geschl4c geschl4_3
rename geschl4d geschl4_4


*** Satisfactions
tab1 zufr1 zufr2 zufr3a zufr3b zufr3c


*** Crosstabs

*housework
ta geschl2_SQ001_0 geschl2_SQ001_1

*money
ta geschl3_SQ001_0 geschl3_SQ001_1 



*** Labelling ***
** Socio-demographics **

* Sex *
recode stat1 (0=1 "Man")(1=2 "Woman") (2=.), gen (sex)
label val sex sex
label var sex "Sex"

* Age 
gen age=2020-stat2

* Age three groups
clonevar age_gr3=stat2_gr3

* Age recoding in 6 groups: "18-34" "35-44" "45-54" "55-64" "65+"
recode age (0/18=1 "Below 18")(18/34=2 "18-34")(35/44=3 "35-44")(45/54=4 "45-54")(55/64=5 "55-64")(65/110=6 "Above 65"), gen (age_gr6)

* Regions
clonevar regions=stat4 

* Family status
clonevar civil_status=stat8
label def civil_status 1 "Married, with spouse" 2"Registered partnership (live together)" 3"Married, permanently separated" 4"Registered partnership (separated)" 5"Single, was never married" 6"Divorced / registered partnership dissolved" 7"Widowed / life partner died", replace
label val civil_status civil_status
label var civil_status "Civil status"
ta civil_status

* Committed partnership
clonevar partnership=stat9
label def partnership 0"No" 1 "Yes", replace 
label val partnership partnership
label var partnership "Strong partnership"
ta partnership

* Do you live with your partner in the same household?
clonevar in_partner=stat10
label def in_partner 0"No" 1 "Yes", replace 
label val in_partner in_partner
label var in_partner "Partner lives in the same household"
ta in_partner


* Crosstab strong partnership and living together first
gen cohab=.
replace cohab=1 if partnership==1&in_partner==1
replace cohab=0 if partnership==1&in_partner==0
replace cohab=2 if partnership==0&in_partner==0
replace cohab=3 if partnership==0&in_partner==1


* Family Status: Married (registered), cohabiting and single
gen civ_stat3=.
replace civ_stat3=1 if civil_status==1|civil_status==2
replace civ_stat3=2 if (civil_status==3|civil_status==4|civil_status==5|civil_status==6|civil_status==7)&cohab==1
replace civ_stat3=3 if (civil_status==3|civil_status==4|civil_status==5|civil_status==6|civil_status==7)&cohab==0

* Partner's sex
ta stat11
clonevar sex_p=stat11
label def sex_p 1 "Man" 2" Woman" 3" Other", replace
label val sex_p sex_p
label var sex_p "Partner's sex"
replace sex_p=. if sex_p==3
ta sex_p


* Heterosexual versus homosexual couples
gen hete=.
replace hete=1 if sex==1&sex_p==2|sex==2&sex_p==1
replace hete=0 if sex==1&sex_p==1|sex==2&sex_p==2
label def hete 0 "Homosexual" 1"Heterosexual", replace
label var hete "Type of union"
label val hete hete
ta hete

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

***1 are couples without small children, 2 couples with small children, singles with children, singles without children
gen civ_stat2=.
replace civ_stat2=1 if (civ_stat3==1|civ_stat3==2)& children_16==0
replace civ_stat2=2 if (civ_stat3==1|civ_stat3==2)& children_16!=0
replace civ_stat2=3 if civ_stat3==3&children_16==0
replace civ_stat2=4 if civ_stat3==3&children_16!=0


* Education groups///up to elementary school, high school, post-secondary
ta stat16_gr3
ta stat16_gr3, nolabel
clonevar edu_gr3=stat16_gr3
label def edu_gr3 1 "Low" 2"Medium" 3"High", replace
label val edu_gr3 edu_gr3
label var edu_gr3 "Education in three groups"
ta edu_gr3


***Isced education (a combination of regular education and vocational education)

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



*** Income variables ***
* Net household income in groups (it has more missings than the open question option)
ta erw7
ta erw7, nolabel
clonevar hincome_cat=erw7
replace hincome_cat=. if hincome_cat==7
label def hincome_cat 1 " Under 900" 2" 900-1500" 3 "1500-2600" 4 "2600-4000" 5 "4000-6000" 6 "Above 6000" 7 "I cannot disclose", replace
label val hincome_cat hincome_cat
label var hincome_cat "Household income in categories"
ta hincome_cat

* Merge of inc var and categorical income
* First recode income groups of categorical income
recode hincome_cat (1 2=1)(3=2)(4=3)(5 6=4), gen (hincome_cat_4)

* Recode continous income

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

* Who earned more before the Corona crisis
clonevar relative_earning=erw8a
label def relative_earning  1 "I earn much more than my partner" 2 "I earn a little more than my partner" 3 "I earn about the same as my partner" 4 "My partner earns a little more than me" 5 "My partner earns much more than I do" , replace
label val relative_earning relative_earning
label var relative_earning "Relative earnings"
ta relative_earning

* Change in earnings after Corona (How did your contribution on household income since the Corona crisis changed?

ta erw8b 
ta erw8b, nolabel
clonevar contribution_change=erw8b
label def contribution_change 1 "Dropped sharply" 2 "Dropped somewhat" 3 "Stayed roughly the same" 4 "Slightly increased" 5 "Increased strongly", replace
label val contribution_change contribution_change
label var contribution_change "Change in contributions to the household income"
ta contribution_change


recode contribution_change (1 2=1)(3=2)(4 5=3), gen (contribution_change_3)

label def contribution_change_3 1 "Dropped" 2 "Remained the same" 3 "Increased", replace
label val contribution_change_3 contribution_change_3



*** Gender Role Attitudes ***
*When jobs are scarce men have more right
 ta geschl4_1
 ta  geschl4_1, nolabel
 
 * Values in two dimensions
  recode geschl4_1 ( 1 2=1 "Agree") (3 4=2 "Disagree"), gen(geschl4_1_2)
ta geschl4_1 geschl4_1_2

label val geschl4_1_2 geschl4_1_2
label var geschl4_1_2 "Employment women when jobs are scarce"



*** HOUSEWORK AND MANAGEMENT ***

***Increase or decrease in hours of housework since the Corona crisis began
***Housework: washing, cleaning and cooking
ta geschl1_1 
clonevar housework=geschl1_1
label def housework 1 "Dicreased significantly" 7 "Increased significantly", modify
label val housework housework
label var housework "Housework"

***Childcare
ta geschl1_2
clonevar childcare=geschl1_2
label def childcare 1 "Dicreased significantly" 7 "Increased significantly", modify
label val childcare childcare
label var childcare "Childcare"
ta childcare
  
***Care work 
ta geschl1_3
clonevar care=geschl1_3 
label def care 1 "Dicreased significantly" 7 "Increased significantly", modify
label val care care
label var care "Care of people in need of care"
 ta care 

 *** Leisure time
ta geschl1_4
clonevar leisure=geschl1_4 
label def leisure 1 "Dicreased significantly" 7 "Increased significantly", modify
label val leisure leisure
label var leisure "Leisure time"
 ta leisure

  gen housework_m=housework-4
 gen leisure_m=leisure-4
 gen childcare_m=childcare-4
 
 
 ** Housework in couples **
* Before
 ta geschl2_SQ001_0
clonevar rel_housework_bc=geschl2_SQ001_0
label def rel_housework_bc  1 "I do all the housework" 2 "I do more housework than my partner" 3"We take over about the same amount of house " 4 "My partner does more" 5 "My partner takes over the entire housework", replace
label val rel_housework_bc rel_housework_bc
label var rel_housework_bc "Relative housework before Corona"
ta rel_housework_bc
 
 
* After Corona crisis
tab geschl2_SQ001_1 
clonevar rel_housework_ac=geschl2_SQ001_1
label def rel_housework_ac  1 "I do all the housework" 2 "I do more housework than my partner" 3"We take over about the same amount of house " 4 "My partner does more" 5 "My partner takes over the entire housework", replace
label val rel_housework_ac rel_housework_ac
label var rel_housework_ac "Relative housework after Corona"
ta rel_housework_ac
 
 
** Money management in couples

 * Before Corona
 ta geschl3_SQ001_0 
 clonevar management_bc=geschl3_SQ001_0
 label def management_bc 1 "Everyone manages their own money" 2 "I manage all the money and give me" 3 "My partner manages all the money" 4 "We put all the money together" 5 "We pool one part of money together and everyone keeps one part separate", replace
 label val management_bc management_bc
 label var management_bc "Money management before Corona"
 ta management_bc
 
 * After Corona
 ta geschl3_SQ001_1 
 clonevar management_ac=geschl3_SQ001_1
 label def management_ac 1 "Everyone manages their own money" 2 "I manage all the money and give me" 3 "My partner manages all the money" 4 "We put all the money together" 5 "We pool one part of money together and everyone keeps one part separate", replace
 label val management_ac management_ac
 label var management_ac "Money management before Corona"
 ta management_ac
 
 

* Missings values//check whether this is the best way
foreach var of varlist income{
	gen mi_`var' = `var' >= .
	replace `var' = 9 if mi_`var' == 1
	}

foreach var of varlist income {
	assert `var' !=.
	}

	save refresher_labelled.dta, replace


 

