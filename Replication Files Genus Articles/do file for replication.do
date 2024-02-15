*******************************
*** Do File for Replication ***
*******************************

Data available from Gesis: https://doi.org/10.7802/2456

use "COVID-19 and Social Inequality A - Welle 3.dta", clear

ge treat=close_RandNumber
ge kita=close1_1
replace kita=close2_1 if kita==.
replace kita=close3_1 if kita==.
replace kita=close4_1 if kita==.

replace kita=0 if kita==1
replace kita=1 if kita==2
label define support 0 "No" 1 "Yes", replace
label values kita support
tab kita, m

ge primary=close1_2
replace  primary=close2_2 if primary==.
replace  primary=close3_2 if primary==.
replace  primary=close4_2 if primary==.
replace primary=0 if primary==1
replace primary=1 if primary==2
label values  primary support
tab  primary, m

ge highs=close1_3
replace highs=close2_3 if highs==.
replace  highs=close3_3 if highs==.
replace  highs=close4_3 if highs==.
replace highs=0 if highs==1
replace highs=1 if highs==2
label values  highs support
tab  highs, m


label define treat 1"Control" 2"Gender" 3"Education" 4"Gender+Education"

label values treat treat
drop if stat1==2
/*
graph bar [pweight= gewicht_W3], over(kita) by(treat) blabel(bar, format(%4.1f)) name(kita, replace)
graph export "${results}/kita.png", replace

graph bar [pweight= gewicht_W3], over(primary) by(treat) blabel(bar, format(%4.1f))  name(prim, replace)
graph export "${results}/prim.png", replace
graph bar [pweight= gewicht_W3], over(highs) by(treat) blabel(bar, format(%4.1f))  name(highs, replace)
graph export "${results}/highs.png", replace


graph bar [pweight= gewicht_W3], over(kita) by(treat stat1) blabel(bar, format(%4.1f))  name(kita_g, replace)
graph export "${results}/kita_g.png", replace
graph bar [pweight= gewicht_W3], over(primary) by(treat stat1) blabel(bar, format(%4.1f)) name(prim_g, replace)
graph export "${results}/prim_g.png", replace
graph bar [pweight= gewicht_W3], over(highs) by(treat stat1) blabel(bar, format(%4.1f))  name(highs_g, replace)
graph export "${results}/highs_g.png", replace
graph bar [pweight= gewicht_W3], over(kita) by(treat stat12) blabel(bar, format(%4.1f)) name(kita_c, replace)
graph export "${results}/kita_c.png", replace
graph bar [pweight= gewicht_W3], over(primary) by(treat stat12) blabel(bar, format(%4.1f)) name(prim_c, replace)
graph export "${results}/prim_c.png", replace
graph bar [pweight= gewicht_W3], over(highs) by(treat stat12) blabel(bar, format(%4.1f))  name(highs_c, replace)
graph export "${results}/highs_c.png", replace
*/

graph bar kita primary highs [pweight= gewicht_W3], over(treat) blabel(bar, format(%4.2f)) yscale(range(0(0.2)0.6)) leg(pos(6)) yline(0.5, lcolor(red)) 
graph bar kita primary highs, over(treat) blabel(bar, format(%4.2f)) yscale(range(0(0.2)0.6)) leg(pos(6)) yline(0.5, lcolor(red)) 

*** Children Variable **

* detailed by age (only for those living with kids) *
recode stat15 (0/2=1) (3/5=2) (6/12=3) (13/16=4) (17/50=5) (1999=5) (2020=1), ge(agekid)
lab define agekid 0"no kids" 1"0-2" 2"3-5" 3"6-12" 4"13-16" 5"16 or over / not in hh"
lab val agekid agekid
ta agekid

* dichotomous *
ge minorkidshh=0 if stat14==0|stat14==6
replace minorkidshh=1 if stat14>0&stat14<6
ta agekid minorkidshh, m

ge children=stat12


* include childless - 5 categories *
ge kids5=agekid
replace kids5=5 if children==1 &stat14==0
replace kids5=5 if children==1 &stat14==.
replace kids5=0 if children==0 & stat14==0 
replace kids5=0 if children==0 & stat14==. 
lab val kids5 agekid

* 3 categories *
ge kidsneu=.
replace kidsneu=1 if minorkidshh==1
replace kidsneu=2 if minorkidshh==0&children==1
replace kidsneu=0 if children==0 & kidsneu==. & minorkidshh==0
lab define kidsneu 0"no kids" 1"kids <16 in hh" 2"has own kids, but not in hh or not <16"
lab val kidsneu kidsneu
lab var kidsneu "children, measured with three categories"

* 4 categories *
recode kids5 (0=0) (1 2 =1) (3 4=2) (5=3), ge (kids4)
lab define kids4 0"no kids" 1"0-5" 2"6-16" 3"over 16/not in hh"
lab val kids4 kids4


*isced
drop isced
gen isced=.
 
replace isced=1 if (stat16==2&stat17==11)|(stat16==2&stat17==.)
 
replace isced=2 if (stat16==3|stat16==4) & (stat17==1|stat17==5|stat17==12 | stat17==11) | ///
                     (stat16==2 &inlist(stat17,1,5,12))
 
replace isced=3 if (inlist(stat16,3,4) & inlist(stat17,2,3,4,6,7)) | ///
                     (inlist(stat16,5,6) & stat17==11) | ///
                                                                       (stat16==2&inlist(stat17,2,3,4,6,7))
 
replace isced=4 if ((stat16==5|stat16==6)&(stat17==2 |stat17==3|stat17==4|stat17==6|stat17==7)) | stat17==8
 
replace isced=5 if stat17==9|stat17==10
 
/*lab def isced 1 "primary education" ///
             2 "lower secondary education" ///
                                          3 "upper secondary education" ///
                                          4 "post secondary education" ///
 
 5 "tertiary education"

 */
 label values isced isced

recode isced (1 2=1)(3 4=2)(5=3), gen (isced3)

**missing 162 obs when using this variable

****add info on school closure at the NUTS3 level
* File available in same folder on GitHub *

sort id submitdate
merge m:1 id submitdate using "opening policies.dta"

 recode schoolkita (2 3 = 2) (4 =3), gen(schoolkita_new)
 tab schoolkita_new

label define schoolkita_new 1 "closed" 2 "partially open" 3 "open"

label values schoolkita_new schoolkita_new

recode schoolkita_new (2 3 = 2), gen(schoolkita_new2)
tab schoolkita_new2


label values schoolkita_new2 schoolkita_new


***add labels
label define ageg 1"18-39" 2 "40-59" 3"60+"
label values stat2_gr3 ageg


ge treat2=(treat>1)

ge pol_miss=(inst4>10)

global des 
global cat stat1 stat2_gr3 stat4 kids4 stat8 isced3 inc_categories  erw1now pol_miss schoolkita_new2
foreach var in $cat{
tab `var', ge(`var'_d)
global des  $des `var'_d*
}

estpost summarize $des, listwise
	

esttab using "$results//tableA1.rtf", replace ///
    cells("count(fmt(a2)) mean sd min max") label ///
    title("Table A1: Descriptive Statistics") nomtitle nonumber noobs 




*check randomization
/*
ge edu= (treat==3|treat==4)
replace edu=. if treat==2
ge gender= (treat==2|treat==4)
replace gender=. if treat==3

estpost ttest $des inst4, by(gender)
  
eststo est1

estpost ttest $des inst4, by(edu)
eststo est2

estpost ttest $des inst4, by(treat2)
eststo est3

esttab est3 est1 est2  using "${results}/rand_test.rtf" ,replace  cell(b(star label(Difference) fmt(2))) wide nonumber noobs label




replace edu=0 if treat==2
replace gender=0 if treat==3
*/



est clear

global controls i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now inst4 pol_miss i.schoolkita_new2

*Table 1:

probit kita i.treat $controls [iweight=gewicht_W3], robust
ge sample_kita=e(sample)
*margins treat, plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post

test 2.treat=3.treat=4.treat


probit kita i.treat [iweight=gewicht_W3] if sample_kita, robust
*margins treat, plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post



probit primary i.treat $controls [iweight=gewicht_W3], robust
ge sample_primary=e(sample)
*margins treat,  plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post

test 2.treat=3.treat=4.treat
test 3.treat=2.treat

probit primary i.treat [iweight=gewicht_W3] if sample_primary, robust
*margins treat, plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post



probit highs i.treat $controls [iweight=gewicht_W3], robust
ge sample_highs=e(sample)
*margins treat,   plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post

probit highs i.treat [iweight=gewicht_W3] if sample_highs, robust
*margins treat, plot(recast(scatter) ciopts(lpattern(dash)))
eststo: margins, dydx(treat) post

esttab est2 est1 est4 est3 est6 est5 using "${results}/table_1.rtf", ///
replace b(%15.3fc) ci  stats(N , labels("Observations"  ))  label nogaps compress nobase 

***run regression also without wights 
est clear
probit kita i.treat $controls, robust
eststo: margins, dydx(treat) post


probit kita i.treat if sample_kita, robust
eststo: margins, dydx(treat) post

probit primary i.treat $controls, robust
eststo: margins, dydx(treat) post


probit primary i.treat  if sample_primary, robust

eststo: margins, dydx(treat) post



probit highs i.treat $controls, robust
eststo: margins, dydx(treat) post

probit highs i.treat if sample_highs, robust
eststo: margins, dydx(treat) post

esttab est2 est1 est4 est3 est6 est5 using "${results}/table_1_NW.rtf", ///
replace b(%15.3fc) ci  stats(N , labels("Observations"  ))  label nogaps compress nobase 

***run regression with logit

est clear
logit kita i.treat $controls , robust
eststo: margins, dydx(treat) post


logit kita i.treat if sample_kita , robust
eststo: margins, dydx(treat) post

logit primary i.treat $controls , robust
eststo: margins, dydx(treat) post


logit primary i.treat  if sample_primary , robust

eststo: margins, dydx(treat) post

logit highs i.treat $controls , robust
eststo: margins, dydx(treat) post

logit highs i.treat if sample_highs , robust
eststo: margins, dydx(treat) post

esttab est2 est1 est4 est3 est6 est5 using "${results}/table_1_logit_NW.rtf", ///
replace b(%15.3fc) ci  stats(N , labels("Observations"  ))  label nogaps compress nobase 


***Interactions


**political left-right

graph bar [aweight=gewicht_W3] if inst4<=10, over(inst4)  by(stat1)

recode inst4 (0 1 2 3=1)(4 5 6=2) (7 8 9 10=3) (11 12=.), gen(left_right)
 
label variable left_right "leftright"

label define leftright 1 "Left" 2 "Center" 3 "Right"

label values left_right leftright

est clear 

*probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  [iweight=gewicht_W3] if inst4<=10, robust

probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Preschool): margins, dydx(treat) by(left_right) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g1)


probit primary i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Primary school): margins, dydx(treat) by(left_right) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g2)
probit highs i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Secondary school): margins, dydx(treat) by(left_right) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g3)


probit kita i.treat##i.schoolkita_new2 $controls, robust
eststo, title(Preschool): margins, dydx(treat) by(schoolkita_new2) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash)) xscale(range(0.5 2.5)) leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g4)
probit primary i.treat##i.schoolkita_new2 $controls , robust
eststo, title(Primary school): margins, dydx(treat) by(schoolkita_new2) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash)) xscale(range(0.5 2.5)) leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g5)
probit highs i.treat##i.schoolkita_new2 $controls , robust
eststo, title(Secondary school): margins, dydx(treat) by(schoolkita_new2) post
marginsplot, recast(scatter) yline(0, lcolor(red) lwidth(thin) lpattern(dash)) xscale(range(0.5 2.5)) leg(pos(6) rows(1))  ciopts(recast(rcap) lpattern(dash) lwidth(thin)) saving(g6)

grc1leg g4 g5 g6
esttab * using "${results}/table_2.rtf",replace b(%15.3fc)  ci   star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle
est clear

probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Preschool): margins i.treat##i.left_right, post

probit primary i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Primary school): margins i.treat##i.left_right, post
probit highs i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo, title(Secondary school): margins i.treat##i.left_right, post

probit kita i.treat##i.schoolkita_new2 $controls , robust
eststo, title(Preschool): margins i.treat##i.schoolkita_new2, post
probit primary i.treat##i.schoolkita_new2 $controls , robust
eststo, title(Primary school):margins i.treat##i.schoolkita_new2,  post
probit highs i.treat##i.schoolkita_new2 $controls , robust
eststo, title(Secondary school): margins i.treat##i.schoolkita_new2,  post


esttab * using "${results}/table_2_pp.rtf",replace b(%15.3fc)  ci   star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle



/*new figure 2 & 3 */



probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==1) post
 probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==2) post
 probit kita i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==3) post


 coefplot (est1, label(Left)) (est2,label(Center)) (est3, label(Right)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap)  lwidth(thin)) title("Preschools") saving(g1, replace) //lpattern(dash)
 
 est clear
 probit primary i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==1) post
probit primary i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==2) post
probit primary i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==3) post
 coefplot (est1, label(Left)) (est2,label(Center)) (est3, label(Right)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap)  lwidth(thin)) title("Primary Schools") saving(g2, replace)
 
 est clear
probit highs i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==1) post
probit highs i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==2) post
probit highs i.treat##i.left_right i.stat1 i.stat2_gr3 i.stat4 i.kids4 i.stat8 i.isced3 i.inc_categories  i.erw1now  if inst4<=10, robust
eststo: margins, dydx(treat) at(left_right==3) post
 coefplot (est1, label(Left)) (est2,label(Center)) (est3, label(Right)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap) lwidth(thin)) title("Secondary Schools") saving(g3, replace)
 

 est clear
grc1leg g1.gph g2.gph g3.gph, rows(1) ycommon iscale(*.9)


 est clear
probit kita i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==1) post
probit kita i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==2) post
 coefplot (est1, label(Closed)) (est2,label(Partially Open)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap)  lwidth(thin)) title("Preschools") saving(g4, replace)
 est clear
probit primary i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==1) post
probit primary i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==2) post
 coefplot (est1, label(Closed)) (est2,label(Partially Open)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap)  lwidth(thin)) title("Primary Schools") saving(g5, replace)
 est clear
probit highs i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==1) post
probit highs i.treat##i.schoolkita_new2 $controls, robust
eststo: margins, dydx(treat) at(schoolkita_new2==2) post
 coefplot (est1, label(Closed)) (est2,label(Partially Open)), vertical  yline(0, lcolor(red) lwidth(thin) lpattern(dash))  ///
 leg(pos(6) rows(1))  ciopts(recast(rcap)  lwidth(thin)) title("Secondary Schools") saving(g6, replace)


grc1leg g4.gph g5.gph g6.gph, rows(1) ycommon iscale(*.9)

/*
probit kita i.treat##i.schoolkita_new2 $controls [iweight=gewicht_W3], robust
eststo, title(kindergarten): margins i.treat##i.schoolkita_new2, pwcompare(group) post
/*

---------------------------------------------------------------------
                                 |            Delta-method Unadjusted
                                 |     Margin   Std. Err.      Groups
---------------------------------+-----------------------------------
                           treat |
                        Control  |   .5725823   .0199558
                         Gender  |   .4658684   .0197975            A
                      Education  |    .473859   .0197885            A
               Gender+Education  |   .4787139   .0194282            A
                                 |
                 schoolkita_new2 |
                         closed  |   .5000807   .0167509            A
                 partially open  |   .4943224    .014239            A
                                 |
           treat#schoolkita_new2 |
                 Control#closed  |   .6091608    .030971            D
         Control#partially open  |    .544532   .0270535           CD
                  Gender#closed  |   .4875588   .0305449         ABC 
          Gender#partially open  |   .4495998   .0272622         AB  
               Education#closed  |   .4985861   .0320159          BC 
       Education#partially open  |   .4552812   .0261419         AB  
        Gender+Education#closed  |   .4115939   .0314365         A   
Gender+Education#partially open  |   .5290737   .0258624           CD
---------------------------------------------------------------------
Note: Margins sharing a letter in the group label are not
      significantly different at the 5% level.
*/
probit primary i.treat##i.schoolkita_new2 $controls [iweight=gewicht_W3], robust
eststo, title(Primary): margins i.treat##i.schoolkita_new2, pwcompare(group) post
/*
---------------------------------------------------------------------
                                 |            Delta-method Unadjusted
                                 |     Margin   Std. Err.      Groups
---------------------------------+-----------------------------------
                           treat |
                        Control  |   .5487063   .0198861
                         Gender  |   .4737888   .0196665            A
                      Education  |   .4685256    .019598            A
               Gender+Education  |    .478833   .0194047            A
                                 |
                 schoolkita_new2 |
                         closed  |   .4970151   .0166917            A
                 partially open  |   .4878056   .0141358            A
                                 |
           treat#schoolkita_new2 |
                 Control#closed  |   .5859799   .0310077            E
         Control#partially open  |   .5204666   .0269333          CDE
                  Gender#closed  |   .4942097   .0307159         BCD 
          Gender#partially open  |   .4584836   .0268423        ABC  
               Education#closed  |   .5092074   .0315532         BCDE
       Education#partially open  |   .4380467   .0259274        AB   
        Gender+Education#closed  |   .4045225    .031324        A    
Gender+Education#partially open  |   .5344738   .0258692           DE
---------------------------------------------------------------------
Note: Margins sharing a letter in the group label are not
      significantly different at the 5% level.
*/

probit highs i.treat##i.schoolkita_new2 $controls [iweight=gewicht_W3], robust
eststo, title(Secondary): margins i.treat##i.schoolkita_new2, pwcompare(group) post
/*
---------------------------------------------------------------------
                                 |            Delta-method Unadjusted
                                 |     Margin   Std. Err.      Groups
---------------------------------+-----------------------------------
                           treat |
                        Control  |   .5604457   .0198047            A
                         Gender  |   .5150743   .0192524            A
                      Education  |   .5187774    .019313            A
               Gender+Education  |   .5103213   .0192087            A
                                 |
                 schoolkita_new2 |
                         closed  |   .5358653    .016477            A
                 partially open  |   .5177964   .0140049            A
                                 |
           treat#schoolkita_new2 |
                 Control#closed  |   .6000276   .0303895            D
         Control#partially open  |   .5303277   .0270876          BCD
                  Gender#closed  |   .5335903    .030042          BCD
          Gender#partially open  |   .5010952   .0264327         ABC 
               Education#closed  |   .5705777   .0306872           CD
       Education#partially open  |   .4796198    .025828         AB  
        Gender+Education#closed  |   .4446257    .031322         A   
Gender+Education#partially open  |   .5597982   .0254438           CD
---------------------------------------------------------------------
Note: Margins sharing a letter in the group label are not
      significantly different at the 5% level.
*/

esttab  using "${results}/table_9.rtf",replace b(%15.3fc) p star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle


///individuals living where school were closed during the interview are on average more supportive of closures, but react with a stronger decrease in their support (significative at 5%) once faced with the combination of gender end edu treatment on all the level of education///


probit kita i.treat##i.schoolkita_new2##stat1 $controls [iweight=gewicht_W3], robust
probit primary i.treat##i.schoolkita_new2##stat1 $controls [iweight=gewicht_W3], robust


/*
probit kita i.treat##c.inst4##i.stat1 $controls [iweight=gewicht_W3] if inst4<=10, robust
margins, dydx(2.treat) at(inst4=(0(1)10) stat1=(0 1)) plot(ciopts(lpattern(dash)) by(stat1))
margins, dydx(3.treat) at(inst4=(0(1)10) stat1=(0 1)) plot(ciopts(lpattern(dash)) by(stat1))

probit primary ii.treat##c.inst4##i.stat1 $controls [iweight=gewicht_W3] if inst4<=10, robust
margins, dydx(treat) at(inst4=(0(1)10)) plot

probit highs i.treat##c.inst4 $controls [iweight=gewicht_W3] if inst4<=10, robust
margins, dydx(treat) at(inst4=(0(1)10)) plot

*education 
est clear

qui probit kita i.treat##i.isced3 $controls [iweight=gewicht_W3], robust


eststo, title(kindergarten): margins treat,  by(isced3) pwcompare(group)
margins, dydx(treat)   by(isced3) pwcompare


/*Pairwise comparisons of predictive margins      Number of obs     =      3,142
Model VCE    : Robust

Expression   : Pr(kita), predict()

--------------------------------------------------------
                    |            Delta-method Unadjusted
                    |     Margin   Std. Err.      Groups
--------------------+-----------------------------------
              treat |
           Control  |   .5761413   .0199901
            Gender  |   .4629813   .0200698            A
         Education  |   .4797029   .0203059            A
  Gender+Education  |    .482977   .0201415            A
                    |
             isced3 |
                 1  |    .437688   .0386512           A 
                 2  |   .4986285    .012152           AB
                 3  |   .5306094   .0225356            B
                    |
       treat#isced3 |
         Control#1  |   .5118186   .0805765          ABC
         Control#2  |   .5808925   .0240194            C
         Control#3  |   .5864915   .0398421            C
          Gender#1  |   .3860963   .0745561          A  
          Gender#2  |   .4585678   .0236496          AB 
          Gender#3  |   .5089462    .044697          ABC
       Education#1  |   .3980315    .075008          AB 
       Education#2  |   .4942425   .0242328          AB 
       Education#3  |   .4647123   .0443454          AB 
Gender+Education#1  |   .4555182   .0713714          ABC
Gender+Education#2  |   .4624312   .0242072          AB 
Gender+Education#3  |   .5624165   .0436526           BC
--------------------------------------------------------
Note: Margins sharing a letter in the group label are
      not significantly different at the 5% level.

*/

qui probit primary i.treat##i.isced3 $controls [iweight=gewicht_W3], robust

eststo, title(Primary): margins i.treat##i.isced3,  pwcompare post

qui probit highs i.treat##i.isced3 $controls [iweight=gewicht_W3], robust

eststo, title(Secondary): margins i.treat##i.isced3, pwcompare(group) post

esttab  using "${results}/table_5.rtf",replace b(%15.3fc) p star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle

*income 
est clear

qui probit kita i.treat##i.inc_categories $controls [iweight=gewicht_W3], robust

eststo, title(kindergarten): margins treat,  by(inc_categories) pwcompare(group)post


qui probit primary  i.treat##i.inc_categories $controls [iweight=gewicht_W3], robust

eststo, title(Primary): margins, dydx(treat) at(inc_categories=(1 2 3 4 5 6)) pwcompare post

qui probit highs  i.treat##i.inc_categories $controls [iweight=gewicht_W3], robust

eststo, title(Secondary): margins, dydx(treat) at(inc_categories=(1 2 3 4 5 6)) pwcompare post

esttab  using "${results}/table_6.rtf",replace b(%15.3fc) p star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle

*household duty share

tab geschl2b, m
 ge eq_share=(geschl2b==3)
 replace eq_share=. if geschl2b==.
 
 ge uneq_share=eq_share
 replace uneq_share=2 if geschl2b>3
  replace uneq_share=0 if geschl2b<3
  replace uneq_share=. if geschl2b==.
  
  label define share 0 "more than partner" 1 " equal as partner" 2"less than partner"
  label values uneq_share share
est clear

qui probit kita i.treat##i.uneq_share i.stat16_gr3 i.stat12  i.stat1 i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins i.treat##i.uneq_share, plot( by(uneq_share))
margins, dydx(3.treat) at(uneq_share=(0  2)) level(90) plot (recast(scatter))
eststo, title(kindergarten): margins, dydx(treat) at(uneq_share=(0 1 2)) pwcompare post

qui probit primary i.treat##i.eq_share i.stat16_gr3 i.stat12  i.stat1 i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins i.treat##i.eq_share, plot

eststo, title(Primary): margins, dydx(treat) at(eq_share=(0 1)) pwcompare post

qui probit secondary i.treat##i.eq_share i.stat16_gr3 i.stat12  i.stat1 i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins i.treat##i.eq_share, plot

eststo, title(Secondary): margins, dydx(treat) at(eq_share=(0 1)) pwcompare post

esttab  using "${results}/table_7.rtf",replace b(%15.3fc) p star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle

*household duty share +gender

est clear

qui probit kita i.treat##i.uneq_share##i.stat1  i.stat16_gr3 i.stat12  i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins treat, by(stat1 uneq_share) 
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(eq_share)
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(stat1)
 pwcompare  i.treat##i.uneq_share##i.stat1, level(90)  group sort

eststo, title(kindergarten): margins, dydx(treat) at(uneq_share=(0 1 2) stat1=(0 1)) pwcompare level(90) post

qui probit primary i.treat##i.eq_share##i.stat1  i.stat16_gr3 i.stat12  i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins treat, by(stat1 eq_share) 
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(eq_share)
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(stat1)


eststo, title(Primary): margins, dydx(treat) at(eq_share=(0 1) stat1=(0 1)) pwcompare post

qui probit highs i.treat##i.eq_share##i.stat1  i.stat16_gr3 i.stat12  i.stat2_gr3 i.stat4  i.stat8  i.inc_categories i.erw1now [iweight=gewicht_W3], robust

margins treat, by(stat1 eq_share) 
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(eq_share)
marginsplot, recast(scatter) ciopts(lpattern(dash)) by(stat1)


eststo, title(Secondary): margins, dydx(treat) at(eq_share=(0 1) stat1=(0 1)) pwcompare post

esttab  using "${results}/table_8.rtf",replace b(%15.3fc) p star(* 0.1 ** 0.05 *** 0.01) stats(N , labels("Observations"  ))  label nogaps compress nobase noomit mtitle




****************number of supported policies

bysort id: ge n_support=kita+primary+highs
tab n_support
 poisson n_support i.treat $controls [iweight=gewicht_W3], robust

 
