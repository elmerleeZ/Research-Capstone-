****************************************************************
*** Prepare
****************************************************************
clear all
prog drop _all
capture log close
set more off

findit xtabond2
ssc install xtabond2
findit femlogit
ssc install femlogit

cd "/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
cd "C:/Users/zyl220/Downloads/temp/poverty"
use family_head_all_restrict_final_1, clear

*****************************************************************
**** Organize Data
*****************************************************************

****gen dummies for year
gen year10=0
replace year10=1 if year==2010

gen year12=0
replace year12=1 if year==2012

gen year14=0
replace year14=1 if year==2014

****gen provine dummies
gen Beijing=0
replace Beijing=1 if provcd==11

gen Tianjin=0
replace Tianjin=1 if provcd==12 

gen Hebei=0
replace Hebei=1 if provcd==13 

gen Shanxi=0
replace Shanxi=1 if provcd==14

gen Liaoning=0
replace Liaoning=1 if provcd==21

gen Jilin=0
replace Jilin=1 if provcd==22  

gen Heilongjiang=0
replace Heilongjiang=1 if provcd==23

gen Shanghai=0
replace Shanghai=1 if provcd==31

gen Jiangsu=0
replace Jiangsu=1 if provcd==32 

gen Zhejiang=0
replace Zhejiang=1 if provcd==33

gen Anhui=0
replace Anhui=1 if provcd==34

gen Fujian=0
replace Fujian=1 if provcd==35

gen Jiangxi=0
replace Jiangxi=1 if provcd==36

gen Shandong=0
replace Shandong=1 if provcd==37

gen Henan=0
replace Henan=1 if provcd==41

gen Hubei=0
replace Hubei=1 if provcd==42

gen Hunan=0
replace Hunan=1 if provcd==43

gen Guangdong=0
replace Guangdong=1 if provcd==44

gen Guangxi=0
replace Guangxi=1 if provcd==45

gen Chongqing=0
replace Chongqing=1 if provcd==50

gen Sichuan=0
replace Sichuan=1 if provcd==51

gen Guizhou=0
replace Guizhou=1 if provcd==52

gen Yunnan=0
replace Yunnan=1 if provcd==53

gen Shannxi=0
replace Shannxi=1 if provcd==61

gen Gansu=0
replace Gansu=1 if provcd==62                  

/*** Health ***/
gen health1=0
replace health1=1 if health==1 | health==2 | health==3 /* Create dummy for health (1:very good 0:other)*/
gen health2=0
replace health2=1 if health==4
gen health3=0
replace health3=1 if health==5

/*** Education ***/
replace edu_highest=. if edu_highest==-8
gen edu1=0
replace edu1=1 if edu_highest<=1
gen edu2=0
replace edu2=1 if edu_highest==2 
gen edu3=0 
replace edu3=1 if edu_highest==3 | edu_highest==4
gen edu4=0 
replace edu4=1 if edu_highest>=5

/*** Marriage ***/
gen marr=0
replace marr=1 if marriage==2 | marriage==3

/*** Employment ***/
gen employed=0
replace employed=1 if employ==1

/*** Age ***/
gen age2=age*age

/*** Houseownership ***/
rename house_ownership house_ownership_old
gen house_ownership = house_ownership_old
replace house_ownership=0 if house_ownership_old!=1

/*** Ethnicity ***/
gen eth_han=0
replace eth_han=1 if ethnicity==1

/*** Dependent ***/
   * gen ln_income=ln(f_income)
   * gen ln_asset=ln(asset_liq)
   * replace ln_asset=0 if ln_asset==.

gen category_mls = 1 if income_p_asset_p_mls==1
replace category_mls = 2 if income_np_asset_p_mls==1
replace category_mls = 3 if income_p_asset_np_mls==1
replace category_mls = 0 if income_np_asset_np_mls==1

gen category_wb = 1 if income_p_asset_p_wb==1
replace category_wb = 2 if income_np_asset_p_wb==1
replace category_wb = 3 if income_p_asset_np_wb==1
replace category_wb = 0 if income_np_asset_np_wb==1

save family_head_all_restrict_final_2, replace

****************************************************************
*** Mutinomial Regression
****************************************************************
clear all
prog drop _all
capture log close
set more off

cd "/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
cd "C:/Users/zyl220/Downloads/temp"
use family_head_all_restrict_final_2, clear

** Still expense need to be changed: 
rename expense expense_old
replace expense = expense_old/1000

set more off
log using "/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Output/Regression_motinomial.log", replace
log using "C:/Users/zyl220/Downloads/temp/Regression_motinomial.log", replace

** Set panelvar
xtset fid10 year

*** FULL ***

/*** Poverty Category MLS ***/
set more off
femlogit category_mls urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit1.doc, replace

/*** Poverty Category WB ***/
set more off
femlogit category_wb urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit2.doc, replace






*** SUBSET ***

/*** Poverty Category MLS ***/
** WITH ASSET

/*** no year effect
set more off
femlogit category_mls urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit1.doc, append 

** no year no prov effect
set more off
femlogit category_mls urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed
outreg2 using femlogit1.doc, append 
*/

** WITHOUT ASSET
/***  no income asset effect
   ** this doesn't work! 
set more off
femlogit category_mls urban expense house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit1.doc, append

**  no asset effect
   ** this won't work I guess
set more off
femlogit category_mls urban f_income expense house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit1.doc, append
*/


**  no year no prov no asset effect
set more off
femlogit category_mls urban expense house_ownership old children depen familysize ///
   year12 year14 eth_han age age2 gender edu2 edu3 edu4 marr health1 employed, group(fid10) b(3) 
outreg2 using femlogit1.doc, replace 

set more off
femlogit category_mls urban expense house_ownership old children depen familysize ///
   year12 year14 eth_han age age2 gender edu2 edu3 edu4 marr health1 employed, group(fid10) b(3) or
outreg2 using femlogit12.doc, tstat eform see replace

/***  no year no prov with specific asset effect
   ** this doesn't work! 
set more off
femlogit category_mls urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed
outreg2 using femlogit1.doc, append 

**  no year no prov with asset_net effect
   ** this doesn't work! 
set more off
femlogit category_mls urban f_income expense asset_net house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed
outreg2 using femlogit1.doc, append 
*/



/*** Poverty Category WB ***/
** WITH ASSET

/*** no year effect
set more off
femlogit category_wb urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit2.doc, append 

** no prov effect
set more off
femlogit category_wb urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed
outreg2 using femlogit2.doc, append 
*/


** WITHOUT ASSET

/*** no income asset
set more off
femlogit category_wb urban expense house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit2.doc, append

** no asset
set more off
femlogit category_wb urban f_income expense house_ownership old children depen familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, group(fid10) b(0)
outreg2 using femlogit2.doc, append*/


** no asset no year no prov
set more off
femlogit category_wb urban expense house_ownership old children depen familysize ///
   year12 year14 eth_han age age2 gender edu2 edu3 edu4 marr health1 employed, group(fid10) b(3)
outreg2 using femlogit2.doc, replace

set more off
femlogit category_wb urban expense house_ownership old children depen familysize ///
   year12 year14 eth_han age age2 gender edu2 edu3 edu4 marr health1 employed, group(fid10) b(3) or
outreg2 using femlogit22.doc, tstat eform see replace


















****************************************************************
*** Back Up
****************************************************************

mlogit category_mls urban expense asset_cash_deposit debt_tot familysize, base(3)
   
mlogit category_mls urban expense asset_cash_deposit asset_financial house_ownership house_price debt_tot  old  children  depen  familysize ///
   eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 p_income employed ///
   year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, base(3)

/*** fixed effects seperate ***/
areg category_wb urban f_income expense asset_liq house_price debt_tot house_ownership old children depen familysize, absorb(fid10)
outreg2 using cate.doc, replace ctitle(both) 

areg inc_poor urban expense old children depen familysize eth_han age age2 gender edu2 edu3 edu4 marr health1 health2 emplo year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, absorb(fid10)
outreg2 using cate.doc, append ctitle(income) 
areg ass_poor urban expense old children depen familysize ethni age age2 gender edu2 edu3 edu4 marr health1 health2 emplo year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, absorb(fid10)
outreg2 using cate.doc, append ctitle(asset) 
areg non_poor urban expense old children depen familysize ethni age age2 gender edu2 edu3 edu4 marr health1 health2 emplo year12 year14 Tianjin Hebei Shanxi Liaoning Jilin Heilongjiang Shanghai Jiangsu Zhejiang Anhui Fujian Jiangxi Shandong Henan Hubei Hunan Guangdong Guangxi Chongqing Sichuan Guizhou Yunnan Shannxi Gansu, absorb(fid10)
outreg2 using cate.doc, append ctitle(non) 

/*** sem ***/
gsem (1.category_mls <- urban expense asset_cash_deposit asset_financial house_ownership RI2[fid10])
     (2.category_mls <- urban expense asset_cash_deposit asset_financial house_ownership RI3[fid10]), mlogit

/*** femlogit ***/
femlogit category_mls urban expense f_income debt_tot, group(fid10) b(0)

****************************************************************
*** A-Bound Example
****************************************************************

/*** example 1 ***/

webuse abdata

xtabond2 n L(1/2).n L(0/1).w L(0/2).(k ys) yr*, gmm(L.n) ///
iv(L(0/1).w L(0/2).(k ys) yr*) nolevel robust small

xtabond2 n L(1/2).n L(0/1).w L(0/2).(k ys) yr*, gmm(L(n w k))///
iv(L(0/2).ys yr*) nolevel robust small

/*** example 2 ***/
xtabond2 kc L.kc cgnp _I*, gmm(L.kc cgnp, lag(2 8)) iv(_I* L.openc) ///
twostep robust nodiffsargan

xtabond2 kc L.kc cgnp _I*, gmm(L.kc cgnp, lag(2 8)) iv(_I* L.openc) ///
twostep robust nodiffsargan orthog


****************************************************************
*** A-Bound Regression
****************************************************************

tsset fid10 year 

xtabond2 f_income L.f_income L.(expense asset_tot debt_tot old children) year, gmm(expense asset_tot debt_tot old children lag(2 2)) ///
iv(L(depen familysize)) nolevel robust small


f_income  expense asset_tot debt_tot old children depen familysize