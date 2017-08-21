
capture log close
log using "Matching", text replace
/****************************************************************************
Program Name:   Matching.do
Location:       GitHub\
Author:         Zongyang (Elmer) Li
Date Created:   2017
Project:        
****************************************************************************/

clear all
prog drop _all
set more off

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone 2016-2017/Data/Raw - CFPS"

ssc install nnmatch

********************************************************************************
* Create Annuity Value
*****

/*** calculate life expectancy ***/

* import
insheet using "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone 2016-2017/Data/Raw - CFPS/other/Life_expectancy.csv", clear 
* reshape to long format (for merging) - provcd starting with 7 are those regions not included
reshape long life_expectancy, i(provcd) j(gender)
drop province
save "life_expectancy"

/*** merge data ***/ 

use family_head_all_restrict_final_2, clear
	** Still some variable to be changed: 
	rename expense expense_old
	gen expense = expense_old/1000
	replace gender =. if gender<0

merge m:1 provcd gender using "life_expectancy"
	drop if _merge==2
	drop _merge
gen remain_life = life_expectancy - age

/*** Calculate income + anuity asset ***/ 

local r = 0.02
gen tot_inc_asset_net =  f_income + asset_net * (`r'/(1-(1+`r')^(-remain_life)))
save "family_head_all_restrict_final_2", replace


********************************************************************************
* Match
*****

* keep only two transient groups, create treatement variable
drop if income_p_asset_p_wb == 1 | income_np_asset_np_mls == 1 /* 10,880  left */
generate treat = 1 if income_p_asset_np_mls == 1 /* 6631 treated, 4249 untreated */
replace treat = 0 if income_np_asset_p_mls == 1

* match to see outcomes

gen id = _n
gen index = _n
save, replace


/*** Education ***/

* match tot_inc_asset_net

nnmatch edu_highest treat tot_inc_asset_net,pop m(1) keep(match_info_edu_1) tc(ate) replace

use "match_info_edu_1", clear
collapse treat edu_highest tot_inc_asset_net km km_prime dist edu_highest_0 edu_highest_1 tot_inc_asset_net_0m tot_inc_asset_net_1m, by(id)

	* check matching
	twoway (scatter tot_inc_asset_net_1m id, ms(O) mc(red) msize(small)) ///
	 (scatter tot_inc_asset_net_0m id, ms(O) mc(blue) msize(small))
	graph export "test.png", replace

	twoway (scatter tot_inc_asset_net_0m tot_inc_asset_net_1m, ms(O) mc(red) msize(small)) ///
		function y = x, ra(tot_inc_asset_net_0m) clpat(dash)


* match tot_inc_asset_net age gender ethnicity familysize, outcome = education

use "family_head_all_restrict_final_2", clear

nnmatch edu_highest treat tot_inc_asset_net age gender ethnicity familysize,pop m(1) keep(match_info_edu_2) tc(ate) replace

use "match_info_edu_2", clear
collapse treat edu_highest tot_inc_asset_net km km_prime dist edu_highest_0 edu_highest_1 tot_inc_asset_net_0m tot_inc_asset_net_1m, by(id)

	* check matching
	twoway (scatter tot_inc_asset_net_1m id, ms(O) mc(red) msize(small)) ///
	 (scatter tot_inc_asset_net_0m id, ms(O) mc(blue) msize(small))
	graph export "test.png", replace

	twoway (scatter tot_inc_asset_net_0m tot_inc_asset_net_1m, ms(O) mc(red) msize(small)) ///
		function y = x, ra(tot_inc_asset_net_0m) clpat(dash) 


/*** Health ***/

* match tot_inc_asset_net
use "family_head_all_restrict_final_2", clear

nnmatch health treat tot_inc_asset_net,pop m(1) keep(match_info_hth_1) tc(ate) replace

use "match_info_hth_1", clear
collapse treat health tot_inc_asset_net km km_prime dist health_0 health_1 tot_inc_asset_net_0m tot_inc_asset_net_1m, by(id)

	* check matching
	twoway (scatter tot_inc_asset_net_1m id, ms(O) mc(red) msize(small)) ///
	 (scatter tot_inc_asset_net_0m id, ms(O) mc(blue) msize(small))
	graph export "test.png", replace

	twoway (scatter tot_inc_asset_net_0m tot_inc_asset_net_1m, ms(O) mc(red) msize(small)) ///
		function y = x, ra(tot_inc_asset_net_0m) clpat(dash)



********************************************************************************
* Use Panel Method to See the Treatment effect
*****

/*** Education ***/
use "family_head_all_restrict_final_2", clear

xi: reg edu_highest treat age gender gender ethnicity familysize ///
pct_poverty  pct_black  pct_hispanic  pct_asian  pct_hl_neng  pct_pob_nyc  pct_pob_usa num_student ///
i.d_n  i.grade_level, vce(cluster dbn)






************************************************************
************************************************************
********************    END PROGRAM    *********************
************************************************************
************************************************************

log close
