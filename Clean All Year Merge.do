 clear all
prog drop _all
capture log close
set more off

******************************************
** Import Datasets and Prepare
******************************************

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS/2010"
use family_head_2010, clear
gen year = 2010
cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
save family_head_2010, replace

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS/2012"
use family_head_2012, clear
drop edu_highest
rename edu_latest edu_highest
gen year = 2012
cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
save family_head_2012, replace

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS/2014"
use family_head_2014, clear
drop edu_highest
rename edu_latest edu_highest
gen year = 2014
sort fid12
cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
save family_head_2014,replace

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone 2016-2017/Data/Raw - CFPS"
use family_head_2014, clear
append using family_head_2012
append using family_head_2010
sort fid10
drop head

save family_head_all,replace

use family_head_all, clear 
bysort fid10: gen num_obs=_n
table num_obs
drop num_obs



******************************************
** Check Vavirables
******************************************
 
* debt - test restriction
sum debt_tot if year == 2010 & f_income < 2000000 & debt_tot < 2000000
sum debt_tot if year == 2012 & f_income < 2000000 & debt_tot < 2000000
sum debt_tot if year == 2014 & f_income < 2000000 & debt_tot < 2000000

graph twoway (scatter debt_tot f_income, ms(o) mc(gs4) msize(small))

* children
table children if year == 2010
table children if year == 2012
table children if year == 2014
	/* in year 2010 there are only 6k in the children dataset and are all merged. Children is constant*/

* asset_financial 

sum asset_financial if year == 2010 & f_income < 2000000 & asset_financial < 5000000
sum asset_financial if year == 2012 & f_income < 2000000 & asset_financial < 5000000
sum asset_financial if year == 2014 & f_income < 2000000 & asset_financial < 5000000
	/* asset in 2010 is much bigger; back and redefine*/

* asset_cash_deposit 

sum asset_cash_deposit if year == 2010 & f_income < 2000000 & asset_cash_deposit < 5000000
sum asset_cash_deposit if year == 2012 & f_income < 2000000 & asset_cash_deposit < 5000000
sum asset_cash_deposit if year == 2014 & f_income < 2000000 & asset_cash_deposit < 5000000
	/* */

graph twoway (scatter asset_financial f_income, ms(o) mc(gs4) msize(small))

* houseprice

sum house_price if year == 2010 & f_income < 2000000 & house_price < 7500000
sum house_price if year == 2012 & f_income < 2000000 & house_price < 7500000
sum house_price if year == 2014 & f_income < 2000000 & house_price < 7500000

sum house_price_tot if year == 2010 & f_income < 2000000 
sum house_price_tot if year == 2012 & f_income < 2000000 
sum house_price_tot if year == 2014 & f_income < 2000000 
	/* problem: a lot of missing for second houses for year 2012 */
graph twoway (scatter house_price f_income, ms(o) mc(gs4) msize(small))


******************************************
** Restrict the data I
******************************************

drop head
drop if fid10==.
keep if f_income < 2000000 | f_income==.
keep if debt_tot < 2000000 | debt_tot==.
keep if asset_financial < 5000000 | asset_financial==.
keep if house_price < 10000000 | house_price==.

sum f_income
su expense
sum house_price
sum asset_cash_deposit
sum asset_financial
sum debt_tot

save family_head_all_restrict1, replace


******************************************
** Restrict the data II
******************************************

cd "/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS"
use family_head_all_restrict1, clear

* Replace missing with 0's
replace house_price = 0 if house_price ==.
replace asset_cash_deposit = 0 if asset_cash_deposit==.
replace asset_financial = 0 if asset_financial==.
replace debt_mortgage_tot = 0 if debt_mortgage_tot==.
replace debt_frind_other_ins = 0 if debt_frind_other_ins==.
replace children = 0 if children==.

* Deal with missings
replace gender =. if gender<0
replace age =. if age<0
replace marriage =. if marriage <0
replace health =. if health <0
replace p_income=. if p_income <0
replace employ =. if employ ==8 | employ ==-8

save family_head_all_restrict2, replace
** Turn to R from here 
