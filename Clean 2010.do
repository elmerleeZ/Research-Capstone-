clear all
prog drop _all
capture log close
set more off

cd "/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS/2010"

********************************************************************************
* Partition Datasets
*****

** gender age party ethinicity(qa5code), highest level of edu attained(cfps2010edu_best), years of edu(cfps2010eduy_best), current marital status(qe1), 
** self-eval health(qp3), employment(qg3), the year you participate the party(qa701),
** type of employer not available for this dataset
* in 2010, personal income is (income)
* no party info

/*** Seperate Independent ***/
use cfps2010adult_report_nat072016, clear
keep pid fid income gender qa1age qa701 qa5code cfps2010edu_best qe1 qp3 qg3
save independent_2010

/*** Seperate Ethnicity ***/
use cfps2010adult_report_nat072016, clear
keep pid fid qa5code
rename fid fid10
rename qa5code ethnicity
save ethnicity_2010, replace

/*** Filter Household head ***/
* Head defined by largest income
use independent_2010, clear 
bysort fid: egen head=max(income)
keep if head==income

* within a household, keep the first one when their personal income are the same * (14,608 households)
bysort fid: gen head_order=_n
keep if head_order==1

* Rename variables
rename qa1age age
rename qa701 party
rename qa5code ethnicity
rename cfps2010edu_best edu_highest
rename qe1 marriage
rename qp3 health
rename qg3 employ
rename income p_income
rename fid fid10
drop head_order head 

save head_independent_2010,replace

/*** Exploratory Sum Statistics ***/
tab gender
su age 
tab edu_highest
tab marriage
tab health
tab employ  
/* Notes:variables like party and ethinicity have a lot of missing */

********************************************************************************
* Family  Level Variables
*****

** number of old       （0.48 on average）
use independent_2010, clear
gen old=0
replace old=1 if qa1age>=60
collapse (sum) old, by (fid)
su old
save old_2010, replace

** number of dependent    （0.47 on average）
use independent_2010, clear
gen depen=0
replace depen=1 if income==0 & qa1age>=16
collapse (sum) depen, by (fid)
su depen
save depen_2010, replace

** number of children       （1.42 on average）
use cfps2010child_report_nat072016, clear
gen children=0
replace children=1 if wa1age<16
collapse (sum) children, by (fid)
su children
save children_2010, replace

********************************************************************************
* Financial Information
*****

***************get the info of income and expenses  （income:55534, expense:57387）
use cfps2010family_report_nat072016, clear
keep fid provcd countyid urban familysize ff601 ff401 expense fd1 fd4 fd703 total_asset savings stock funds debit_other company otherasset valuable nonhousing_debts house_debts fh201_a_1 fh201_a_3 fh201_a_5 fh201_a_6   

* recode these values into missing
mvdecode _all, mv(-8)
mvdecode _all, mv(-1)
mvdecode _all, mv(-2)

* rename all the variables 
	* gen fincome_tot
recode ff401 .=0
egen f_income=rowtotal(ff601 ff401)


gen house_ownership = fd1
gen house_price = fd4*10000
gen house_price_other = fd703*10000
egen house_price_tot = rowtotal(house_price house_price_other)

	* gen asset_cash
	* gen asset_deposit
gen asset_cash_deposit = savings

egen asset_financial = rowtotal(stock funds)
	* gen asset_financial_income
	
	
gen debt_mortgage_tot = house_debts
*attention: it just includes housing mortgage, no info of cars and etc
gen debt_bank = fh201_a_1
gen debt_frind_other_ins = fh201_a_3+fh201_a_5+fh201_a_6
egen debt_tot = rowtotal(debt_mortgage_tot  debt_bank  debt_frind_other_ins)

rename fid fid10
keep fid10 provcd countyid urban familysize f_income expense house_ownership house_price house_price_tot asset_cash_deposit asset_financial debt_mortgage_tot debt_frind_other_ins debt_tot
save family_2010, replace


sum f_income
su expense
sum house_price_tot
sum asset_cash_deposit
sum asset_financial
sum debt_tot


****
* ff601:total family income excluding pension and subsidy
* ff401:total income from pension/social security/subsistence

* expense:total faily expense for the last 12 months(13% missing)
* fd1:home ownership
* fd4:current value of the house last month
* fd703:total current value of other house price(unit:10,000yuan)


* total_asset: Net family assets
* savings:total cash/deposit
* stock:Stock (yuan)
* funds:funds (yuan)
* debit_other: money let out to others(yuan)
* company:company assets(yuan)
* otherasset:other assets(yuan)
* valuable:market price of valuable collections(yuan)


* nonhousing_debts:Financial debt(except housing mortgage)(yuan)
* house_debts:total amount of mortgage for all houses(yuan)
* fh201_a_1:bank loan last year(yuan)
* fh201_a_3:money borrowed from friends/relatives
* fh201_a_5:private loan from the market(yuan)
* fh201_a_6:other loans(yuan)


*** Merge family and household head independent_2010 ***

use family_2010, clear
rename fid10 fid
merge 1:1 fid using old_2010
drop _merge
save family_2010, replace

use family_2010, clear
merge 1:1 fid using depen_2010
drop _merge
save family_2010, replace

use family_2010, clear
merge 1:1 fid using children_2010
drop _merge
save family_2010, replace

use family_2010, clear
rename fid fid10
merge 1:1 fid using head_independent_2010


** restrict the sample (total 14608)
keep if _merge==3
drop if fid10==.

drop _merge
save family_head_2010, replace


