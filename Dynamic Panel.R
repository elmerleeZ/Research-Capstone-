#####################
# Prepare
#####################

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
load("family_head_all_restrict_final_1.RData")

install.packages("dynpanel")
library(dynpanel)

################################################
# A-B with dynpanel
################################################

# First turn into a list 
data1 <- family_head_all_restrict_final_1 %>% purrr::map(as.list) 

# Run model
income_p_dpd <- dpd(income_p~factor(provcd) + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize
			     + factor(ethnicity) + age + gender + factor(edu_highest) + factor(marriage) + factor(health) + p_income + employ, 
					data=family_head_all_restrict_final_1, index=c("fid10", "year"),1,0)


