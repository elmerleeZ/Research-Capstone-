#####################
# Prepare
#####################

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
install.packages("readstata13")
library(readstata13)

################################################
# Clean
################################################

family_head_all_restrict2 <- read.dta13("family_head_all_restrict2.dta")

family_head_all_restrict2_0 <- family_head_all_restrict2 %>%
	# dplyr::select(-head) %>% 
	# dplyr::mutate(debt_tot = debt_mortgage_tot + debt_frind_other_ins) %>%
	group_by(fid10) %>% # here only group by fid10 to see duplicates across years
    dplyr::mutate(num_obs = n()) %>%
    select(fid14, fid12, fid10, year, provcd, countyid, urban, f_income, expense, asset_cash_deposit, asset_financial, house_ownership, house_price, house_price_tot, debt_mortgage_tot, debt_frind_other_ins, debt_tot, old, children, depen, familysize, pid, ethnicity, age, party, gender, edu_highest, marriage, health, p_income, employ, num_obs)

table(family_head_all_restrict2_0$num_obs)
	# 1     2     3     4     5       6     7     8 
	# 1523  4112 28917  4484  2025   222   154    56 


## Keep obs with complete
# family_head_all_restrict2_1 <- family_head_all_restrict2_0 %>% 
#   filter(num_obs ==3) %>% 
#   group_by(fid10,year) %>%
#   dplyr::mutate(num_obs = n()) %>% ungroup()

#   table(family_head_all_restrict2_1$num_obs)

  
## Only keep obs with num_obs 3 or more
family_head_all_restrict2_2 <- family_head_all_restrict2_0 %>%
	filter(num_obs >=3) %>%
	group_by(year,fid10) %>% # here group by both fid10 and year to see duplicates in a single year
	dplyr::mutate(num_obs = n())

table(family_head_all_restrict2_2$num_obs)
	# 1     2     3     4     5 
	# 31521  3780   492    60     5 

family_head_all_restrict2_20 <- family_head_all_restrict2_2 %>% filter(num_obs ==1) 

family_head_all_restrict2_21 <- family_head_all_restrict2_2 %>%
	filter(num_obs>1) %>%
	group_by(year,fid10) %>%
	summarise(fid14 = first(fid14),
			fid12 = first(fid12),
			provcd = first(provcd),
			countyid = first(countyid),
			urban = first(urban),
			f_income = mean(f_income,na.rm=T),
			expense = mean(expense,na.rm=T),
			asset_cash_deposit = mean(asset_cash_deposit,na.rm=T),
			asset_financial = mean(asset_financial,na.rm=T),
			house_ownership = mean(house_ownership,na.rm=T),
			house_price = mean(house_price,na.rm=T),
			house_price_tot = mean(house_price_tot,na.rm=T),
			debt_mortgage_tot = mean(debt_mortgage_tot,na.rm=T),
			debt_frind_other_ins = mean(debt_frind_other_ins,na.rm=T),
			debt_tot = mean(debt_tot,na.rm=T),
			old = mean(old,na.rm=T),
			children = mean(children,na.rm=T),
			depen = mean(depen,na.rm=T),
			familysize = mean(familysize,na.rm=T),
			pid = first(pid),
			ethnicity = first(ethnicity),
			age = mean(age,na.rm=T),
			party = first(party),
			gender = first(gender),
			edu_highest = first(edu_highest),
			marriage = first(marriage),
			health = first(health),
			p_income = mean(p_income,na.rm=T),
			employ = first(employ),
			num_obs = mean(num_obs,na.rm=T)) 

family_head_all_restrict2_23 <- bind_rows(family_head_all_restrict2_20, family_head_all_restrict2_21) # bind together
family_head_all_restrict2_23 <- family_head_all_restrict2_23[order(family_head_all_restrict2_23$fid10),] # order
family_head_all_restrict2_23 <- family_head_all_restrict2_23 %>% group_by(fid10) %>% mutate(num_obs = n())
table(family_head_all_restrict2_23$num_obs)
	# 1     2     3 
	# 1   200 33390 

## Append together
family_head_all_restrict_final <- family_head_all_restrict2_23 %>% 
	filter(num_obs ==3) %>%
	group_by(fid10) %>%
	dplyr::mutate(num_obs = n()) %>% ungroup %>%
	group_by(fid10,year) %>%
	dplyr::mutate(num_obs_year = n()) %>% ungroup

table(family_head_all_restrict_final$num_obs)
table(family_head_all_restrict_final$num_obs_year)

family_head_all_restrict_final$num_obs <- NULL
family_head_all_restrict_final$num_obs_year <- NULL

################################################
# Merge in and replace ethnicity data
################################################

ethnicity_2010 <- read.dta13("ethnicity_2010.dta")

# Match ethnicity (first by pid, then fid10)
family_head_all_restrict_final$ethnicity1 <- ethnicity_2010[match(family_head_all_restrict_final$pid, ethnicity_2010$pid),'ethnicity']
family_head_all_restrict_final$ethnicity2 <- ethnicity_2010[match(family_head_all_restrict_final$fid10, ethnicity_2010$fid10), 'ethnicity']
family_head_all_restrict_final$ethnicity <- ifelse(!is.na(family_head_all_restrict_final$ethnicity1), 
                                              family_head_all_restrict_final$ethnicity1,
                                              family_head_all_restrict_final$ethnicity2)
# remove intermediate ethnicity variables
family_head_all_restrict_final$ethnicity1 <- NULL
family_head_all_restrict_final$ethnicity2 <- NULL

# label missing
family_head_all_restrict_final$ethnicity[family_head_all_restrict_final$ethnicity < 0] <- NA
family_head_all_restrict_final$urban[family_head_all_restrict_final$urban < 0] <- NA


################################################
# Merge in MLS Data 
################################################

mls <- read.dta13("merge_all.dta")

## Urban
mls_urban <- mls %>% 
	select(provcd,province,starts_with("urban"))%>% 
	group_by(provcd,province) %>%
	summarise(urban_2010 = mean(urban_2010,na.rm = T),
			  urban_2012 = mean(urban_2012,na.rm = T),
			  urban_2014 = mean(urban_2014,na.rm = T)) 
	names(mls_urban) <- c("provcd","province","2010",'2012','2014')

 mls_urban_long <- mls_urban %>% 
    gather(`2010`, `2012`, `2014`, key = "year", value = "mls") %>%
    mutate(urban = 1)

## Rural 
mls_rural <- mls %>% 
	select(provcd,province,starts_with("rural"))%>% 
	group_by(provcd,province) %>%
	summarise(rural_2010 = mean(rural_2010,na.rm = T),
			  rural_2012 = mean(rural_2012,na.rm = T),
			  rural_2014 = mean(rural_2014,na.rm = T)) 
	names(mls_rural) <- c("provcd","province","2010",'2012','2014')

 mls_rural_long <- mls_rural %>% 
    gather(`2010`, `2012`, `2014`, key = "year", value = "mls") %>%
    mutate(urban = 0)

## Bind together

mls_all <- mls_urban_long %>% bind_rows(mls_rural_long) %>% filter(provcd!=-9&provcd!=46&provcd!=64&provcd!=65)
mls_all$year <- as.integer(as.character(mls_all$year))

family_head_all_restrict_final_1 <- family_head_all_restrict_final %>% left_join(mls_all, by=c('provcd','year','urban'))

################################################
# Other restrictions
################################################

family_head_all_restrict_final_1$employ[is.na(family_head_all_restrict_final_1$employ)] <- 0
family_head_all_restrict_final_1$party <- NULL
family_head_all_restrict_final_1$fid12 <- NULL
family_head_all_restrict_final_1$fid14 <- NULL
family_head_all_restrict_final_1$num_obs <- NULL
family_head_all_restrict_final_1$f_income <- ifelse(is.na(family_head_all_restrict_final_1$f_income),
											   family_head_all_restrict_final_1$p_income,
											   ifelse(family_head_all_restrict_final_1$f_income==0,
											   family_head_all_restrict_final_1$p_income,
											   family_head_all_restrict_final_1$f_income))

# family_head_all_restrict_final_1$expense <- ifelse(is.na(family_head_all_restrict_final_1$expense),
# 												family_head_all_restrict_final_1$f_income,
# 												family_head_all_restrict_final_1$expense)

family_head_all_restrict_final_1$house_price <- ifelse(family_head_all_restrict_final_1$house_price == 0,
												family_head_all_restrict_final_1$house_price_tot,
												family_head_all_restrict_final_1$house_price)


################################################
# Add Poverty Lines
################################################

family_head_all_restrict_final_1 <- family_head_all_restrict_final_1 %>%
	mutate( asset_liq = asset_cash_deposit+asset_financial, 
		    asset_tot = asset_cash_deposit+asset_financial+house_price, 
		    asset_net = asset_cash_deposit+asset_financial+house_price-debt_tot) %>%
	mutate( asset_liq_index = asset_liq/(mls*familysize*3),
		    asset_tot_index = asset_tot/(mls*familysize*3),
		    asset_net_index = asset_net/(mls*familysize*3)) %>%
	mutate( asset_liq_p = ifelse(asset_liq_index<1,1,0),
			asset_tot_p = ifelse(asset_tot_index<1,1,0),
			asset_net_p = ifelse(asset_net_index<1,1,0)) %>%
	mutate( income_index = f_income/(12*mls*familysize), 
			income_p = ifelse(income_index<1,1,0))

family_head_all_restrict_final_1 <- family_head_all_restrict_final_1 %>%
	mutate(income_p_asset_p = ifelse(income_p == 0,0,ifelse(asset_net_p ==0,0,1)), 
		   income_np_asset_p = ifelse(income_p == 1,0,ifelse(asset_net_p ==0,0,1)), 
		   income_p_asset_np = ifelse(income_p == 0,0,ifelse(asset_net_p ==1,0,1)), 
		   income_np_asset_np = ifelse(income_p == 1,0,ifelse(asset_net_p ==1,0,1)))

## See summary
ggplot(family_head_all_restrict_final_1, aes(y=asset_net_index, x=income_index)) +
  geom_point(shape = 1)+
  geom_smooth(method=lm) 

ggplot(family_head_all_restrict_final_1, aes(y=asset_liq_index, x=income_index)) +
  geom_point(shape = 1)+
  geom_smooth(method=lm) 

	summary(family_head_all_restrict_final_1$income_p_asset_p)
	summary(family_head_all_restrict_final_1$income_np_asset_p)
	summary(family_head_all_restrict_final_1$income_p_asset_np)
	summary(family_head_all_restrict_final_1$income_np_asset_np)


family_head_all_restrict_final_2 <- family_head_all_restrict_final_1 %>%
	filter(income_index <100, asset_liq_index<300)

ggplot(family_head_all_restrict_final_2, aes(y=asset_liq_index, x=income_index)) +
  geom_point(shape = 1)+
  geom_smooth(method=lm) 


length(unique(family_head_all_restrict_final_1$fid10)) 
sum(with(family_head_all_restrict_final_1, urban==1)) # 4947
nrow(filter(family_head_all_restrict_final_1,urban==0)) # 6176 - should be 6270
################################################
# Export
################################################
library(foreign)
setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
save(family_head_all_restrict_final_1, file = "family_head_all_restrict_final_1.RData")
write.csv(family_head_all_restrict_final_1, file = 'family_head_all_restrict_final_1.csv')
write.dta(family_head_all_restrict_final_1, file = 'family_head_all_restrict_final_1.dta')




