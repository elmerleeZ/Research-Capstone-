#############################################################################
# Program Name:   New Focus.R
# Location:       /Users/zongyangli/Documents/Github/poverty-measure-prediction/New Focus.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################

source("/Users/zongyangli/Documents/Github/R-Key-functions/Start up.R")
library(readstata13)


setwd("/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone 2016-2017/Data/Raw - CFPS")

family_head_all_restrict_final_2 <- read.dta13("family_head_all_restrict_final_2.dta") 
family_head_all_restrict_final_1 <- read.dta13("family_head_all_restrict_final_1.dta") 


########################################################################
## Inspection  ##

ggplot(data = family_head_all_restrict_final_2, aes(x=asset_tot)) +
	geom_density()


family_head_all_restrict_final_2 <- family_head_all_restrict_final_2 %>%
	# filter(num_obs ==3) %>%
	group_by(fid10) %>%
	dplyr::mutate(num_obs = n()) %>% ungroup %>%
	group_by(fid10,year) %>%
	dplyr::mutate(num_obs_year = n()) %>% ungroup

family_head_all_restrict_final_1 <- family_head_all_restrict_final_1 %>%
	# filter(num_obs ==3) %>%
	group_by(fid10) %>%
	dplyr::mutate(num_obs = n()) %>% ungroup %>%
	group_by(fid10,year) %>%
	dplyr::mutate(num_obs_year = n()) %>% ungroup


table(family_head_all_restrict_final_1$num_obs)
#    1    2    3 
# 5078 4374 1464 
table(family_head_all_restrict_final_1$num_obs_year)


table(family_head_all_restrict_final_2$num_obs)
table(family_head_all_restrict_final_2$num_obs_year)



########################################################################
## See Distribution  ##

useful_var <- family_head_all_restrict_final_2 %>% select(f_income,asset_tot,debt_tot)
psych::describe(useful_var)


###############################################################################
## Construct SGP Measure
###############################################################################

## New Version 
library(Hmisc)
income_gp_all <- family_head_all_restrict_final_2 %>%
  select(fid10,year,provcd,f_income) %>%
  mutate(provcd = as.integer(provcd),
         fid10 = as.character(fid10)) %>%
  group_by(fid10) %>%
  mutate(f_income_prev = Lag(f_income,+1),
  		 provcd_prev = Lag(provcd,+1))

# sort by how percentiles will be calculated (note: not necessary for calculation)
income_gp_all <- income_gp_all[order(
        income_gp_all$year,
        income_gp_all$f_income_prev,
        income_gp_all$f_income),]

# calculate RANK within each subject/year/grade/score_prev group
income_gp_all$rank <- ave(income_gp_all$f_income,
        income_gp_all$year,
        income_gp_all$f_income_prev,
        FUN = function(x) rank(x, ties.method = "average"))

# calculate COUNT within each subject/year/grade/score_prev group
income_gp_all$count <- ave(income_gp_all$f_income,
        income_gp_all$year, 
        income_gp_all$f_income_prev,
        FUN = length) + 1   # note: + 1 here ensures SGP of small groups is not biased up

# calculate SGP as RANK/COUNT
income_gp_all$income_gp = income_gp_all$rank / income_gp_all$count * 100


## See the correlation with income measures
cor(income_gp_all$income_gp,income_gp_all$f_income)


## Merge Back
data_all_year <- data_all_year %>%
  left_join(income_gp_all[,c("fid10", "year","provcd","income_gp")], by = c("fid10","year","provcd"))









