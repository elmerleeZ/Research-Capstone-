#####################
# Prepare
#####################

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
load("family_head_all_restrict_final_1.RData")

install.packages("dynpanel")
library(readr)
library(tidyr)
library(dplyr)
library(pryr)
# library(plyr)
library(tidyverse)

library(dynpanel)
library(plm)
library(stargazer)

##########################
## First check data
##########################

data1 <- family_head_all_restrict_final_1 %>%
	group_by(fid10,year) %>%
    dplyr::mutate(num_obs = n()) 

    table(data1$num_obs)
data1 <- family_head_all_restrict_final_1 %>% purrr::map(as.list) 

## Delete missings
sapply(family_head_all_restrict_final_1, function(x) sum(is.na(x))) # check missings
data1=na.omit(family_head_all_restrict_final_1)


################################################
# A-B with dynpanel
################################################

# Run model
income_p_dpd <- dpd(income_p~factor(provcd) + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize
			     + factor(ethnicity) + age + gender + factor(edu_highest) + factor(marriage) + factor(health) + p_income + employ, 
					data=data1, index=c("fid10", "year"),1,0)

income_p_dpd <- dpd(income_p ~ provcd + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize, 
					data=data1, index=c("fid10", "year"),1,4)

	## returning error:
		# Error in match.names(clabs, names(xi)) : names do not match previous names

identical(names(data1[[1]]), names(data1[[2]]))

##### Example
		# Load data
		data(Produc)
		# Fit the dynamic panel data using the Arellano Bond (1991) instruments
		reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,4)
		summary(reg)




################################################
# A-B with pgmm
################################################
income_p_plm_ab = plm(income_p~provcd + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize
			     + factor(ethnicity) + age + gender + factor(edu_highest) + factor(marriage) + factor(health) + p_income + employ,index = c("fid10","year"), model = "within", na.rm=TRUE, data=family_head_all_restrict_final_1)

income_pgmm = pgmm(dynformula(f_income ~  expense + asset_tot + debt_tot + old + children + depen + familysize, lag = list(1,0,1,0,0,0,0,0), log = TRUE), data = data1, effect = "individual", model = "twosteps", gmm.inst = ~(f_income), lag.gmm = list(c(2,99)))

income_pgmm = pgmm(dynformula(f_income ~  expense + asset_tot + debt_tot, lag = list(1,0,1,0)), 
              data = data1, effect = "individual", model = "twosteps", 
              gmm.inst = ~f_income, lag.gmm = list(c(2,99)))
              # after adding too many instrument: system is computationally singular: reciprocal condition number = 1.83269e-27

stargazer(income_pgmm, type="text")
summary(income_pgmm)
  # error: Error in t(y) %*% x : non-conformable arguments















################################################
# Example:
################################################
### http://stats.stackexchange.com/questions/111643/fit-measures-for-gmm-arellano-bond-estimator-in-r

data("EmplUK", package = "plm")
EmplUK_raw <- EmplUK
EmplUK <- EmplUK_raw[1:500,]
## Arellano and Bond (1991), table 4b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")

lag(log(EmplUK$output), 0:1) | lag(log(EmplUK$emp), 2:99)

# Shows Sargan test and serial correlation of residuals.
summary(z1)

# Getting the actual Y values out. There is likely a more elegant way to do this.
Y <- c()
for( i in 1:length(z1$model)){ Y <- c(Y,z1$model[[i]][,1])}

# Note that the fitted-values are exactly what you need.
Yhat <- z1$fitted.values[1:length(z1$fitted.values)]

# Squared correlation of fitted values and actual values
cor(Y,Yhat)^2



#### Example:

data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4 col. b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1)

## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 2:99) + lag(log(capital), 2:99),        
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

## Not run: 
## Same with the old formula or dynformula interface
## Arellano and Bond (1991), table 4, col. b 
z1 <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
            lag.form = list(2,1,0,1), data = EmplUK, 
            effect = "twoways", model = "twosteps",
            gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
summary(z1)

## Blundell and Bond (1998) table 4 (cf DPD for OX p. 12 col. 4)
z2 <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital), list(1,1,1)), 
            data = EmplUK, effect = "twoways", model = "onestep", 
            gmm.inst = ~log(emp) + log(wage) + log(capital), 
            lag.gmm = c(2,99), transformation = "ld")
summary(z2, robust = TRUE)

## End(Not run)
