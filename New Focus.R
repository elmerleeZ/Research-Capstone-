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


########################################################################
## Inspection  ##

ggplot(data = family_head_all_restrict_final_2, aes(x=f_income, y=health)) +
	geom_point(shape = 19, size = 1)   


