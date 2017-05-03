#####################
# Prepare
#####################

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
setwd("C:/Users/zyl220/Downloads/temp")

library(readr)
library(tidyr)
library(dplyr)
library(pryr)
library(tidyverse)
library(stargazer)
library(readstata13)

install.packages('mlogit')
library(mlogit)

family_head_all_restrict_final_2 <- read.dta13("family_head_all_restrict_final_2.dta")


################################################
# Mobility MLS
################################################

mobility_mls <- family_head_all_restrict_final_2 %>% 
	select(fid10,category_mls,year) %>% 
	group_by(fid10) %>% 
	spread(year,category_mls) 

names(mobility_mls) <- c('fid10','yr2010','yr2012','yr2014')

mobility_mls_1012 <- mobility_mls%>% 
	group_by(yr2010) %>%
	dplyr::summarise(Stable = sum(ifelse(yr2012==0,1,0),na.rm = T),
			Poor = sum(ifelse(yr2012==1,1,0),na.rm = T),
			Transient_Stable = sum(ifelse(yr2012==2,1,0),na.rm = T),
			Transient_Poor = sum(ifelse(yr2012==3,1,0),na.rm = T)
		) %>%ungroup()

mobility_mls_1214 <- mobility_mls%>% 
	group_by(yr2012) %>%
	dplyr::summarise(Stable = sum(ifelse(yr2014==0,1,0),na.rm = T),
			Poor = sum(ifelse(yr2014==1,1,0),na.rm = T),
			Transient_Stable = sum(ifelse(yr2014==2,1,0),na.rm = T),
			Transient_Poor = sum(ifelse(yr2014==3,1,0),na.rm = T)
		) %>%ungroup()


################################################
# Mobility WB
################################################

mobility_wb <- family_head_all_restrict_final_2 %>% 
	select(fid10,category_wb,year) %>% 
	group_by(fid10) %>% 
	spread(year,category_wb) 

names(mobility_wb) <- c('fid10','yr2010','yr2012','yr2014')

mobility_wb_1012 <- mobility_wb%>% 
	group_by(yr2010) %>%
	dplyr::summarise(Stable = sum(ifelse(yr2012==0,1,0),na.rm = T),
			Poor = sum(ifelse(yr2012==1,1,0),na.rm = T),
			Transient_Stable = sum(ifelse(yr2012==2,1,0),na.rm = T),
			Transient_Poor = sum(ifelse(yr2012==3,1,0),na.rm = T)
		) %>%ungroup()

mobility_wb_1214 <- mobility_wb%>% 
	group_by(yr2012) %>%
	dplyr::summarise(Stable = sum(ifelse(yr2014==0,1,0),na.rm = T),
			Poor = sum(ifelse(yr2014==1,1,0),na.rm = T),
			Transient_Stable = sum(ifelse(yr2014==2,1,0),na.rm = T),
			Transient_Poor = sum(ifelse(yr2014==3,1,0),na.rm = T)
		) %>%ungroup()


################################################
# EXPORT
################################################

write.csv(mobility_mls_1012,file='mobility_mls_1012.csv')
write.csv(mobility_mls_1214,file='mobility_mls_1214.csv')
write.csv(mobility_wb_1012,file='mobility_wb_1012.csv')
write.csv(mobility_wb_1214,file='mobility_wb_1214.csv')

data("Train", package = "mlogit")
Tr <- mlogit.data(Train, shape = "wide", varying = 4:11, choice = "choice",
   sep = "", 
   alt.levels = c("choice1", "choice2"), id = "id")

Tr <- Tr %>% filter(choice == T)
Tr$choice <- Train$choice

Train.ml <- mlogit(choice ~ price + time + change + comfort,Tr)

Train.mxlc <- mlogit(choice ~ price + time + change + comfort,
   Tr, panel = TRUE, rpar = c(time = "cn", change = "n", comfort = "ln"),
   correlation = TRUE, R = 100, halton = NA)
Train.mxlu <- update(Train.mxlc, correlation = FALSE)














