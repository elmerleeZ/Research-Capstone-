#####################
# Prepare
#####################

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")

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
#
################################################

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














