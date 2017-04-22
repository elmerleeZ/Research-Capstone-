################################################
# Prepare
################################################

# setwd("~/Dropbox/NYU/Spring/Econometrics/Project/Data")
setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
load("family_head_all_restrict_final_1.RData")

# set up useful functions 
`%S%` <- function(x, y) {
  paste0(x, y)
}
`%notin%` <- Negate(`%in%`)

library(readr)
library(tidyr)
library(dplyr)
library(pryr)
 # library(plyr)
library(stringr)
library(tidyverse)
library(stargazer)
library(MASS)
library(caret)

######################################################################
# Organize Data
######################################################################

#Creating one categorical variable
family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 15] <- 14
family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 46] <- 45
family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 64] <- 62
family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 65] <- 62

family_head_all_restrict_final_1 <- family_head_all_restrict_final_1 %>%
  mutate(category = ifelse(income_p_asset_p==1,0,ifelse(income_np_asset_p==1,1, ifelse(income_p_asset_np==1,2,ifelse(income_np_asset_np==1,3,0)))),
         provcd = as.factor(provcd), 
         countyid = as.factor(countyid), 
         edu_highest = as.factor(edu_highest), 
         marriage = as.factor(marriage),
         health = as.factor(health)) %>%
  mutate(eth_han = ifelse(ethnicity==1,1,0))

#Creating function to calculate missclassification rate
rm(misclassification.rate)
misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}

##Using 2010 and 2012 as train, to test on 2014
train = subset(family_head_all_restrict_final_1, year<2013)
test = subset(family_head_all_restrict_final_1, year>2013)

######################################################################
# LDA Model 
######################################################################

lda1 = lda(category ~  urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize 
  + eth_han + age + gender + edu_highest + marriage + health + p_income + employ
  ,data = train)

#Predicting the test data
pred_lda1 = predict(lda1, newdata=test)$class

#Comparing predictions vs. real
tab = table(pred_lda1, test$category)
tab
    #did LDA predict no people in income_p_asset_p or am I reading the table wrong? 

#Calculate misclassification rate
misclassification.rate(tab)
    #LDA misclassified 28.4% of observations


######################################################################
# QDA Model 
######################################################################

##QDA model
qda1 = qda(category ~ urban + f_income + expense + asset_cash_deposit + house_ownership + house_price + house_price_tot + debt_tot + old + children + depen + familysize 
  + ethnicity + age + gender + edu_highest + marriage + health + p_income + employ
  ,data = train)

lda1 = lda(category ~  urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize 
+ eth_han + age + gender + edu_highest + marriage + health + p_income + employ
,data = train)

qda1 = qda(category ~ urban + f_income + expense + asset_cash_deposit + house_ownership + house_price + house_price_tot + debt_tot + old + children + depen + familysize 
+ eth_han + age + gender + p_income + marriage + health + employ
,data = train)

qda1 = qda(category~provcd+countyid+urban+f_income+expense+asset_tot+debt_tot+old+children+depen+age+gender+edu_highest+marriage+health+employ, data = train)

#Predicting the test data
pred_qda1 = predict(qda1, newdata=test)$class

#Comparing predictions vs. real
tab2 = table(pred_qda1, test$category)

#Calculating misclassification rate
misclassification.rate(tab2)
#QDA misclassified 21.4% of observations, much better than LDA

##KNN won't run with NA obs, so we have to decide what we're doing with 
#the ones that have too many NAs, like expense, before we run this model to compare
#with LDA and QDA
model <- train(
  category~provcd+countyid+urban+f_income+expense+asset_tot+debt_tot+old+children+depen+age+gender+edu_highest+marriage+health+employ, 
  data=data, 
  method='knn',
  tuneGrid=expand.grid(.k=1:25),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))
model
plot(model)


################################################
# Back up useful 
################################################

fid10 +
factor(provcd) +
factor(countyid) +
urban +
f_income +
expense +
asset_cash_deposit +
asset_financial +
house_ownership +
house_price +
house_price_tot +
debt_mortgage_tot +
debt_frind_other_ins +
debt_tot +
old +
children +
depen +
familysize +
# pid +
factor(ethnicity) +
age +
party +
gender +
factor(edu_highest) +
marriage +
health +
p_income +
factor(employ) 

factor(provcd) + factor(countyid) + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize + factor(ethnicity) + age + party + gender + factor(edu_highest) + marriage + health + p_income + factor(employ)

### Dealing with categorical variables

attach(train)
train1 <- model.matrix(category ~ factor(provcd)-1 + factor(countyid)-1 + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize 
  + factor(ethnicity)-1 + age + gender + factor(edu_highest)-1 + marriage + health + p_income + factor(employ)-1)
train1$X.Intercept. <- NULL
train1 <- model.matrix(category ~ provcd + urban + f_income + expense + asset_cash_deposit + asset_financial + house_ownership + house_price + house_price_tot + debt_mortgage_tot + debt_frind_other_ins + debt_tot + old + children + depen + familysize 
  + eth_han + age + gender + edu_highest + marriage + health + p_income + employ, train1)
train1 <- data.frame(train1)
train1 <- na.pass(train1)
train1 <- train1[complete.cases(train1),]
# train$provcd <- as.factor(train$provcd)
# train_new <- dummy.data.frame(train, names = c("provid") ,sep = ".")
#LDA model
lda1 = lda(category ~   f_income + expense , data = train1, na.action = "na.pass")
sapply(train1, function(x) sum(is.na(x)))




