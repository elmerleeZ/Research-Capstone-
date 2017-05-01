################################################
# Prepare
################################################

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
library(readstata13)

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
setwd("C:/Users/zyl220/Downloads/temp/poverty")

family_head_all_restrict_final_2 <- read.dta13("family_head_all_restrict_final_2.dta") %>%
  mutate(expense = expense/1000,f_income=f_income/1000)


##########################
# Test Endogeneity 
##########################

family_head_all_restrict_final_2 <- family_head_all_restrict_final_2 %>% group_by(fid10) %>% grangertest(income_index_mls~expense, order=1)

library(lmtest)
grangertest(income_index_mls~expense, order=0,data = family_head_all_restrict_final_2)
grangertest(expense~income_index_mls, order=1,data = family_head_all_restrict_final_2)

######################################################################
# Organize Data
######################################################################
#Creating one categorical variable
    # family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 15] <- 14
    # family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 46] <- 45
    # family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 64] <- 62
    # family_head_all_restrict_final_1$provcd[family_head_all_restrict_final_1$provcd == 65] <- 62

    # family_head_all_restrict_final_1 <- family_head_all_restrict_final_1 %>%
    #   mutate(category = ifelse(income_p_asset_p==1,0,ifelse(income_np_asset_p==1,1, ifelse(income_p_asset_np==1,2,ifelse(income_np_asset_np==1,3,0)))),
    #          provcd = as.factor(provcd), 
    #          countyid = as.factor(countyid), 
    #          edu_highest = as.factor(edu_highest), 
    #          marriage = as.factor(marriage),
    #          health = as.factor(health)) %>%
    #   mutate(eth_han = ifelse(ethnicity==1,1,0))

#Creating function to calculate missclassification rate
rm(misclassification.rate)
misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}

##Using 2010 and 2012 as train, to test on 2014
train = subset(family_head_all_restrict_final_2, year<2013)
test = subset(family_head_all_restrict_final_2, year>2013)

######################################################################
# LDA Model 
######################################################################


##########################
# Poverty MLS
##########################


lda_mls_1 = lda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_mls_2 = lda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      +Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_mls_3 = lda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

lda_mls_4 = lda(category_mls~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_mls_5 = lda(category_mls~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

#Predicting the test data
pred_lda_mls_1 = predict(lda_mls_1, newdata=test)$class
pred_lda_mls_2 = predict(lda_mls_2, newdata=test)$class
pred_lda_mls_3 = predict(lda_mls_3, newdata=test)$class
pred_lda_mls_4 = predict(lda_mls_4, newdata=test)$class
pred_lda_mls_5 = predict(lda_mls_5, newdata=test)$class

#Comparing predictions vs. real
tab1 = table(pred_lda_mls_1, test$category_mls)
tab2 = table(pred_lda_mls_2, test$category_mls)
tab3 = table(pred_lda_mls_3, test$category_mls)
tab4 = table(pred_lda_mls_4, test$category_mls)
tab5 = table(pred_lda_mls_5, test$category_mls)

    #did lda predict no people in income_p_asset_p or am I reading the table wrong? 

#Calculate misclassification rate
misclassification.rate(tab1)
misclassification.rate(tab2)
misclassification.rate(tab3)
misclassification.rate(tab4)
misclassification.rate(tab5)

lda_mls_1
lda_mls_2
lda_mls_3
lda_mls_4
lda_mls_5

##########################
# Try to test significance
##########################
    # mydata.manova <- manova(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
    #       + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed,  data=train)
    # summary(mydata.manova, test="Wilks")

    # mydata.lda.predict <- predict(lda_mls_3,data=train)
    # #join predictions to original dataframe
    # mydata2 <- cbind(train, mydata.lda.predict)

    # test1 <- aov(mydata2$x.LD1 ~d$category_mls)
    # summary(test1)

    # test2 <- aov(mydata2$x.LD2 ~d$category_mls)
    # summary(test2)

    # test3 <- aov(mydata2$x.LD3 ~d$category_mls)
    # summary(test3)


##########################
# Poverty WB
##########################

lda_wb_1 = lda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_wb_2 = lda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      +Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_wb_3 = lda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

lda_wb_4 = lda(category_wb~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Tianjin+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

lda_wb_5 = lda(category_wb~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

#Predicting the test data
pred_lda_wb_1 = predict(lda_wb_1, newdata=test)$class
pred_lda_wb_2 = predict(lda_wb_2, newdata=test)$class
pred_lda_wb_3 = predict(lda_wb_3, newdata=test)$class
pred_lda_wb_4 = predict(lda_wb_4, newdata=test)$class
pred_lda_wb_5 = predict(lda_wb_5, newdata=test)$class

#Comparing predictions vs. real
tab1 = table(pred_lda_wb_1, test$category_wb)
tab2 = table(pred_lda_wb_2, test$category_wb)
tab3 = table(pred_lda_wb_3, test$category_wb)
tab4 = table(pred_lda_wb_3, test$category_wb)
tab5 = table(pred_lda_wb_3, test$category_wb)
tab1
tab2
tab3
tab4
tab5
    #did qda predict no people in income_p_asset_p or am I reading the table wrong? 

#Calculate misclassification rate
misclassification.rate(tab1)
misclassification.rate(tab2)
misclassification.rate(tab3)
misclassification.rate(tab4)
misclassification.rate(tab5)

lda_wb_1
lda_wb_2
lda_wb_3
lda_wb_4
lda_wb_5


######################################################################
# QDA Model 
######################################################################


##########################
# Poverty MLS
##########################

qda_mls_1 = qda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_mls_2 = qda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      +Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_mls_3 = qda(category_mls~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train) # 0.215, the best

qda_mls_4 = qda(category_mls~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_mls_5 = qda(category_wb~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

#Predicting the test data
pred_qda_mls_1 = predict(qda_mls_1, newdata=test)$class
pred_qda_mls_2 = predict(qda_mls_2, newdata=test)$class
pred_qda_mls_3 = predict(qda_mls_3, newdata=test)$class
pred_qda_mls_4 = predict(qda_mls_4, newdata=test)$class
pred_qda_mls_5 = predict(qda_mls_4, newdata=test)$class

#Comparing predictions vs. real
tab1 = table(pred_qda_mls_1, test$category_mls)
tab2 = table(pred_qda_mls_2, test$category_mls)
tab3 = table(pred_qda_mls_3, test$category_mls)
tab4 = table(pred_qda_mls_4, test$category_mls)
tab5 = table(pred_qda_mls_4, test$category_mls)
tab1
tab2
tab3
tab4
tab5
    #did qda predict no people in income_p_asset_p or am I reading the table wrong? 

#Calculate misclassification rate
misclassification.rate(tab1)
misclassification.rate(tab2)
misclassification.rate(tab3)
misclassification.rate(tab4)
misclassification.rate(tab5)

qda_mls_1
qda_mls_2
qda_mls_3
qda_mls_4
qda_mls_5

##########################
# Poverty WB
##########################

qda_wb_1 = qda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_wb_2 = qda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      +Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_wb_3 = qda(category_wb~urban+f_income+expense+asset_liq+house_price+debt_tot+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

qda_wb_4 = qda(category_wb~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      + year12+Hebei+Shanxi+Liaoning+Jilin+Heilongjiang+Shanghai+Jiangsu+Zhejiang+Anhui+Fujian+Jiangxi+Shandong+Henan+Hubei+Hunan+Guangdong+Guangxi+Chongqing+Sichuan+Guizhou+Yunnan+Shannxi+Gansu
      , data = train)

qda_wb_5 = qda(category_wb~urban+expense+house_ownership+old+children+depen+familysize
      + eth_han+age+age2+gender+edu2+edu3+edu4+marr+health1+employed
      , data = train)

#Predicting the test data
pred_qda_wb_1 = predict(qda_wb_1, newdata=test)$class
pred_qda_wb_2 = predict(qda_wb_2, newdata=test)$class
pred_qda_wb_3 = predict(qda_wb_3, newdata=test)$class
pred_qda_wb_4 = predict(qda_wb_4, newdata=test)$class
pred_qda_wb_5 = predict(qda_wb_5, newdata=test)$class

#Comparing predictions vs. real
tab1 = table(pred_qda_wb_1, test$category_wb)
tab2 = table(pred_qda_wb_2, test$category_wb)
tab3 = table(pred_qda_wb_3, test$category_wb)
tab4 = table(pred_qda_wb_3, test$category_wb)
tab5 = table(pred_qda_wb_3, test$category_wb)
tab1
tab2
tab3
tab4
tab5
    #did qda predict no people in income_p_asset_p or am I reading the table wrong? 

#Calculate misclassification rate
misclassification.rate(tab1)
misclassification.rate(tab2)
misclassification.rate(tab3)
misclassification.rate(tab4)
misclassification.rate(tab5)

qda_wb_1
qda_wb_2
qda_wb_3
qda_wb_4
qda_wb_5



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




