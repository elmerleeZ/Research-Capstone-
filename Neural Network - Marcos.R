#####################
# Prepare
#####################
install.packages('RSNNS')
install.packages('neuralnet')
install.packages('caret')
install.packages('devtools')

library(zoo)
library(RSNNS)
library(neuralnet)
library(nnet)
library(caret)
library(readstata13)

setwd("C:/Users/zyl220/Downloads/temp/poverty")

#Misclassification function
rm(misclassification.rate)
misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}
#RSNNS Plotting function
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

################################################
# ATTEMPT 4
################################################

###ATTEMPT 4, JUST REORGANIZING AT. 3, AND DOING IT FOR DIFFERENT
#SETS OF VARIABLES

##Loading dataset
data = read.dta13("family_head_all_restrict_final_2.dta")


##Select Variables
nnvars2 = c("category_mls","category_wb","fid10","year","urban", "gender", "edu1", "edu2","edu3","edu4", 
            "marr", "health1","health2", "health3", "employed","eth_han","house_ownership","f_income", 
            "expense", "asset_liq","house_price","debt_tot","children","depen","familysize","age","old")
datann2 = data[nnvars2]

#Removing NAs (takes out about 3k obs)
datann2 = na.omit(datann2)

#Encoding response variable as different dummies
datann2 = cbind(datann2[3:27],class.ind(as.factor(datann2$category_mls)),class.ind(as.factor(datann2$category_wb)))
names(datann2) = c(names(datann2)[1:25],"mls_inp_anp","mls_ip_ap","mls_inp_ap","mls_ip_anp","wb_inp_anp","wb_ip_ap","wb_inp_ap","wb_ip_anp")

#'effect coding' dummies 
datann2[, 3:15] = ifelse(datann2[3:15] == 0,-1,1)

#scaling continuous features
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
datann2[,16:25] = data.frame(lapply(datann2[,16:25], scl))


#Separating into train and test
train = subset(datann2, year<2013)
test = subset(datann2, year>2013)

##Defining inputs and outputs for the 4 models

#Model 1 - including income and assets, mls
inp_tr_1 = train[, 3:25]
out_tr_1 = train[, 26:29]
inp_te_1 = test[, 3:25]
out_te_1 = test[, 26:29]

#Model 2 - without income and assets, mls (note problem with col name of expense)
inp_tr_2 = cbind(train[, 3:15], train$expense, train[, 21:25])
out_tr_2 = train[, 26:29]
inp_te_2 = cbind(test[, 3:15], test$expense, test[, 21:25])
out_te_2 = test[, 26:29]

#Model 3 - with income and assets, wb
inp_tr_3 = train[, 3:25]
out_tr_3 = train[, 30:33]
inp_te_3 = test[, 3:25]
out_te_3 = test[, 30:33]

#Model 4 - without income and assets, wb
inp_tr_4 = cbind(train[, 3:15], train$expense, train[, 21:25])
out_tr_4 = train[, 30:33]
inp_te_4 = cbind(test[, 3:15], test$expense, test[, 21:25])
out_te_4 = test[, 30:33]

##Fitting model 1
mlp_1 = mlp(x = inp_tr_1, y = out_tr_1, size = 5, learnFuncParams = c(0.1),
            maxit = 1000, inputsTest = inp_te_1, targetsTest = out_te_1)

mlp_1a = mlp(x = inp_tr_1, y = out_tr_1, size = 5, learnFuncParams = c(0.1),
            maxit = 1000)
plotIterativeError(mlp_1a)
predictions_1a = predict(mlp_1a, inp_te_1)
CM_1a = RSNNS::confusionMatrix(as.matrix(out_te_1),predictions_1a)

##Plotting error of model 1- seems to have converged
plotIterativeError(mlp_1)

##Predicting model 1 
predictions_1 = predict(mlp_1, inp_te_1)
CM_1 = RSNNS::confusionMatrix(as.matrix(out_te_1),predictions_1)
misclassification.rate(CM_1)
#Model 1 has 11.6% misclassification rate

##Fitting model 2
mlp_2 =  mlp(x = inp_tr_2, y = out_tr_2, size = 5, learnFuncParams = c(0.1),
             maxit = 1000, inputsTest = inp_te_2, targetsTest = out_te_2)

##Plotting error of model 2- Lots of red lines, what do they mean?
plotIterativeError(mlp_2)

##Predicting model 2 
predictions_2 = predict(mlp_2, inp_te_2)
CM_2 = RSNNS::confusionMatrix(as.matrix(out_te_2),predictions_2)
misclassification.rate(CM_2)
#11.6%, exactly same as CM_1, is that a problem??

##Plotting model 2 
plot.nnet(mlp_2)

##Fitting model 3
mlp_3 = mlp(x = inp_tr_3, y = out_tr_3, size = 5, learnFuncParams = c(0.1),
            maxit = 1000, inputsTest = inp_te_3, targetsTest = out_te_3)

##Plotting error of model 3- Seems to have converged
plotIterativeError(mlp_3)

##Predicting model 3
predictions_3 = predict(mlp_3, inp_te_3)
CM_3 = RSNNS::confusionMatrix(as.matrix(out_te_3),predictions)
misclassification.rate(CM_3)
#Misclassified 10.6%, best so far

##Fitting model 4
mlp_4 = mlp(x = inp_tr_4, y = out_tr_4, size = 5, learnFuncParams = c(0.1),
            maxit = 1000, inputsTest = inp_te_4, targetsTest = out_te_4)

##Plotting error of model 4- same red lines as in model 2
plotIterativeError(mlp_4)

##Predicting model 4
predictions_4 = predict(mlp_4, inp_te_4)
CM_4 = RSNNS::confusionMatrix(as.matrix(out_te_4),predictions)
misclassification.rate(CM_4)
#Again, 10.6%, same predictions as model 3

##Plotting iterative errors for ppt
par(mfrow=c(1,2))
plotIterativeError(mlp_1, main = "I.E. with income and assets")
plotIterativeError(mlp_2, main = "I.E. without income and assets")


################################################
# ATTEMPT 3
################################################

###ATTEMPT 3 - Revisiting attempt 1 with improvements from attempt 2

#Defining inputs and outputs
inputstr = train2[, 3:21]
outputstr = train2[, 22:25]
inputste = test2[, 3:21]
outputste = test2[, 22:25]

#Fitting model

fit2mlp = mlp(x = inputstr, y = outputstr, size = 5, learnFuncParams = c(0.1),
              maxit = 1000, inputsTest = inputste, targetsTest = outputste)
summary(fit2mlp)

#Seems to have converged:
plotIterativeError(fit2mlp)

#Predicting
predictions = predict(fit2mlp, inputste)
plotRegressionError(predictions[,1], outputste[,1])
confusionMatrix(outputstr, fitted.values(fit2mlp))
CF = confusionMatrix(outputste2,predictions)
misclassification.rate(CF)
#This one gets only 15% misclassification

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
par(mfrow=c(1,1))
plot.nnet(fit2mlp)

###ATTEMPT 2 - Following class and online examples

##Loading newest dataset (different from one used above)
data = read.dta13("family_head_all_restrict_final_2.dta")

#Will take out unordered categorical variables until figure out what to do 
#"provcd", "countyid", "depen". I should add ethhan in dummies

nnvars2 = c("category_mls","fid10","year","urban", "gender", "edu1", "edu2","edu3","edu4", 
            "marr", "health1","health2", "health3", "employed","eth_han","f_income", 
            "expense", "asset_tot","debt_tot","children", "age","old")
datann2 = data[nnvars2]

#Removing NAs (takes out about 3k obs)
datann2 = na.omit(datann2)

#Encoding response variable
datann2 = cbind(datann2[2:22],class.ind(as.factor(datann2$category_mls)))
names(datann2) = c(names(datann2)[1:21],"inp_anp","ip_ap","inp_ap","ip_anp")

#'effect coding' dummies (should I do the same with response?)
datann2[, 3:14] = ifelse(datann2[3:14] == 0,-1,1)

#scaling continuous features
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
datann2[,15:21] = data.frame(lapply(datann2[,15:21], scl))


#Separating into train and test
train2 = subset(datann2, year<2013)
test2 = subset(datann2, year>2013)

##We have 18 inputs so I will use 12 neurons. The architecture will be 18:8:4:4

#Writing formula
n= names(datann2)
f = as.formula(paste("inp_anp+ip_ap+inp_ap+ip_anp ~", paste(n[!n %in% c("inp_anp","ip_ap","inp_ap","ip_anp","fid10","year")], collapse = "+")))
#Doesn't converge, but it does with year and fid10
f2 = as.formula(paste("inp_anp+ip_ap+inp_ap+ip_anp ~", paste(n[!n %in% c("inp_anp","ip_ap","inp_ap","ip_anp")], collapse = "+")))

#Fitting model
nn = neuralnet(f2,data=train2,hidden=c(8,4),act.fct = "logistic",linear.output=F, lifesign = "minimal")
plot(nn)

#Predictions
pr.nn = compute(nn, test2[,1:21])
pr.nn_ = pr.nn$net.result

#Accuracy
original_values = max.col(test2[, 22:25])
pr.nn_2 = max.col(pr.nn_)
mean(pr.nn_2 == original_values)
##Has 70% Accuracy in predicting third year


###ATTEMPT 1, FROM MLMEWR

load("family_head_all_restrict_final_1.RData")
data = family_head_all_restrict_final_1
attach(data)

#Creating one categorical variable (note as.factor so that class.ind works)
data$category = ifelse(data$income_p_asset_p==1,0,ifelse(data$income_np_asset_p==1,1, ifelse(data$income_p_asset_np==1,2,ifelse(data$income_np_asset_np==1,3,0))))

##Creating dataset with variables that we need, without NA
#Same variables as in QDA except for marriage
nnvars = c("fid10", "year", "category", "provcd", "countyid", "urban", "f_income", "expense", "asset_tot", 
           "debt_tot", "old", "children", "depen", "age", "gender", "edu_highest","marriage",
           "health", "employ")
datann = data[nnvars]
apply(datann,2,function(x) sum(is.na(x)))
datann = na.omit(datann)
#About 10% of observations are removed due to NA. Later: multiple imputation?

#Dividing data into training and test
train = subset(datann, year<2013)
test = subset(datann, year>2013)

#Scaling data
sctrain = scale(train)
sctest = scale(test)

#Setting inputs and outputs
set.seed(314)
inputs = train[,4:19]
output = train[,3]

#Fitting mlp
fitmlp = mlp(x=inputs,y=output, size = c(12,8), maxit = 1000, initFunc = "Randomize_Weights",
             initFuncParams = c(-0.3,0.3), learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2,0),
             updateFunc = "Topological_Order", updateFuncParams = c(0), hiddenActFunc = "Act_Logistic",
             shufflePatterns = TRUE, linOut = TRUE)

#Predicting
predmlp = sign(predict(fitmlp, sctest[,4:19]))
table(predmlp, sign(sctest[,3]), dnn = c("pred", "obs"))

error = (1-sum(predmlp == sign(sctest[,3]))/11014)
#Predicted with 70% accuracy?



