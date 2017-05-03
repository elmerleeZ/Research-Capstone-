################################################
# Prepare
################################################

# install.packages("RSNNS")
# library("RSNNS")
# ?jordan
# ?elman

install.packages('neuralnet')
install.packages('caret')
library(zoo)
library(RSNNS)
library(neuralnet)
library(nnet)
library(caret)
library(readstata13)

#Creating function to calculate missclassification rate
rm(misclassification.rate)
misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}

setwd("/Users/elmerleezy/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Data/Raw - CFPS")
data = read.dta13("family_head_all_restrict_final_2.dta")

  # load("family_head_all_restrict_final_1.RData")
  # data = family_head_all_restrict_final_1
attach(data)

################################################
# ATTEMPT 1, FROM MLMEWR
################################################

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


################################################
# ATTEMPT 2 - Following class and online examples
################################################

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
datann2 = cbind(datann2[2:22],class.ind(as.factor(datann2$category_mls))) # turn category_mls into 4 factor columns
names(datann2) = c(names(datann2)[1:21],"Stable","Poor","Transient_Stable","Transient_Poor")

#'effect coding' dummies (should I do the same with response?) into 1 and -1
datann2[3:14] = ifelse(datann2[3:14] == 0,-1,1)
datann2[22:25] = ifelse(datann2[22:25] == 0,-1,1)
#scaling continuous features
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
datann2[,15:21] = data.frame(lapply(datann2[,15:21], scl))


#Separating into train and test
train2 = subset(datann2, year<2013)
test2 = subset(datann2, year>2013)

##We have 18 inputs so I will use 12 neurons. The architecture will be 18:8:4:4

#Writing formula
n= names(datann2)
f = as.formula(paste("Stable+Poor+Transient_Stable+Transient_Poor ~", paste(n[!n %in% c("Stable","Poor","Transient_Stable","Transient_Poor","fid10","year")], collapse = "+")))
#Doesn't converge, but it does with year and fid10
f2 = as.formula(paste("Stable+Poor+Transient_Stable+Transient_Poor ~", paste(n[!n %in% c("Stable","Poor","Transient_Stable","Transient_Poor")], collapse = "+")))

#Fitting model
nn = neuralnet(f2,data=train2,hidden=c(8,4),learningrate = 0.001, act.fct = "logistic",linear.output=F, lifesign = "minimal")
  # 8-4 hidden units
plot(nn)

#Predictions
pr.nn = compute(nn, test2[,1:21])
pr.nn_ = pr.nn$net.result

#Accuracy
original_values = max.col(test2[, 22:25])
pr.nn_2 = max.col(pr.nn_)
mean(pr.nn_2 == original_values)
##Has 70% Accuracy in predicting third year


################################################
# ATTEMPT 3.
################################################


###ATTEMPT 3 - Revisiting attempt 1 with improvements from attempt 2

#Defining inputs and outputs
inputstr = train2[, 3:21]
outputstr = train2[, 22:25]
inputste = test2[, 3:21]
outputste = test2[, 22:25]


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










######################################################################
# Back Up - Previous Examples
# Example 1
##################################################


    # data(snnsData)
    # inputs <- snnsData$eight_016.pat[,inputColumns(snnsData$eight_016.pat)]
    # outputs <- snnsData$eight_016.pat[,outputColumns(snnsData$eight_016.pat)]

    # par(mfrow=c(1,2))

    # modelElman <- elman(inputs, outputs, size=8, learnFuncParams=c(0.1), maxit=1000)
    # modelElman
    # modelJordan <- jordan(inputs, outputs, size=8, learnFuncParams=c(0.1), maxit=1000)
    # modelJordan

    # plotIterativeError(modelElman)
    # plotIterativeError(modelJordan)

    # summary(modelElman)
    # summary(modelJordan)


##################################################
# Example 2
##################################################


    # laser <- snnsData$laser_1000.pat
    # inputs <- laser[, inputColumns(laser)]
    # targets <- laser[, outputColumns(laser)]
    # patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)

    # model <- elman(patterns$inputsTrain, patterns$targetsTrain,
    #   size = c(8, 8), learnFuncParams = c(0.1), maxit = 500,
    #   inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest,
    #   linOut = FALSE)


    # plot(inputs, type = "l")
    # plot(targets[1:100], type = "l")
    # lines(model$fitted.values[1:100], col = "green")

    # plotIterativeError(model)
    # plotRegressionError(patterns$targetsTrain, model$fitted.values)
    # plotRegressionError(patterns$targetsTest, model$fittedTestValues)
    # hist(model$fitted.values - patterns$targetsTrain)


