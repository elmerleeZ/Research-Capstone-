setwd('/Users/zongyangli/Google Drive/Wagner/Semester 4/Capstone/Capstone 2016-2017/Literature /Method/QDA')

beetles <- read.table('T5_5_FBEETLES.DAT')
, col.names = c('Measurement.Number', 'Species', 'transverse.groove.dist', 'elytra.length', 'second.antennal.joint.length', 'third.antennal.joint.length'))

head(beetles)

###############################################################################
## QDA
###############################################################################

# Self Built
two.group.quadratic.classification <- function(data, grouping, newdata) {
  dat.split <- split(data, grouping)
  g1 <- as.data.frame(dat.split[1])
  g2 <- as.data.frame(dat.split[2])
  g1.means <- apply(g1, 2, mean)
  g2.means <- apply(g2, 2, mean)
  g1.covar <- cov(g1)
  g2.covar <- cov(g2)
  
  prediction <- apply(newdata, 1, function(y) {
    d2.y1 <- (y - g1.means) %*% solve(g1.covar) %*% (y - g1.means)
    d2.y2 <- (y - g2.means) %*% solve(g2.covar) %*% (y - g2.means)
    ifelse(d2.y1^2 > d2.y2^2, 2, 1)
  })
  
  class.table <- table(grouping, prediction, dnn = c('Actual Group','Predicted Group'))
  pred.errors <- sum(diag(t(apply(class.table, 2, rev)))) / dim(data)[1]
  results <- list('Prediction'=prediction, 'Table of Predictions'=class.table, 'Error Rate'=pred.errors)
  
  return(results)
}

beetle.quad <- two.group.quadratic.classification(beetles[,3:6], beetles[,2], beetles[,3:6])
beetle.quad

# Use package

library(MASS)

beetle.qda <- qda(Species ~.-Measurement.Number, data = beetles)

qda.pred <- predict(beetle.qda)$class
qda.pred

table(beetles$Species, qda.pred, dnn = c('Actual Group','Predicted Group'))

###############################################################################
## Cross Validation
###############################################################################


cv.prediction <- c()

for (i in 1:dim(beetles)[1]) {
  holdout <- beetles[-i,]
  y <- as.numeric(beetles[i,3:6])
  
  holdout1 <- holdout[holdout$Species == 1,][,3:6]
  holdout2 <- holdout[holdout$Species == 2,][,3:6]
  
  holdout1.means <- apply(holdout1, 2, mean)
  holdout2.means <- apply(holdout2, 2, mean)
  
  holdout1.covar <- cov(holdout1)
  holdout2.covar <- cov(holdout2)

  d2.y1 <- (y - holdout1.means) %*% solve(holdout1.covar) %*% (y - holdout1.means)
  d2.y2 <- (y - holdout2.means) %*% solve(holdout2.covar) %*% (y - holdout2.means)
  
  group <- ifelse(d2.y1^2 > d2.y2^2, 2, 1)
  cv.prediction <- append(cv.prediction, group)
}

table(beetles$Species, cv.prediction, dnn = c('Actual Group','Predicted Group'))


# Within the QDA package

beetle.qda.cv <- qda(Species ~.-Measurement.Number, CV = TRUE, data = beetles)
table(beetles$Species, beetle.qda.cv$class, dnn = c('Actual Group','Predicted Group'))

###############################################################################
## Example 2: http://www.statmethods.net/advstats/discriminant.html
###############################################################################

## LDA

# Linear Discriminant Analysis with Jacknifed Prediction 
library(MASS)
fit <- lda(G ~ x1 + x2 + x3, data=mydata, 
   na.action="na.omit", CV=TRUE)
fit # show results
  # cmt: The code above performs an LDA, using listwise deletion of missing data. CV=TRUE generates jacknifed (i.e., leave one out) predictions.

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(mydata$G, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))


## QDA

# Quadratic Discriminant Analysis with 3 groups applying 
# resubstitution prediction and equal prior probabilities. 
library(MASS)
fit <- qda(G ~ x1 + x2 + x3 + x4, data=na.omit(mydata),
  prior=c(1,1,1)/3))


## Compare results

# Scatter plot using the 1st two discriminant dimensions 
plot(fit) # fit from lda

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(fit, dimen=1, type="both") # fit from lda

# Exploratory Graph for LDA or QDA
library(klaR)
partimat(G~x1+x2+x3,data=mydata,method="lda")

# Scatterplot for 3 Group Problem 
pairs(mydata[c("x1","x2","x3")], main="My Title ", pch=22, 
   bg=c("red", "yellow", "blue")[unclass(mydata$G)])

















