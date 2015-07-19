## We need to generate a test and training set (prediction) for spam and not spam model
# If it isn't installed, install the kernlab package
library(kernlab)
data(spam)
head(spam)
str(spam)

# Perform the subsampling to build the model (and then test on other data), here split teh data into two pieces 
## using rbinom
set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
trainIndicator
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)

## Summaries
table(trainSpam$type)

## Exploratory plots
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

## Relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

##Clustering
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

## New clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)


Statistical prediction/modeling
Should be informed by the results of your exploratory analysis
Exact methods depend on the question of interest
Transformations/processing should be accounted for when necessary
Measures of uncertainty should be reported
Statistical prediction/modeling

trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
    lmFormula = reformulate(names(trainSpam)[i], response = "numType")
    glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"

Get a measure of uncertainty
## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

Get a measure of uncertainty
## Classification table
table(predictedSpam, testSpam$type)
##              
## predictedSpam nonspam spam
##       nonspam    1346  458
##       spam         61  449
## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)
## [1] 0.2243
