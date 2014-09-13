## Script used to follow Dr. Peng's 'Structure of Data Analysis' slides
## in his 'Reproducible Data' Coursera class

library(kernlab)
data(spam)

## Dr. Peng set a seed for perfectly duplicatable results.
## But we want to see how close how close we can come.
#set.seed(3435)
set.seed(1234)

trainIndicator <- as.logical(rbinom(nrow(spam), size = 1, prob = 1/2))

table(trainIndicator)

## Splits the dataset into a test dataset and a training dataset.
trainSpam <- spam[trainIndicator, ]
testSpam <- spam[!trainIndicator, ]


plot(trainSpam$capitalAve ~ trainSpam$type)

plot(log10(trainSpam$capitalAve) + 1 ~ trainSpam$type)

hcluster <- hclust(dist(t(trainSpam[ , -58])))
plot(hcluster)


## Dr. Peng must have seen the capitalLong and capitalTotal already clustered
## so he omitted them from the log transformed hcluster
hcluster <- hclust(dist(t(log10(trainSpam[ , 1:55] + 1))))
plot(hcluster)


## Statistical prediction/modeling

trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)

for(i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

names(trainSpam)[which.min(cvError)]







