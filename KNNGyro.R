KNNGyro <- read.delim("~/@Rscripts/KNNGyro.data")
View(KNNGyro)
summary(KNNGyro)
KNNGyro
nrow(KNNGyro)
###############################
## Prepare Normalize function##
###############################
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
## DATAS SHUFFLEING ##
## DEFINE SEED FOR RANDOM NUMBER GENERATION
## So the trianing will always be the same.
KNNGyro.norm <- as.data.frame(lapply(KNNGyro[1:6], normalize))
KNNGyro.norm
nrow(KNNGyro.norm)

#loading knn library
library(class)


set.seed(1234) 
####################
## DATA SAMPLING  ##
## 1 for 60%      ##
## 2 for 40%      ##
## 1 for training ##
## 2 for testing  ##
####################

####################
##DATA INPUT SETUP##
#####################
### OBSERVATIONS  ###
#####################
indexing <- sample(2, nrow(KNNGyro.norm), replace=TRUE, prob=c(0.40, 0.60))
KNNGyro.training <- KNNGyro.norm[indexing==1, 1:6]
KNNGyro.test <- KNNGyro.norm[indexing==2, 1:6]
KNNGyro.training
nrow(KNNGyro.training)
KNNGyro.test
nrow(KNNGyro.test)
####################
### Labeling     ###
####################
KNNGyro.trainLabels <- KNNGyro[indexing==1, 7]
KNNGyro.testLabels <- KNNGyro[indexing==2, 7]
KNNGyro.trainLabels
KNNGyro.testLabels


#####################
##PREDITION MODEL  ##
#####################
KNNGyro_pred <- knn(train = KNNGyro.training, test = KNNGyro.test, cl = KNNGyro.trainLabels, k=sqrt(nrow(KNNGyro.training)))
KNNGyro_pred
KNNGyro.testLabels
## Install CrossTable for review prediction vs testing if not avaiable.
## install.packages("gmodels") 
library(gmodels)
## Note that the last argument prop.chisq indicates 
## whether or not the chi-square contribution of each cell is included. 
## The chi-square statistic is the sum of the contributions 
## from each of the individual cells and is used to decide 
## whether the difference between the observed and 
## the expected values is significant.
CrossTable(x = KNNGyro.testLabels, y = KNNGyro_pred, prop.chisq=FALSE)
#########################################
## Confirmed the prediction is correct ##
## Apply model to assumption test       ##
#########################################
test= c(2,3,3,2,3,3)
falling_detect <- knn(train = KNNGyro.training, test =test, cl = KNNGyro.trainLabels, k=3)
falling_detect
