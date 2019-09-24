# ############################################
# Title: Models
# Author: Abel Camacho Guardian
# Version 1 (19.09.2019): Quick and dirty models to understand the advantage and
#                         disadvantage of each model. Results are store in excel files.
# Next steps: Add naive bayes classifier and Neural Networks 
# Output: ROC curve and performance measure
#         Results are saved in  folder "output"
# #############################################


# Load libraries
library(tidyverse)
library(mlr)
library(tidyverse)
library(randomForest)
library(e1071)
library(adabag)
library(nnet)

set.seed(21)



confuMats<-list()
perfo.models<-list()
ROCs<-list()
pred.classif<-list()



# ####################################
# Load datasets
titanic.dataset<-read.csv('titanic\\train.csv') 
titanic.dataset$id<-1:dim(titanic.dataset)[1]

# Randomize data: Create train and test dataset
train.sample <- sample(x=1:dim(titanic.dataset)[1],
                       size=floor(dim(titanic.dataset)[1]*0.75), # Train data consists of 75% of the dataset
                       replace=F)

# ####################################
# Define Models: Random Forest, Decision Tree, Logistic Regression and Adaptative Boosting
selected.models<-c('classif.randomForest',
                   'classif.rpart',
                   'classif.multinom',
                   'classif.multinom2',
                   'classif.svm','classif.boosting')


# Performance Measures
chosen.measures<-list(acc,
                      mmce,
                      ppv,
                      tpr,
                      f1,
                      gpr,
                      auc,
                      logloss,
                      timetrain)

# Quick & dirty analysis of the ML algorithms

i<-1
for(chosen.model in selected.models){



# ####################################
# Data transformations
name.model<-chosen.model

columns<-c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked','Survived')
source('VarTra.R')
if(chosen.model=='classif.multinom2'){
  name.model<-chosen.model
  chosen.model<-'classif.multinom'
}

# Create Train and Test data
titanic.train<-titanic.dataset%>%
  filter(id %in%train.sample)
titanic.test<-titanic.dataset%>%
  filter(!(id %in%train.sample))

# Variables used in the model
titanic.train<-titanic.dataset[,columns]
titanic.test<-titanic.test[,columns]


# ###################################
# Modelling
# ####################################
classifi.model<-makeClassifTask(id='titanic',
                                data=titanic.train,
                                target='Survived')

chosen.learner<-makeLearner(chosen.model,
                            predict.type = "prob",
                            par.vals = list())


train.model<-mlr::train(learner=chosen.learner,
                     task=classifi.model)


# ##################################
# Predictions
# ##################################
pred.classif[[i]]<-predict(train.model,
                         newdata =titanic.test)


# ##################################
# Model Performance
# ##################################
conf.mat<-
  calculateConfusionMatrix(pred.classif[[i]], relative = TRUE)


perf<-performance(pred.classif[[i]], measures =chosen.measures,
                  model=train.model)


confuMats[[i]]<-conf.mat
perfo.models[[i]]<-perf

# ROC curve
roc = generateThreshVsPerfData(pred.classif[[i]], list(fpr, tpr))
ROCs[[i]]<-roc


names(confuMats)[i]<-names(perfo.models)[i]<-names(ROCs)[i]<-name.model
  i<-i+1
}
# ###########
# End of Loop
# ###########


# ####################################
# Save results
# ####################################

# ##################
# Clean  roc curve data
roc.data<-rbind(
  cbind('model'='RF',ROCs$classif.randomForest$data),
  cbind('model'='DECISION TREE',ROCs$classif.rpart$data),
  cbind('model'='LOGISTIC',ROCs$classif.multinom$data),
      cbind('model'='LOGISTIC 2',ROCs$classif.multinom2$data),
      cbind('model'='SVM',ROCs$classif.svm$data),
      cbind('model'='ADABOOST',ROCs$classif.boosting$data),
      cbind(model='BASELINE','fpr'=c(0,1),'tpr'=c(0,1),'threshold'=c(0,1)))

roc.data<-roc.data%>%
  mutate(fpr=as.numeric(fpr),
         tpr=as.numeric(tpr))

write.csv(roc.data,'output\\roc_data.csv')


# ##################
# Clean  performance measure results
perf.data<-rbind(
  cbind('model'='RF',t(perfo.models$classif.randomForest)),
  cbind('model'='DECISION TREE',t(perfo.models$classif.rpart)),
  cbind('model'='Logistic',t(perfo.models$classif.multinom)),
                 cbind('model'='Logistic 2',t(perfo.models$classif.multinom2)),
                 cbind('model'='SVM',t(perfo.models$classif.svm)),
                 cbind('model'='ADABOOST',t(perfo.models$classif.boosting))
)


perf.data<-as_tibble(as.matrix(perf.data))
perf.data<-perf.data%>%
  mutate(timetrain=round(as.numeric(timetrain),3),
         acc=round(as.numeric(acc),3),
         mmce=round(as.numeric(mmce),3),
         auc=round(as.numeric(auc),3),
         logloss=round(as.numeric(logloss),3))


write.csv(perf.data,'output\\performance_measure.csv')

