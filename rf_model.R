# ############################################
# Title: rf_model
# Author: Abel Camacho Guardian
# Version 1 (19.09.2019): Calculate the best random forest model
# Next steps: Add a list with passangers with highest risk to not survive
# #############################################

# Before running this script, you should run the script models.R


# Variables used in the model
columns<-c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked','Survived')


# ##################################
# Create Train and Test data
titanic.train<-titanic.dataset%>%
  filter(id %in%train.sample)
titanic.test<-titanic.dataset%>%
  filter(!(id %in%train.sample))


titanic.train<-titanic.dataset[,columns]
titanic.test<-titanic.test[,columns]


# Model: Random Forest
chosen.learner<-'classif.randomForest'

classifi.model<-makeClassifTask(id='titanic',
                                data=titanic.train,
                                target='Survived')

# ##################################
# Hyper parameters
discrete_ps = makeParamSet(
  makeDiscreteParam("ntree",
                    values =c(1,5,10,50,100,500)),
  makeDiscreteParam("mtry",
                    values =c(1,2,3,4,12))
  )

# ##################################
# Cross validation: 10 fold.
  rdesc<-makeResampleDesc(method="CV",
                          iters = 10,
                          predict = 'test')

  ctrl<-makeTuneControlGrid()
  
  chosen.model<-  'classif.randomForest'
  chosen.learner<-makeLearner(chosen.model,
                              predict.type = "prob",
                              par.vals = list())
  # Calculate optimal model
  optimal.rf<-tuneParams(chosen.learner, task = classifi.model, resampling =
                    rdesc,
                  par.set = discrete_ps, control=ctrl,
                  measures = list(acc,mmce,timetrain,f1,logloss)) # measures to be calculated
  

  
cv.output = generateHyperParsEffectData(optimal.rf)



## #######################################
# Get Optimal mode#
# ########################################
chosen.learner<-makeLearner(chosen.model,
                            predict.type = "prob",
                            par.vals = list(ntree=optimal.rf$x$ntree,
                                            mtry=optimal.rf$x$mtry))

train.model<-mlr::train(learner=chosen.learner,
                        task=classifi.model)

# ##################################
# Predictions (with optimal model)
pred.classif<-predict(train.model,
                      newdata =titanic.test)


best.model.per<-performance(pred.classif, measures =chosen.measures,
            model=train.model)




# ##################################
# Save Files (Check if files are up -to-date)
# ##################################

# saveRDS(optimal.rf,'output/optimal.rf.RDS')
# saveRDS(cv.output,'output/cv_output.RDS')
# write.csv(best.model.per,'output\\best_model_perf.csv')

# saveRDS(train.model,'output/opt_rf.RDS')
# saveRDS(getLearnerModel(train.model),'output/rf_model.RDS')
