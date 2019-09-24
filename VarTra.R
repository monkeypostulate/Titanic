# ############################################
# Title: Variables transformations (VarTra.R)
# Author: Abel Camacho Guardian
# Version 1 (24.09.2019): Data transformations required for the models.
#                         This script is used in models.R
# #############################################

# ##################################
# Imputation of missing values is done with the mean
mean.Age<-mean(titanic.dataset$Age,na.rm=T)

titanic.dataset<-titanic.dataset%>%
  mutate(Age=replace_na(Age,mean.Age))


# ##################################
# The following variables transformations are only valid for the second 
# logistic model.
if(chosen.model=='classif.multinom2'){
  # Discretize Fare
  quant<-quantile(titanic.dataset$Fare, seq(.1,.9,.2))
  
  titanic.dataset<-titanic.dataset%>%
    mutate(
      Age.bucket=1*(Age>=5)+1*(Age>=15)+1*(Age>=30)+1*(Age>=50)+
        1*(Age>=60), # Discretize Age
      Age.Sex=paste0(Sex,'-',Age.bucket),
      Sibl.kid=1*(Age<=15)*(SibSp>=0)+1*(Age<=15)*(SibSp>=3), # Young Passangers with siblings
      kids.par=1*(Age>=30)*(Parch>=0)+1*(Age>=30)*(Parch>=3),  # Audults with children
      Fare.q=1*(Fare>quant[1])+1*(Fare>quant[2])+1*(Fare>quant[3])+
        1*(Fare>quant[4])+1*(Fare>quant[5]), # quantiztize Fare
      Fare.Sex=paste0(Sex,'-',Fare.q)                      
    )%>%
    mutate(Age.bucket=ifelse(Age.bucket==0,'[0,5)',
                             ifelse(Age.bucket==1,'[5,15)',
                                    ifelse(Age.bucket==2,'[15,30)',
                                           ifelse(Age.bucket==3,'[30,50)',
                                                  ifelse(Age.bucket==4,'[50,60)','[60, )' )) 
                                           )  )   )
           )%>%
  # Convert variables to factors
    mutate(Age.bucket=factor(Age.bucket),
           Age.Sex=factor(Age.Sex),
           Sibl.kid=factor(Sibl.kid),
           kids.par=factor(kids.par),
           Fare.Sex=factor(Fare.Sex)
    )
  

  columns[!columns
          %in%
            c('Sex','Age','SibSp','Parch','Fare')]  }

