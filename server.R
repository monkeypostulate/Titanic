# ############################################
# Title: Titanic App (Server)
# Author: Abel Camacho Guardian
# Version 1 (19.09.2019): Server code for the Titanic App
# Next steps: 1) Modularize code, 2) Add Confusion Matrix and Variable Importance
# 3) Package mlr does not work in the server
# #############################################

library(plotly)
library(randomForest)

# Datasets needed
titanic.train<-read.csv('titanic/train.csv') 
perf.data<-read.csv('output/performance_measure.csv')
roc.data<-read.csv('output/roc_data.csv')
cv.output<-readRDS('output/cv_output.RDS')
optimal.rf<-readRDS('output/optimal.rf.RDS')
train.model<-readRDS('output/rf_model.RDS')
confmat.data<-read.csv('output/confmat.csv')


var.desc<-read.csv('variables.csv', sep='\t')
perf.descr<-read.csv('performance_measures.csv', sep=';')
names(perf.descr)[1]<-'NameR'

# Define a server for the Shiny app
function(input, output) {
  
  
# #####################################################
# Output used in Data Description Page
# ####################################################
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlotly({
    variable<-input$variable
    totalperc<-input$totalperc
    totalperc<-ifelse(totalperc=='Total','dodge2','fill')
    titanic.train$chosen.var<-titanic.train[, variable ]
      
    
    titanic.train<-titanic.train%>%
      mutate(Survived=ifelse(Survived==1,'Yes','No'))
    
    # Render a barplot
    col1<-c('No'='#E8A87C','Yes'='#85CDCA')
    
    if(!variable %in% c('Fare','Age'))
      g<-titanic.train%>%
        group_by(chosen.var,Survived=factor(Survived))%>%
        summarise(Total=dplyr::n())%>%
        ggplot()+
        geom_bar(aes(x=chosen.var,y=Total,fill=Survived),
                 stat='identity', position=totalperc)+
        scale_fill_manual('Survive',values=col1)+
        theme_classic()+xlab(variable)+
        ylab(input$totalperc)
      
    if(variable %in% c('Fare','Age'))
      g<-titanic.train%>%
      ggplot()+
      geom_density(aes(x=chosen.var,fill=factor(Survived)))+
      scale_fill_manual('Survive',values=col1)+
      theme_classic()+xlab(variable)+
      ylab('Density')
    
       ggplotly(g)
  })
  
  

  output$variables.desc <- renderUI({
    description<-as.character(var.desc[var.desc$Var==input$variable,'Description'])
    
    HTML( paste0("<b> Description: </b> <br>",description))
     })
  
  output$summaryvariables <- renderPrint({
    titanic.train<-titanic.train[,c('Age','SibSp','Parch','Fare',
                                    'Cabin','Embarked','Pclass','Sex','Survived')]
    summary(titanic.train)
  })
  

  
# #####################################################
# Output used in Models & Performance Page
# #################################################### 
  
  output$perf.description<-renderUI({
    perfmea<-input$perfmea
    description<-perf.descr[perf.descr$Name==perfmea,]
    HTML(paste0("<b>",as.character(description$Name), "</b> (",description$NameR, ")",
                "<br>",as.character(description$Description)))
  })
  
  
  
  output$perf <-  renderTable({
    perfmea<-input$perfmea
    aa<-as.character(perf.descr[perf.descr$Name==perfmea,'NameR'])
    perf.data[,'var_arr']<-perf.data[,aa]
  perf.data%>%
    arrange(desc(var_arr))%>%
    select(-var_arr)
  })
  
  
  observeEvent(input$btn, {
# Javascript: Hide table with Model performance
    runjs('
  var x = document.getElementById("model_performance");
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
')
  })
  
 
  output$roc.plot <- renderPlotly({
    
    col<-c('ADABOOST'='#C5CBE3','LOGISTIC'='#F13C20', 'LOGISTIC 2'='#E27D60'
           ,'RF'='#C38D9E','DECISION TREE'='#E8A87C','SVM'='#41B3A3','BASELINE'='black')
    roc.plot<-roc.data%>%
      ggplot()+
      geom_line(aes(x=fpr,y=tpr,col=model), cex=1.2)+
      xlab('False positive rate')+ylab('True positive rate')+
      ggtitle('ROC curve')+theme_bw()+
      scale_color_manual(values=col,'Models')+
      geom_vline(xintercept=  input$threshold, lty=2)
    
    ggplotly(roc.plot)
  })
  

# #####################################################
# Output used in Random Forest Page
# ####################################################  
  
  output$cv.plot <- renderPlotly({  
    col<-c('1'='#C5CBE3','5'='#F13C20', '10'='#E27D60'
           ,'50'='#C38D9E','100'='#E8A87C','500'='#41B3A3')
    
    g<-cv.output$data%>%
      ggplot()+
      geom_line(aes(x=mtry,y=mmce.test.mean,col=factor(ntree)), lwd=2)+
      theme_classic()+
      xlab('# variables randomly sampled')+ylab('MMCE')+
      scale_color_manual(values=col, '# of Trees')
   ggplotly(g)
   
  })
  
  
  output$numtrees<-renderUI({
  HTML(  paste0('<b> Number of trees:</b> ',optimal.rf$x[1], '<br>
                <b>Number of variables:</b> ', optimal.rf$x[2])
  )
  })
  
  
  output$optimal.model <-  renderTable({
    data.frame(t(optimal.rf$y))
  })
  
  
# #####################################################
# Output used in Simulation Page
# #################################################### 
  output$simulprob<-renderUI({
  
    Name<-input$Name
    Pclass<-input$Pclass
    Sex<-input$Sex
    Age<-input$Age
    Fare<-input$Fare
    Embarked<-input$Embarked
    Parch<-1
    SibSp<-2

    data.simul<-data.frame('Pclass'=as.integer(Pclass),
               'Sex'=factor(Sex,levels=c('female','male')),
               'Age'=Age,  
               'SibSp'=as.integer(SibSp),
               'Parch'=as.integer(Parch),
               'Fare'=Fare,
               'Embarked'=factor(Embarked,levels=c('C','Q','S'))
               )
    data.simul<-rbind(titanic.train[1,c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked')],data.simul)
        sim.pred<-predict(train.model,
                      newdata =data.simul[-1,],type='prob')

    Survived<-'No'
    if(sim.pred[1,2]>=0.5)
      Survived<-'Yes'
    HTML(  paste0('<b>Name:</b> ',  Name   ,'<br>
    <b> Surviving probability:</b> ',sim.pred[1,2], '<br>
                <b>Prediction (Survive):</b> ', Survived)
    )
  })
    

  output$confmat <- renderUI({ 
    
    model1<-input$Model1
    val.confmat<-confmat.data%>%
      filter(threshold==input$threshold &
               model==model1)%>%
      select(measure ,perf)
    
    tp<-val.confmat[val.confmat$measure=='tp','perf']
    tn<-val.confmat[val.confmat$measure=='tn','perf']
    fp<-val.confmat[val.confmat$measure=='fp','perf']
    fn<-val.confmat[val.confmat$measure=='fn','perf']
  HTML(paste0('
  <h3>Confusion Matrix </h3>
  <p><b>Model: </b>',model1,'</p>
       <table class="confusionmat">
<tr>
<th style="color:white;
       background-color:#4056A1"> 
         </th>
         <th colspan="3"   style="text-align: center; color:white;
background-color:#4056A1">
         Predictions
       </th>
         </tr>
         <tr>
         <th rowspan="4" style="color:white;
background-color:#4056A1">
         Actuals
       </th>
         </tr>
         <tr>
         <th>        </th>
         <th>Survive</th>
         <th>Die</th>
         </tr>
         <tr>
         <th> Survive  </th>
         <td> ',tp,' </td>
         <td>',fn,'</td>
         </tr>
         <tr>
         <th> Die  </th>
         
         <td rowspan=2> ',fp,'</td>
         <td> ',tn,'</td>
         </tr>
         </table>
         <br>
         <p> <b> Precision: </b> ' ,round(tp/(tp+fp),2),'  </p>
         <p><b> Sensitivity:  </b> ',round(tp/(tp+fn),2) ,' </p>
         
       '))
  })
  
  
  
  output$precisionsens.plot <- renderPlotly({ 
  
    col<-c('#E8A87C','#85CDCA')
    
    model1<-input$Model1
    model2<-input$Model2
    g<-confmat.data%>%
      filter(measure %in% c('ppv','tpr'))%>%
      filter(model %in% c(model1,model2))%>%
      ggplot()+
      geom_line(aes(x=threshold,y=perf,col=model,lty=measure), lwd=1.1)+
      theme_classic()+
      xlab('Threshold')+ylab('')+ggtitle('Precision & Sensitivity')+
      scale_color_manual(values=col,'Model')+
      scale_linetype_manual(values=c('solid','dashed'),'Measure')+
      geom_vline(xintercept =input$threshold, lty=2)
    ggplotly(g)
    
  })
  
}