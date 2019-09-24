# ############################################
# Title: Titanic App (UI)
# Author: Abel Camacho Guardian
# Version 1 (19.09.2019): Server code for the Titanic App
# Next steps: 1) Modularize code & better notation
# #############################################

library(plotly)
library(shinyjs)

# Use a fluid Bootstrap layout
fluidPage(    
  # Add google Analytics
  tags$head(includeScript("www/google-analytics.js")),
  # Add Javascript Functions
  tags$head(includeScript("www/shiny_functions.js")),
  # Add CSS style
  includeCSS("www/StyleSheet1.css"),
  # Use pachage shinyjs
  useShinyjs(),


# ##############################################################
  # Give the page a title
  titlePanel("Titanic challenge",
             windowTitle="Titanic challenge"),
  




  navbarPage(id="navbar", "",
             
             
# #####################################################
# Data Description Page
# ####################################################
             
             tabPanel("Data Description",
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    div(class="sidebar",
    sidebarPanel( id="sidebar",
                  tags$div(class='selec',
      selectInput(inputId="variable", label="Variable:", 
                  choices=c('SibSp','Parch','Fare','Age','Cabin','Embarked','Pclass','Sex'))
      ),
      uiOutput("variables.desc"),
      hr(),
      selectInput(inputId="totalperc", label="Type of Plot:", 
                  choices=c('Total','Percentage') )
      )
    ),
    
    
    # Create a spot for the barplot
    mainPanel(
      
     plotlyOutput("phonePlot") ,
      br(), h4("Summary of Variables"),
      verbatimTextOutput("summaryvariables")
    )
    
  )
)
,


# #####################################################
# Models & Performance Page
# ####################################################

tabPanel("Models & Performance",
       
         sidebarLayout(      
           div(class="sidebar",
           # Define the sidebar with one input
           sidebarPanel(
             h4("Performance Measure"),
             selectInput(inputId="perfmea", label="Performance Measure:", 
                         choices=c("Accuracy",
                           "Mean misclassification error",
                                   "Precision/Positive predictive value",
                                   "Recall/Sensitivity/ True positive rate",
                                   "F1 measure",
                                   "Geometric mean of precision and recall",
                                   "Area under the curve",
                                   "Logarithmic loss (logloss)",
                                   "Time to train"
                         ) ),
             uiOutput("perf.description"),
             br(),
             actionButton("btn", "Hidde Performance Table"),
             hr(),
             selectInput(inputId="Model1", label="Select Model:", 
                         choices=c('RF','Logistic','SVM','Adaboost')),
             selectInput(inputId="Model2", label="Select Model 2:", 
                         choices=c('RF','Logistic','SVM','Adaboost'), selected='Logistic'),
             sliderInput(inputId='threshold', label='Threshold', value=0.5, min = 0, max = 1,step=0.1)
             
             
                        
           )),
           
           mainPanel(
             
             div(id="model_performance",
             h3("Model Performance"),
             tableOutput("perf"),
             hr()),
             plotlyOutput("roc.plot"),
             br(),
             uiOutput("confmat"),
             h3('Precision vs Sensitivity'),
             plotlyOutput("precisionsens.plot")
             )
         )
         
)

,
# #####################################################
# Random Forest Page
# ####################################################

tabPanel("Random Forest",
         
         sidebarLayout(      
           div(class="sidebar",
           # Define the sidebar with one input
           sidebarPanel(br(),
                        selectInput(inputId="cv.perf", label="Performance Measure", 
                                    choices=c("Mean misclassification error",
                                    "F1 measure",
                                    "Logarithmic loss (logloss)",
                                    "Area under the curve" ))
                        ) ),
           mainPanel(
             h3("Optimal model"),
             br(),
             p(id='optimal.model',
               'Hyperparameters:'),
             uiOutput("numtrees"),
             br(),
             h4('Model performance during Cross Validation'),
             tableOutput("optimal.model"),
             h3("Cross validation (10-fold)"),
             plotlyOutput("cv.plot")
             )
         )
)
,
# #####################################################
#  Simulation Page
# ####################################################

tabPanel("Simulation",
         
         sidebarLayout(      
           div(class="sidebar",
               # Define the sidebar with one input
               sidebarPanel(
                 h4('Passanger Information'),
                 textInput(inputId='Name', label='Name', value = "Abel Camacho"),
                 sliderInput(inputId='Age', label='Age', value=54, min = 0, max = 120),
                 sliderInput(inputId='Fare', label='Fare', value=10, min = 0, max = 400),
                 selectInput(inputId="Sex", label="Pclass:", 
                             choices=c('male','female')),
                 selectInput(inputId="Embarked", label="Embarked Port:", 
                             choices=c('C','Q','S')),
                 selectInput(inputId="Pclass", label="Pclass:", 
                             choices=c(1,2,3)),
                 numericInput(inputId='SibSp', label='SibSp', value="1", min = 0, max = 20,step=1),
                 numericInput(inputId='Parch', label='Parch', value="0", min = 0, max = 20,step=1)
                 
                    
                            ) ) ,
         
         mainPanel(
           
           h4("Surviving Probability"),
           uiOutput("simulprob")
)
         )
)


# ######################################
# End of Page

)

)
