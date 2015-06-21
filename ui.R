
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
require(ggplot2)

shinyUI(fluidPage(theme = shinytheme("flatly"),

  # Application title
  titlePanel("Titanic Survival Analysis and Prediction"),

  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.baseTab=='tab0'",
                       tags$p("Hello All,"),tags$p("This a Data Application for Analysis of Titanic Data set.The data for this analysis was used from this",tags$a(href="http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv","link"),tags$p("This application can be used to create a model to predict survival of a passenger on Titanic Ship."))
      ),
      conditionalPanel(condition = "input.baseTab=='tab1'",
                       selectInput(inputId = "t1", "Select a Column Name",choices = NULL),
                       tags$p("This tab allows to view basic summary of all fields selected for Analysis. It displays a Histogram/Bar chart for the Column name")
                       ),
      conditionalPanel(condition = "input.baseTab=='tab2'",
                       selectInput(inputId = "t2", "Select a predictor to inlcude in model:",choices = NULL,multiple = T),
                       tags$p("This tab allows to create a simple classification model using 'rpart' function. The data is split into two sets Training(70%) and Validation (30%)."),
                       tags$p("User can select any number of predictors and view the model fit results to select appropriate predictors for final model.It also displays the 'rpart' Tree Model diagram.")
                       
      )      
    ),

    mainPanel(
      tabsetPanel(id = "baseTab",selected = NULL,
                  tabPanel(title = "Titanic Survival - Data Description",includeHTML("info.txt"),value="tab0"),
                  tabPanel(title = "Explorartory Analysis",tags$h3("Data Field Summary:"),pre(textOutput("summaryInfo")),tags$h3("Histogram/Bar Chart:"),plotOutput("histPlot"),value = "tab1"),
                  tabPanel(title = "Create Predictive Model",tags$h3("Model Formula:"),tags$p(tags$b(textOutput("formulaText"))),tags$h3("Confusion Matrix Result:"),pre(tags$b(textOutput("resultText"))),tags$h3("Tree Model Diagram:"),plotOutput("treePlot"),value = "tab2")
                  )
    )
  )
))
