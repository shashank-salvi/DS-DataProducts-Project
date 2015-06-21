
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rattle)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
library(e1071)
myTrainData <- read.csv("train.csv")

shinyServer(function(input, output,session) {
  
  ## Preprocessing and cleaning training data
  myTrainData$Survived <<- as.factor(myTrainData$Survived)
  levels(myTrainData$Survived) <<- c("No","Yes")
  
  myTrainData$Pclass <<- as.factor(myTrainData$Pclass)
  levels(myTrainData$Pclass) <<- c("First","Second","Third")
  
  tIndex <- which(names(myTrainData) == "Name")
  myTrainData <<- myTrainData[,-tIndex]
  tIndex <- which(names(myTrainData) == "Cabin")
  myTrainData <<- myTrainData[,-tIndex]
  tIndex <- which(names(myTrainData) == "Ticket")
  myTrainData <<- myTrainData[,-tIndex]
  tIndex <- which(myTrainData$Embarked=="")
  myTrainData <<- myTrainData[-tIndex,]
  myTrainData$Embarked <- factor(myTrainData$Embarked)
  myTrainData <<- myTrainData[complete.cases(myTrainData),]
  
  ## Training and Validation Sets
  trainSet <- NULL
  validSet <- NULL
  
observe({
      updateSelectInput(session = session,inputId = "t1",label = "Select a Column Name",choices = names(myTrainData),selected = NULL)
      
      updateSelectInput(session = session,inputId = "t2",label = "Select a predictor to inlcude in model",choices=names(myTrainData)[-c(1,2)],selected = NULL)
    })
    
  output$summaryInfo <- renderPrint({
      cname <- input$t1
      if( is.null(cname) || (cname=="") )
      {
        return(NULL)
      }
      summary(myTrainData[,cname])
    })
  
  output$histPlot <- renderPlot({
    cname <- input$t1
    mf_labeller <- function(var, value){
      value <- as.character(value)
      if (var=="Survived") { 
        value[value=="No"] <- "Survived = No"
        value[value=="Yes"]   <- "Survived = Yes"
      }
      return(value)
    }
    
    if(is.numeric(myTrainData[,cname]))
    {
      p <- ggplot(data=myTrainData,aes_string(x=cname)) + geom_histogram(alpha=0.35,col="red",fill="green") + facet_grid(Survived~.,labeller = mf_labeller)
      p <- p + theme(strip.text.y=element_text(face="bold"),
                     strip.background = element_rect(colour="red"))
      return(p)
    }
    else
    {
      p <- NULL;
      if(cname!="Survived")
      {
        p <- ggplot(data=myTrainData,aes_string(x=cname)) + geom_bar(alpha=0.35,col="red",fill="green") + facet_grid(Survived~.,labeller = mf_labeller)
        p <- p + theme(strip.text.y=element_text(face="bold"),
                       strip.background = element_rect(colour="red"))
      }
      else
      {
        p <- ggplot(data=myTrainData,aes_string(x=cname)) + geom_bar(alpha=0.35,col="red",fill="green")
      }
      return(p)
    }
    
  })
  
  output$formulaText <- renderText({
    cname <- input$t2
    if( is.null(cname) || (cname=="") )
    {
      return(NULL)
    }
    tResultText <- "FORMULA = Survived ~ "
    tnameList <- do.call(paste,c(as.list(cname),sep=" + "))
    return(paste(tResultText,tnameList,sep=""))
  })
  
  modelFit <- reactive({
    cname <- input$t2
    if( is.null(cname) || (cname=="") )
    {
      return(NULL)
    }
    tempData <- myTrainData
    tIndex <- which(names(tempData) == "PassengerId")
    tempData <- tempData[,-tIndex]
    
    tIndex <- sapply(cname,function(x){return(which(names(tempData)==x))})
    names(tIndex) <- NULL
    tempData <- tempData[,c(1,tIndex)]    
    
    set.seed(333)
    indexT <- createDataPartition(tempData$Survived, p=0.70, list=F)
    trainSet <<- tempData[indexT, ]
    validSet <<- tempData[-indexT, ]
    
    modelDT <- rpart(Survived ~ .,data=tempData,method='class')        
  })
  
  output$resultText <- renderPrint({
    modelDT <- modelFit()
    if(is.null(modelDT))
    {
      return(NULL)
    }
    predictRf <- predict(modelDT, validSet,type = "class")
    confusionMatrix(validSet$Survived, predictRf)    
  })
  
  output$treePlot <- renderPlot({
    fit <- modelFit()
    if(is.null(fit))
    {
      return(NULL)
    }
    fancyRpartPlot(fit)
  })
  
})
