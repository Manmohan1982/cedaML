#' @title Complete Exploratory Data Analysis and Machine Learning Tool
#'
#' @description This package allows you to do EDA and run supervised machine Learning models via a UI in Shiny
#'
#' @param NULL
#'
#' @return NULL
#'
#' @examples visualise()
#'
#' @export
visualise<-function(){
  #install.packages("pacman")
  #library(pacman)
  #p_load("shiny","sqldf","ggplot2","plotly","shinydashboard","lubridate",
  #       "reshape2","gridExtra","gtable","grid","plyr","qcc","DT",
  #       "shinyjs","gtools","V8","VIM","corrplot","tabplot","e1071","vcd","caret",
  #       "caretEnsemble","randomForest","kernlab","rpart","glmnet","xgboost","gbm","tidyverse")

  jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
  options(sqldf.driver = "SQLite")

  LFC<-list()
  options(shiny.maxRequestSize = 1000*1024^2)


  server<-(function(input, output, session) {

    shiny::observeEvent(input$reset_button, {js$reset()})

    #Upload a Dataset

    loadfile1 <- reactive({


      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Dataset Loading..Kindly wait")

      S <- input$file1


      if (is.null(S))
        return(NULL)

      temp = read.csv (S$datapath, header=TRUE,stringsAsFactors = TRUE)

    }) #End of Load File Reactive

    # File Upload

    output$FileUpload1 = shiny::renderUI(
      shiny::fileInput('file1', 'Upload Training Dataset (CSV File)',
                       accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'))
    )


    #Upload a Dataset

    loadfile2 <- shiny::reactive({


      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Dataset Loading..Kindly wait")

      S <- input$file2


      if (is.null(S))
        return(NULL)

      temp = read.csv (S$datapath, header=TRUE,stringsAsFactors = FALSE)

    }) #End of Load File Reactive

    # File Upload

    output$FileUpload2 = shiny::renderUI(
      shiny::fileInput('file2', 'Upload Testing Dataset (CSV File)',
                       accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'))
    )


    #Upload a Dataset

    loadfile3 <- shiny::reactive({


      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Dataset Loading..Kindly wait")

      S <- input$file3


      if (is.null(S))
        return(NULL)

      temp = read.csv (S$datapath, header=TRUE,stringsAsFactors = TRUE)

    }) #End of Load File Reactive

    # File Upload

    output$FileUpload3 = shiny::renderUI(
      shiny::fileInput('file3', 'Upload Cleaned Training Dataset (CSV File)',
                       accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'))
    )


    output$ExploreWay = shiny::renderUI({
      shiny::radioButtons("ExploreWays", label = "Explore",
                          choices = list("Summary" = 1, "Structure" = 2,"Missing"=3,"Correlation"=4,"Skewness"=5,"Kurtosis"=6),
                          selected = 1)
    })

    output$NumPredictors = shiny::renderUI({
      shiny::radioButtons("NumPredictor", label = "Include all the predictors",
                          choices = list("Yes" = 1, "No" = 2),
                          selected = 1)
    })


    # Drop down for modeling echniques
    output$MLTS = shiny::renderUI({
      DF<-loadfile3()
      if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
        shiny::radioButtons("MLT", label = "Machine Learning Technique",
                            choices = list("Linear Regression" = 1, "Regularized Regression" = 3,"Decision Tree"=4,"Random Forest"=5,"Gradient Boosting"=6,"Extreme Gradient Boosting"=7,"Support Vector Machine"=8,"All(No Tuning)"=9),
                            selected = 1)
      }else{shiny::radioButtons("MLT", label = "Machine Learning Technique",
                                choices = list("Logistic Regression"=2,"Regularized Regression" = 3,"Decision Tree"=4,"Random Forest"=5,"Gradient Boosting"=6,"Extreme Gradient Boosting"=7,"Support Vector Machine"=8,"All(No Tuning)"=9),
                                selected = 1)}
    })

    # Drop down for modeling echniques
    output$PredictMLTS = shiny::renderUI({
      DF<-loadfile3()
      if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
        shiny::radioButtons("PredictMLT", label = "Prediction based on",
                            choices = list("Linear Regression" = 1, "Regularized Regression" = 3,"Decision Tree"=4,"Random Forest"=5,"Gradient Boosting"=6,"Extreme Gradient Boosting"=7,"Support Vector Machine"=8),
                            selected = 1)
      }else{shiny::radioButtons("PredictMLT", label = "Prediction based on",
                                choices = list("Logistic Regression"=2,"Regularized Regression" = 3,"Decision Tree"=4,"Random Forest"=5,"Gradient Boosting"=6,"Extreme Gradient Boosting"=7,"Support Vector Machine"=8),
                                selected = 1)}
    })

    # Tuning of the parameters

    output$Tunings = renderUI({
      shiny::radioButtons("Tuning", label = "Do you want to tune Model",
                          choices = list("Yes" = 1, "No" = 2),
                          selected = 2)
    })

    # Way of Tuning of the parameters

    output$TuningTypes = shiny::renderUI(if(input$Tuning==1){
      shiny::radioButtons("TuningType", label = "Parameter Tuning Way:",
                          choices = list("Random Search" = 1, "Grid Search" = 2),
                          selected = 1)
    })

    # Tuning Length for Random Search

    output$NumFolds = shiny::renderUI({
      shiny::numericInput("NumFolds", "Number of Folds Cross Validation:", 2, min = 1, max = 10)
    })

    # Tuning Length for Random Search

    output$TuneLength = shiny::renderUI(if(input$TuningType==1 & input$Tuning==1){
      shiny::textInput("TuneLengths", "Tune Length", value = "", width = NULL, placeholder = NULL)
    })
    # Paramter Tuning for Regularized Regression

    output$RegTunAlphaStart = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunAlphaStarts", "Start alpha", value = "", width = NULL, placeholder = NULL)
    })

    output$RegTunAlphaEnd = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunAlphaEnds", "End alpha", value = "", width = NULL, placeholder = NULL)
    })

    output$RegTunAlphaStep = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunAlphaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$RegTunLambdaStart = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunLambdaStarts", "Start Lambda", value = "", width = NULL, placeholder = NULL)
    })

    output$RegTunLambdaEnd = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunLambdaEnds", "End Lambda", value = "", width = NULL, placeholder = NULL)
    })

    output$RegTunLambdaStep = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shiny::textInput("RegTunLambdaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning Regularized Regression
    output$RegTun = shiny::renderUI(if(input$MLT==3 & input$TuningType==2){
      shinydashboard::box(shiny::column(6,shinydashboard::box(shiny::uiOutput("RegTunAlphaStart"),
                                                              shiny::uiOutput("RegTunAlphaEnd"),
                                                              shiny::uiOutput("RegTunAlphaStep"),width=10)),
                          shiny::column(6,shinydashboard::box(shiny::uiOutput("RegTunLambdaStart"),
                                                              shiny::uiOutput("RegTunLambdaEnd"),
                                                              shiny::uiOutput("RegTunLambdaStep"),width=10)),title="",status = "primary",solidHeader = T)

    })
    # Paramter Tuning for Random Forest

    output$RFTunMtryStart = shiny::renderUI(if(input$MLT==5 & input$TuningType==2){
      shiny::textInput("RFTunMtryStarts", "Start mtry", value = "", width = NULL, placeholder = NULL)
    })

    output$RFTunMtryEnd = shiny::renderUI(if(input$MLT==5 & input$TuningType==2){
      shiny::textInput("RFTunMtryEnds", "End mtry", value = "", width = NULL, placeholder = NULL)
    })

    output$RFTunMtryStep = shiny::renderUI(if(input$MLT==5 & input$TuningType==2){
      shiny::textInput("RFTunMtrySteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning Random Forest
    output$RFTun = shiny::renderUI(if(input$MLT==5 & input$TuningType==2){
      shiny::column(6,shinydashboard::box(shiny::uiOutput("RFTunMtryStart"),
                                          shiny::uiOutput("RFTunMtryEnd"),
                                          shiny::uiOutput("RFTunMtryStep"),width=10,title="",status = "primary",solidHeader = T))

    })


    # Paramter Tuning for Decision Tree

    output$DtTunCpStart = shiny::renderUI(if(input$MLT==4 & input$TuningType==2){
      shiny::textInput("DtTunCpStarts", "Start Cp", value = "", width = NULL, placeholder = NULL)
    })

    output$DtTunCpEnd = shiny::renderUI(if(input$MLT==4 & input$TuningType==2){
      shiny::textInput("DtTunCpEnds", "End Cp", value = "", width = NULL, placeholder = NULL)
    })

    output$DtTunCpStep = shiny::renderUI(if(input$MLT==4 & input$TuningType==2){
      shiny::textInput("DtTunCpSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning Decision Tree
    output$DtTun = shiny::renderUI(if(input$MLT==4 & input$TuningType==2){
      shiny::column(6,shinyashboard::box(shiny::uiOutput("DtTunCpStart"),
                                         shiny::uiOutput("DtTunCpEnd"),
                                         shiny::uiOutput("DtTunCpStep"),width=10,title="",status = "primary",solidHeader = T))

    })




    # Paramter Tuning for XGBoost

    output$XgbTunAlphaStart = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunAlphaStarts", "Start alpha", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunAlphaEnd = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunAlphaEnds", "End alpha", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunAlphaStep = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunAlphaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunLambdaStart = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunLambdaStarts", "Start Lambda", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunLambdaEnd = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunLambdaEnds", "End Lambda", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunLambdaStep = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunLambdaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunEtaStart = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunEtaStarts", "Start Eta", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunEtaEnd = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunEtaEnds", "End Eta", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunEtaStep = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunEtaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunNroundStart = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunNroundStarts", "Start Nroud", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunNroundEnd = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunNroundEnds", "End Nround", value = "", width = NULL, placeholder = NULL)
    })

    output$XgbTunNroundStep = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shiny::textInput("XgbTunNroundSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning XGboost
    output$XgbTun = shiny::renderUI(if(input$MLT==7 & input$TuningType==2){
      shinydashboard::box(shiny::column(3,shinydashboard::box(shiny::uiOutput("XgbTunAlphaStart"),
                                                              shiny::uiOutput("XgbTunAlphaEnd"),
                                                              shiny::uiOutput("XgbTunAlphaStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("XgbTunLambdaStart"),
                                                              shiny::uiOutput("XgbTunLambdaEnd"),
                                                              shiny::uiOutput("XgbTunLambdaStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("XgbTunEtaStart"),
                                                              shiny::uiOutput("XgbTunEtaEnd"),
                                                              shiny::uiOutput("XgbTunEtaStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("XgbTunNroundStart"),
                                                              shiny::uiOutput("XgbTunNroundEnd"),
                                                              shiny::uiOutput("XgbTunNroundStep"),width=20)),title="",status = "primary",solidHeader = T)

    })



    # Paramter Tuning for GBM

    output$GbmTunDepthStart = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunDepthStarts", "Start Depth", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunDepthEnd = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunDepthEnds", "End Depth", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunDepthStep = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunDepthSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunShrinkageStart = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunShrinkageStarts", "Start Shrinkage", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunShrinkageEnd = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunShrinkageEnds", "End Shrinkage", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunShrinkageStep = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunShrinkageSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunMinObsNodeStart = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunMinObsNodeStarts", "Start MinObsNode", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunMinObsNodeEnd = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunMinObsNodeEnds", "End MinObsNode", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunMinObsNodeStep = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunMinObsNodeSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunNtreeStart = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunNtreeStarts", "Start Ntree", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunNtreeEnd = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunNtreeEnds", "End Ntree", value = "", width = NULL, placeholder = NULL)
    })

    output$GbmTunNtreeStep = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shiny::textInput("GbmTunNtreeSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning Gbmoost
    output$GbmTun = shiny::renderUI(if(input$MLT==6 & input$TuningType==2){
      shinydashboard::box(shiny::column(3,shinydashboard::box(shiny::uiOutput("GbmTunDepthStart"),
                                                              shiny::uiOutput("GbmTunDepthEnd"),
                                                              shiny::uiOutput("GbmTunDepthStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("GbmTunShrinkageStart"),
                                                              shiny::uiOutput("GbmTunShrinkageEnd"),
                                                              shiny::uiOutput("GbmTunShrinkageStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("GbmTunMinObsNodeStart"),
                                                              shiny::uiOutput("GbmTunMinObsNodeEnd"),
                                                              shiny::uiOutput("GbmTunMinObsNodeStep"),width=20)),
                          shiny::column(3,shinydashboard::box(shiny::uiOutput("GbmTunNtreeStart"),
                                                              shiny::uiOutput("GbmTunNtreeEnd"),
                                                              shiny::uiOutput("GbmTunNtreeStep"),width=20)),title="",status = "primary",solidHeader = T)

    })



    # Paramter Tuning for SVM

    output$SvmTunSigmaStart = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunSigmaStarts", "Start Sigma", value = "", width = NULL, placeholder = NULL)
    })

    output$SvmTunSigmaEnd = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunSigmaEnds", "End Sigma", value = "", width = NULL, placeholder = NULL)
    })

    output$SvmTunSigmaStep = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunSigmaSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    output$SvmTunCStart = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunCStarts", "Start C", value = "", width = NULL, placeholder = NULL)
    })

    output$SvmTunCEnd = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunCEnds", "End C", value = "", width = NULL, placeholder = NULL)
    })

    output$SvmTunCStep = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shiny::textInput("SvmTunCSteps", "Step", value = "", width = NULL, placeholder = NULL)
    })

    # Box for Tuning SVM
    output$SvmTun = shiny::renderUI(if(input$MLT==8 & input$TuningType==2){
      shinydashboard::box(shiny::column(6,shinydashboard::box(shiny::uiOutput("SvmTunSigmaStart"),
                                                              shiny::uiOutput("SvmTunSigmaEnd"),
                                                              shiny::uiOutput("SvmTunSigmaStep"),width=10)),
                          shiny::column(6,shinydashboard::box(shiny::uiOutput("SvmTunCStart"),
                                                              shiny::uiOutput("SvmTunCEnd"),
                                                              shiny::uiOutput("SvmTunCStep"),width=10)),title="",status = "primary",solidHeader = T)

    })






    # Choose which Data Type to be Factor

    output$FactorList = shiny::renderUI({
      shiny::selectInput(
        "FactorLists",
        label = "Convert following to Factor Data Type",
        "",selectize=TRUE,multiple=TRUE,choices=names(loadfile1())
      )
    })

    # Choose the Target Variable

    output$Targets = shiny::renderUI({
      shiny::selectInput(
        "Target",
        label = "Chose the Target Variable",
        "",selectize=TRUE,multiple=FALSE,choices=names(loadfile3())
      )
    })


    # Choose the Target Variable

    output$Predictors = shiny::renderUI(if(input$NumPredictor==2){
      shiny::selectInput(
        "Predictor",
        label = "Chose Predictors",
        "",selectize=TRUE,multiple=TRUE,choices=names(loadfile3())
      )
    })


    # Choose which Data Type to be Numeric

    output$NumericList = shiny::renderUI({
      shiny::selectInput(
        "NumericLists",
        label = "Convert following to Factor Data Type",
        "",selectize=TRUE,multiple=TRUE,choices=names(loadfile1())
      )
    })

    # Choose Histogram Parameter

    output$HistParams = shiny::renderUI(if(input$PlotType=="Histogram"){
      nums <- sapply(loadfile1(), is.numeric)
      numericcols<-as.list(colnames(loadfile1()[,nums]))
      shiny::selectInput(
        "HistParam",
        label = "Plot Histogram",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })

    # Choose BoxPlot Parameter

    output$BoxParams = shiny::renderUI(if(input$PlotType=="Box"){
      nums <- sapply(loadfile1(), is.numeric)
      numericcols<-as.list(colnames(loadfile1()[,nums]))
      shiny::selectInput(
        "BoxParam",
        label = "Box Plot",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })


    # Choose Group By Parameter for Histogram

    output$GrByBox = shiny::renderUI(if(input$PlotType=="Box"){
      nums <- sapply(loadfile1(), is.numeric)
      numericcols<-as.list(colnames(loadfile1()[,nums]))
      index<-Matrix::which(names(loadfile1()) %in% numericcols)
      `%>%` <- magrittr::`%>%`
      nonnumericcols<-as.list(colnames(loadfile1()%>%dplyr::select_if(purrr::negate(is.numeric))))
      shiny::selectInput(
        "GrByBoxs",
        label = "Group By",
        "",selectize=TRUE,multiple=FALSE,choices=nonnumericcols
      )
    })

    # Dropdown to select X-axis for plot

    output$XaxisTypes = shiny::renderUI(if(input$PlotType=="Scatter"){
      nums <- sapply(loadfile1(), is.numeric)
      numericcols<-as.list(colnames(loadfile1()[,nums]))
      shiny::selectInput(
        "Xaxis",
        label = "Select Xaxis",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })

    # Dropdown to select y-axis for plot

    output$YaxisTypes = shiny::renderUI(if(input$PlotType=="Scatter"){
      nums <- sapply(loadfile1(), is.numeric)
      numericcols<-as.list(colnames(loadfile1()[,nums]))
      shiny::selectInput(
        "Yaxis",
        label = "Select Yaxis",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })

    # Dropdown to select first variable for mosaic plot

    output$MosaicFirst = shiny::renderUI(if(input$PlotType=="Mosaic"){
      factors <- sapply(loadfile1(), is.factor)
      factorcols<-as.list(colnames(loadfile1()[,factors]))
      shiny::selectInput(
        "Mosaic1st",
        label = "Select First Variable",
        "",selectize=TRUE,multiple=FALSE,choices=factorcols
      )
    })

    # Dropdown to select select variable for plot

    output$MosaicSecond = shiny::renderUI(if(input$PlotType=="Mosaic"){
      factors <- sapply(loadfile1(), is.factor)
      factorcols<-as.list(colnames(loadfile1()[,factors]))
      shiny::selectInput(
        "Mosaic2nd",
        label = "Select Second Variable",
        "",selectize=TRUE,multiple=FALSE,choices=factorcols
      )
    })



    # Dropdown to select columns for tabplot

    output$SelectTabPlots = shiny::renderUI({
      shiny::selectInput(
        "SelectTabPlot",
        label = "Select Columns",
        "",selectize=TRUE,multiple=TRUE,choices=names(loadfile1())
      )
    })

    # Dropdown to sort columns for tabplot

    output$SortTabPlots = shiny::renderUI({
      shiny::selectInput(
        "SortTabPlot",
        label = "Select Sorting Column",
        "",selectize=TRUE,multiple=FALSE,choices=input$SelectTabPlot
      )
    })

    # Slider Input for range for the sorted column
    output$FilterTabPlots = shiny::renderUI({

      shiny::sliderInput('FilterTabPlot',
                         paste0("Choose Filter Range on Sorted Column"),
                         min = 0,
                         max = 100,
                         value = c(0,100),round=TRUE)

    })

    # Slider Input for range for the Correlation Range
    output$CorRanges = shiny::renderUI({

      shiny::sliderInput('CorRange',
                         paste0("Exclude Correlation Between"),
                         min = -1,
                         max = 1,
                         step=0.1,
                         value = c(-1,1),round=FALSE)

    })

    # Dropdown to choose Plot Type

    output$PlotTypes = shiny::renderUI({
      shiny::selectInput(
        "PlotType",
        label = "Select Plot",
        "",selectize=TRUE,multiple=FALSE,choices=c("Missing","Histogram","Box","Scatter","Scatter Matrix","Correlation","Tabular","Mosaic")
      )
    })


    # Dropdown to choose Metric Type

    output$MetricTypes = shiny::renderUI({
      shiny::selectInput(
        "MetricType",
        label = "Select Metric",
        "",selectize=TRUE,multiple=FALSE,choices=c("RMSE","ROC","Accuracy","Kappa")
      )
    })

    # changing Data Type of the Dataset

    DataTypeConversion <-shiny::reactive({


      DF<-loadfile1()
      DF[input$FactorLists] <- sapply(DF[input$FactorLists],as.factor)
      DF[input$NumericLists] <- sapply(DF[input$NumericLists],as.numeric)
      DF


    })

    # Provide missing Summmary of the Dataset

    output$Summary = shiny::renderPrint(if(input$Go){

      #if(input$Go){
      DF<-DataTypeConversion()
      #print((DF))
      if(shiny::isolate(input$ExploreWays)==1){
        summary(DF)
      }else if(shiny::isolate(input$ExploreWays)==2){
        str(DF)
      }else if(shiny::isolate(input$ExploreWays)==3){
        #round(colMeans(is.na(DF))*100,2)[order(-round(colMeans(is.na(DF))*100,2))]
        sort(colMeans(is.na(DF))[colMeans(is.na(DF))>0]*100,decreasing = T)
      }else if(shiny::isolate(input$ExploreWays)==4){
        round(stats::cor(DF[,sapply(DF, is.numeric)], use="pairwise", method="pearson"),3)
      }else if(shiny::isolate(input$ExploreWays)==5){
        #sort(round(skewness(DF[,sapply(DF, is.numeric)]),3))
        t<-sapply(DF,is.numeric)
        sort(sapply(DF[,t], e1071::skewness),decreasing = T)
      }else if(isolate(input$ExploreWays)==6){
        #sort(round(kurtosis(DF[,sapply(DF, is.numeric)]),3))
        t<-sapply(DF,is.numeric)
        sort(sapply(DF[,t], e1071::kurtosis),decreasing = T)

      }else{}
      #}
    })


    # Provide missing Summmary of the Dataset

    output$Model = shiny::renderPrint(if(input$Go2){

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Processing is going on..Kindly wait")

      set.seed(7)
      trainControl <- caret::trainControl(method="cv", number=shiny::isolate(as.numeric(input$NumFolds)),classProbs = TRUE)

      #if(input$Go){
      DF<-loadfile3()
      index<-which(names(DF) %in% input$Target)
      Metric<-input$MetricType
      #print(index)
      #Target<-as.matrix(DF[,input$Target])
      #print(Target)
      #Features<-as.matrix(DF[,-c(index)])
      #Target<-input$Target
      #print(make.names(Target))
      #Features<-paste(c(colnames(DF[,-c(index)])),collapse=" + ")
      if (input$NumPredictor==1){
        Formula<-as.formula(paste(colnames(DF)[index], paste(colnames(DF)[-c(index)], sep = "",
                                                             collapse = " + "), sep = " ~ "))
      }else{
        Formula<-as.formula(paste(colnames(DF)[index], paste(isolate(input$Predictor), sep = "",
                                                             collapse = " + "), sep = " ~ "))
      }
      #print(Features)
      #print(loadfile1())
      #print((DF))
      if(isolate(input$Tuning)==1){

        if(isolate(input$MLT)==1){
          fit.linear<<- caret::train(Formula, data=DF, method="lm", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(LinearRegression=fit.linear,LinearRegression=fit.linear))
          #LFC$Linear<-fit.linear
          print(fit.linear)
          print(caret::varImp(fit.linear))
          summary(results)
        }else if(shiny::isolate(input$MLT)==2){
          loggrid<<-expand.grid(lambda=seq(shiny::isolate(as.numeric(input$RegTunAlphaStarts)),shiny::isolate(as.numeric(input$RegTunAlphaEnds)),shiny::isolate(as.numeric(input$RegTunAlphaSteps))),
                                alpha=seq(shiny::isolate(as.numeric(input$RegTunLambdaStarts)),shiny::isolate(as.numeric(input$RegTunLambdaEnds)),shiny::isolate(as.numeric(input$RegTunLambdaSteps))))

          fit.log<<- caret::train(Formula, data=DF, method="glm", metric="ROC",
                                  preProc=c("center", "scale"), trControl=trainControl,tuneGrid = loggrid)

          results <<- caret::resamples(list(Logistic=fit.log,Logistic=fit.log))
          #LFC$Logistic<<-fit.log

          print(fit.log)
          print(caret::varImp(fit.log))
          summary(results)

        }else if(shiny::isolate(input$MLT)==3 & shiny::isolate(input$TuningType)==2){
          reggrid<<-expand.grid(lambda=seq(shiny::isolate(as.numeric(input$RegTunAlphaStarts)),shiny::isolate(as.numeric(input$RegTunAlphaEnds)),shiny::isolate(as.numeric(input$RegTunAlphaSteps))),
                                alpha=seq(shiny::isolate(as.numeric(input$RegTunLambdaStarts)),shiny::isolate(as.numeric(input$RegTunLambdaEnds)),shiny::isolate(as.numeric(input$RegTunLambdaSteps))))

          fit.hybrid<<- caret::train(Formula, data=DF, method="glmnet", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl,tuneGrid = reggrid)
          results <<- caret::resamples(list(Regularized=fit.hybrid,Regularized=fit.hybrid))
          #LFC$Regularized<-fit.hybrid
          #print(LFC)
          print(fit.hybrid)
          print(caret::varImp(fit.hybrid))
          summary(results)
        }else if(shiny::isolate(input$MLT)==3 & shiny::isolate(input$TuningType)==1){
          fit.hybrid<<- caret::train(Formula, data=DF, method="glmnet", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl,tuneGrid = reggrid)
          results <<- caret::resamples(list(Regularized=fit.hybrid,Regularized=fit.hybrid))
          #LFC$Regularized<-fit.hybrid
          #print(LFC)
          print(fit.hybrid)
          print(caret::varImp(fit.hybrid))
          summary(results)
        }else if(shiny::isolate(input$MLT)==4 & shiny::isolate(input$TuningType)==2){
          dtgrid<<-expand.grid(cp=seq(shiny::isolate(as.numeric(input$DtTunCpStarts)),shiny::isolate(as.numeric(input$DtTunCpEnds)),shiny::isolate(as.numeric(input$DtTunCpSteps))))

          fit.rpart<<- caret::train(Formula, data=DF, method="rpart", metric=Metric,
                                    preProc=c("center", "scale"), trControl=trainControl,tuneGrid = dtgrid)
          results <<- caret::resamples(list(DecisionTree=fit.rpart,DecisionTree=fit.rpart))
          #LFC$DecisionTree<-fit.rpart
          print(fit.rpart)
          print(caret::varImp(fit.rpart))
          summary(results)

        }else if(shiny::isolate(input$MLT)==4 & shiny::isolate(input$TuningType)==1){
          fit.rpart<<- caret::train(Formula, data=DF, method="rpart", metric=Metric,
                                    preProc=c("center", "scale"), trControl=trainControl,tuneLength=shiny::isolate(as.numeric(input$TuneLengths)))
          results <<- caret::resamples(list(DecisionTree=fit.rpart,DecisionTree=fit.rpart))
          #LFC$DecisionTree<-fit.rpart
          print(fit.rpart)
          print(caret::varImp(fit.rpart))
          summary(results)

        }else if(shiny::isolate(input$MLT)==5 & shiny::isolate(input$TuningType)==2){
          rfgrid<<-expand.grid(mtry=seq(shiny::isolate(as.numeric(input$RFTunMtryStarts)),shiny::isolate(as.numeric(input$RFTunMtryEnds)),shiny::isolate(as.numeric(input$RFTunMtrySteps))))
          fit.rf<<- caret::train(Formula, data=DF, method="rf", metric=Metric,
                                 preProc=c("center", "scale"), trControl=trainControl,tuneGrid = rfgrid,importance=T)
          results <<- caret::resamples(list(RandomForest=fit.rf,RandomForest=fit.rf))
          #LFC$RandomForset<-fit.rf
          print(fit.rf)
          print(caret::varImp(fit.rf))
          summary(results)
        }else if(shiny::isolate(input$MLT)==5 & shiny::isolate(input$TuningType)==1){

          fit.rf<<- caret::train(Formula, data=DF, method="rf", metric=Metric,
                                 preProc=c("center", "scale"), trControl=trainControl,importance=T,tuneLength=shiny::isolate(as.numeric(input$TuneLengths)))
          results <<- caret::resamples(list(RandomForest=fit.rf,RandomForest=fit.rf))
          print(fit.rf)
          print(caret::varImp(fit.rf))
          summary(results)

        }else if(shiny::isolate(input$MLT)==6 & shiny::isolate(input$TuningType)==2){
          gbmgrid<<-expand.grid(interaction.depth=seq(shiny::isolate(as.numeric(input$GbmTunDepthStarts)),shiny::isolate(as.numeric(input$GbmTunDepthEnds)),shiny::isolate(as.numeric(input$GbmTunDepthSteps))),
                                n.trees=seq(shiny::isolate(as.numeric(input$GbmTunNtreeStarts)),shiny::isolate(as.numeric(input$GbmTunNtreeEnds)),shiny::isolate(as.numeric(input$GbmTunNtreeSteps))),
                                shrinkage=seq(shiny::isolate(as.numeric(input$GbmTunShrinkageStarts)),shiny::isolate(as.numeric(input$GbmTunShrinkageEnds)),shiny::isolate(as.numeric(input$GbmTunShrinkageSteps))),
                                n.minobsinnode=seq(shiny::isolate(as.numeric(input$GbmTunMinObsNodeStarts)),shiny::isolate(as.numeric(input$GbmTunMinObsNodeEnds)),shiny::isolate(as.numeric(input$GbmTunMinObsNodeSteps))))

          fit.gbm<<- caret::train(Formula, data=DF, method="gbm", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneGrid = gbmgrid)
          results <<- caret::resamples(list(GradientBoosting=fit.gbm,GradientBoosting=fit.gbm))
          print(fit.gbm)
          print(caret::varImp(fit.gbm))
          summary(results)
        }else if(shiny::isolate(input$MLT)==6 & shiny::isolate(input$TuningType)==1){
          fit.gbm<<- caret::train(Formula, data=DF, method="gbm", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneLength=shiny::isolate(as.numeric(input$TuneLengths)))
          results <<- caret::resamples(list(GradientBoosting=fit.gbm,GradientBoosting=fit.gbm))
          print(fit.gbm)
          print(caret::varImp(fit.gbm))
          summary(results)

        }else if(shiny::isolate(input$MLT)==7 & shiny::isolate(input$TuningType)==2){
          xgbgrid<<-expand.grid(lambda=seq(shiny::isolate(as.numeric(input$XgbTunAlphaStarts)),shiny::isolate(as.numeric(input$XgbTunAlphaEnds)),shiny::isolate(as.numeric(input$XgbTunAlphaSteps))),
                                alpha=seq(shiny::isolate(as.numeric(input$XgbTunLambdaStarts)),shiny::isolate(as.numeric(input$XgbTunLambdaEnds)),shiny::isolate(as.numeric(input$XgbTunLambdaSteps))),
                                eta=seq(shiny::isolate(as.numeric(input$XgbTunEtaStarts)),shiny::isolate(as.numeric(input$XgbTunEtaEnds)),shiny::isolate(as.numeric(input$XgbTunEtaSteps))),
                                nrounds=seq(shiny::isolate(as.numeric(input$XgbTunNroundStarts)),shiny::isolate(as.numeric(input$XgbTunNroundEnds)),shiny::isolate(as.numeric(input$XgbTunNroundSteps))))

          fit.xgb<<- caret::train(Formula, data=DF, method="xgbLinear", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneGrid = xgbgrid)
          results <<- caret::resamples(list(ExtremeGradientBoosting=fit.xgb,ExtremeGradientBoosting=fit.xgb))
          #LFC$ExtremeGradientBoosting<-fit.xgb
          print(fit.xgb)
          print(caret::varImp(fit.xgb))
          summary(results)
        }else if(shiny::isolate(input$MLT)==7 & shiny::isolate(input$TuningType)==1){
          fit.xgb<<- caret::train(Formula, data=DF, method="xgbLinear", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneLength=shiny::isolate(as.numeric(input$TuneLengths)))
          results <<- caret::resamples(list(ExtremeGradientBoosting=fit.xgb,ExtremeGradientBoosting=fit.xgb))
          #LFC$ExtremeGradientBoosting<-fit.xgb
          print(fit.xgb)
          print(caret::varImp(fit.xgb))
          summary(results)
        }else if(shiny::isolate(input$MLT)==8 & shiny::isolate(input$TuningType)==2){
          svmgrid<<-expand.grid(sigma=seq(shiny::isolate(as.numeric(input$SvmTunSigmaStarts)),shiny::isolate(as.numeric(input$SvmTunSigmaEnds)),shiny::isolate(as.numeric(input$SvmTunSigmaSteps))),
                                C=seq(shiny::isolate(as.numeric(input$SvmTunCStarts)),shiny::isolate(as.numeric(input$SvmTunCEnds)),shiny::isolate(as.numeric(input$SvmTunCSteps))))

          fit.svm<<- caret::train(Formula, data=DF, method="svmRadial", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneGrid = svmgrid)
          results <<- caret::resamples(list(SupportVectorMachine=fit.svm,SupportVectorMachine=fit.svm))
          #LFC$SupportVectorMachine<-fit.svm
          print(fit.svm)
          print(caret::varImp(fit.svm))
          summary(results)
        }else if(shiny::isolate(input$MLT)==8 & shiny::isolate(input$TuningType)==1){
          fit.svm<<- caret::train(Formula, data=DF, method="svmRadial", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl,tuneLength=shiny::isolate(as.numeric(input$TuneLengths)))
          results <<- caret::resamples(list(SupportVectorMachine=fit.svm,SupportVectorMachine=fit.svm))

          print(fit.svm)
          print(caret::varImp(fit.svm))
          summary(results)
        }else if(shiny::isolate(input$MLT)==9){

          if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){

            fit.linear<<- caret::train(Formula, data=DF, method="lm", metric=Metric,
                                       preProc=c("center", "scale"), trControl=trainControl)
          }else{

            fit.log<<- caret::train(Formula, data=DF, method="glm", metric="ROC",
                                    preProc=c("center", "scale"), trControl=trainControl)
          }

          fit.hybrid<<- caret::train(Formula, data=DF, method="glmnet", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl)

          fit.rpart<<- caret::train(Formula, data=DF, method="rpart", metric=Metric,
                                    preProc=c("center", "scale"), trControl=trainControl)

          fit.rf<<- caret::train(Formula, data=DF, method="rf", metric=Metric,
                                 preProc=c("center", "scale"), trControl=trainControl)

          fit.gbm<<- caret::train(Formula, data=DF, method="gbm", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          fit.xgb<<- caret::train(Formula, data=DF, method="xgbLinear", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          fit.svm<<- caret::train(Formula, data=DF, method="svmRadial", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
            results <<- caret::resamples(list(Linear=fit.linear,DecisionTree=fit.rf,RandomForest=fit.rf,ExtremeGradientBoosting=fit.xgb,Regularized=fit.glmnet,GradientBoosting=fit.gbm,SupportVectorMachine=fit.svm))
          }else{
            results <<- caret::resamples(list(Logistic=fit.log,DecisionTree=fit.rf,RandomForest=fit.rf,ExtremeGradientBoosting=fit.xgb,Regularized=fit.glmnet,GradientBoosting=fit.gbm,SupportVectorMachine=fit.svm))
          }
          summary(results)


        }else{}
      }else{
        if(isolate(input$MLT)==1){
          fit.linear<<- caret::train(Formula, data=DF, method="lm", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(LinearRegression=fit.linear,LinearRegression=fit.linear))
          #LFC$Linear<-fit.linear
          print(fit.linear)
          print(caret::varImp(fit.linear))
          summary(results)
        }else if(shiny::isolate(input$MLT)==2){
          fit.log<<- caret::train(Formula, data=DF, method="glm", metric="ROC",
                                  preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(Logistic=fit.log,Logistic=fit.log))
          #LFC$Logistic<-fit.log

          print(fit.log)
          print(caret::varImp(fit.log))
          summary(results)

        }else if(shiny::isolate(input$MLT)==3){
          fit.hybrid<<- caret::train(Formula, data=DF, method="glmnet", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(Regularized=fit.hybrid,Regularized=fit.hybrid))
          print(fit.hybrid)
          print(caret::varImp(fit.hybrid))
          summary(results)

        }else if(shiny::isolate(input$MLT)==4){
          fit.rpart<<- caret::train(Formula, data=DF, method="rpart", metric=Metric,
                                    preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(DecisionTree=fit.rpart,DecisionTree=fit.rpart))
          print(fit.rpart)
          print(caret::varImp(fit.rpart))
          summary(results)
        }else if(shiny::isolate(input$MLT)==5){
          fit.rf<<- caret::train(Formula, data=DF, method="rf", metric=Metric,
                                 preProc=c("center", "scale"), trControl=trainControl,importance=T)

          results <- caret::resamples(list(RandomForest=fit.rf,RandomForest=fit.rf))
          print(fit.rf)
          print(caret::varImp(fit.rf))
          summary(results)
        }else if(shiny::isolate(input$MLT)==6){
          fit.gbm<<- caret::train(Formula, data=DF, method="gbm", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          results <- caret::resamples(list(GradientBoosting=fit.gbm,GradientBoosting=fit.gbm))
          print(fit.gbm)
          print(caret::varImp(fit.gbm))
          summary(results)
        }else if(shiny::isolate(input$MLT)==7){
          fit.xgb<<- caret::train(Formula, data=DF, method="xgbLinear", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(ExtremeGradientBoosting=fit.xgb,ExtremeGradientBoosting=fit.xgb))
          print(fit.xgb)
          print(caret::varImp(fit.xgb))
          summary(results)
        }else if(shiny::isolate(input$MLT)==8){
          fit.svm<<- caret::train(Formula, data=DF, method="svmRadial", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          results <<- caret::resamples(list(SupportVectorMachine=fit.svm,SupportVectorMachine=fit.svm))
          print(fit.svm)
          print(caret::varImp(fit.svm))
          summary(results)
        }else if(shiny::isolate(input$MLT)==9){

          if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){

            fit.linear<<- caret::train(Formula, data=DF, method="lm", metric=Metric,
                                       preProc=c("center", "scale"), trControl=trainControl)
          }else{

            fit.log<<- caret::train(Formula, data=DF, method="glm", metric="ROC",
                                    preProc=c("center", "scale"), trControl=trainControl)
          }

          fit.hybrid<<- caret::train(Formula, data=DF, method="glmnet", metric=Metric,
                                     preProc=c("center", "scale"), trControl=trainControl)

          fit.rpart<<- caret::train(Formula, data=DF, method="rpart", metric=Metric,
                                    preProc=c("center", "scale"), trControl=trainControl)

          fit.rf<<- caret::train(Formula, data=DF, method="rf", metric=Metric,
                                 preProc=c("center", "scale"), trControl=trainControl)

          fit.gbm<<- caret::train(Formula, data=DF, method="gbm", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          fit.xgb<<- caret::train(Formula, data=DF, method="xgbLinear", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          fit.svm<<- caret::train(Formula, data=DF, method="svmRadial", metric=Metric,
                                  preProc=c("center", "scale"), trControl=trainControl)

          if(is.numeric(DF[,input$Target]) | is.integer(DF[,input$Target])){
            results <<- caret::resamples(list(Linear=fit.linear,DecisionTree=fit.rf,RandomForest=fit.rf,ExtremeGradientBoosting=fit.xgb,Regularized=fit.hybrid,GradientBoosting=fit.gbm,SupportVectorMachine=fit.svm))
          }else{
            results <<- caret::resamples(list(Logistic=fit.log,DecisionTree=fit.rf,RandomForest=fit.rf,ExtremeGradientBoosting=fit.xgb,Regularized=fit.hybrid,GradientBoosting=fit.gbm,SupportVectorMachine=fit.svm))
          }
          summary(results)


        }else{}
      }

    })


    # Provide Do's and Dont's in file upload

    output$TestUploadRules = shiny::renderText({
      print(paste("Guidelines:","1. Test File is clean with no missing values",
                  "2. Number of columns should be equal to one less in Number of columns
                  in training dataset", "3. Column Names should Match","5. Same Transformations applied across Training and Test DataSet",
                  "6. Algorithm has been processed in previous step","If any of the above assumption is failed then
                  Model will not run or predictions will not be correct",sep="\n"))
    })


    # Provide guidelines to upload the test dataset

    output$PredictModel = shiny::renderDataTable(if(input$Go3){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Processing is going on..Kindly wait")

      DFTest<<-loadfile2()
      #print(fit.hybrid)
      if(shiny::isolate(input$PredictMLT)==1){
        DFTest$Prediction<-kernlab::predict(fit.linear,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==2){
        DFTest$Prediction<-kernlab::predict(fit.log,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==3){
        DFTest$Prediction<-kernlab::predict(fit.hybrid,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==4){
        DFTest$Prediction<-kernlab::predict(fit.rpart,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==5){
        DFTest$Prediction<-kernlab::predict(fit.rf,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==6){
        DFTest$Prediction<-kernlab::predict(fit.gbm,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==7){
        DFTest$Prediction<-kernlab::predict(fit.xgb,DFTest)
      }else if(shiny::isolate(input$PredictMLT)==8){
        DFTest$Prediction<-kernlab::predict(fit.svm,DFTest)
      }else{}
      write.csv(DFTest,"Prediction.csv")
      DFTest
      #print("Prediction has been and done and file Prediction.csv has been exported")
    },options = list(lengthMenu = c(5, 10, 15), pageLength = 5,scrollX=T))

    output$downloadPlot <- shiny::downloadHandler(
      filename = "Shinyplot.png",
      content = function(file) {
        png(file)
        Exportplot()
        dev.off()
      })

    # Provide guidelines to upload the train dataset during model development

    output$GuideTrain = shiny::renderText({
      print(paste("Note:1.There should not be missing data in the file",
                  "2. This is because few Models does not accept missing values",sep="\n"))
    })

    # Missing Pattern Plot
    output$MissingPattern<-shiny::renderPlot({
      DF<-DataTypeConversion()
      #if(input$Go & isolate(input$ExploreWays)==3){
      VIM::aggr(DF, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
      #}
    })

    # Correlation Plot
    output$CorPlot<-shiny::renderPlot(if(input$Go1){
      DF<-DataTypeConversion()
      #if(input$Go & isolate(input$ExploreWays)==4){

      #corrplot(loadfile1()[, sapply(loadfile1(), is.numeric)], method="number")
      M <- stats::cor(na.omit(DF[, sapply(DF, is.numeric)]))
      #corrplot(M, method="number")


      # correlations
      row_indic <- apply(M, 1, function(x) sum(x > input$CorRange[2] | x < input$CorRange[1]) > 1)

      correlations<- M[row_indic ,row_indic ]
      corrplot::corrplot(correlations, method="number")

      #}
    })

    # Scatter Plot All Pairs
    output$ScatterAllPairs<-shiny::renderPlot({
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Plot will be displayed..Kindly wait")

      DF<-DataTypeConversion()
      #if(input$Go & isolate(input$ExploreWays)==5){

      graphics::pairs(DF[, sapply(DF, is.numeric)],
                      main="Simple Scatterplot Matrix")
      #}
    })


    # Scatter Plot Single Pairs
    output$ScatterSinglePair<-plotly::renderPlotly(if(input$PlotType=="Scatter"){
      DF<-DataTypeConversion()

      ggplot2::ggplot(DF, ggplot2::aes_string(x=input$Xaxis, y=input$Yaxis)) +
        ggplot2::geom_jitter(size=2)+ ggplot2::xlab(paste(input$Xaxis))+ggplot2::ylab(input$Yaxis)+ggplot2::geom_smooth(method = lm )+
        ggplot2::ggtitle(paste(input$Xaxis," Vs ",input$Yaxis))+ggplot2::theme(plot.title = element_text(size = 15, face = "bold"),axis.title = element_text(face="bold",size=12),
                                                             axis.text.x  = element_text(vjust=0.5, size=10,face="bold"),axis.text.y  = element_text(size=10,face="bold"),legend.text=element_text(size=12))

    })


    # Histogram
    output$Hist<-plotly::renderPlotly(if(input$PlotType=="Histogram"){
      DF<-DataTypeConversion()

      H <- hist(DF[,input$HistParam], plot = FALSE)
      minimum<-min(H$breaks,na.rm=TRUE)
      maximum<-max(H$breaks,na.rm=TRUE)
      step<-H$breaks[2]-H$breaks[1]

      ggplot2::ggplot(DF,ggplot2::aes_string(x=input$HistParam)) +
        ggplot2::stat_bin(binwidth=step,colour="blue",fill="pink") +
        ggplot2::stat_bin(binwidth=step, geom="text", ggplot2::aes(label=scales::percent((..count../sum(..count..)))), vjust=-1.5)+
        ggplot2::scale_x_continuous(breaks=seq(minimum,maximum, by=step))+theme_bw()



    })

    # Box Plot
    output$BoxPlot<-plotly::renderPlotly(if(input$PlotType=="Box"){
      DF<-DataTypeConversion()

      #     H <- hist(DF[,input$HistParam], plot = FALSE)
      #     minimum<-min(H$breaks,na.rm=TRUE)
      #     maximum<-max(H$breaks,na.rm=TRUE)
      #     step<-H$breaks[2]-H$breaks[1]

      ggplot2::ggplot(DF,ggplot2::aes_string(x=input$GrByBoxs,y=input$BoxParam,fill=input$GrByBoxs)) +ggplot2::geom_boxplot()+ggplot2::theme_bw()



    })


    # tabPlot
    output$tabPlot<-shiny::renderPlot(if(input$Go1 & input$PlotType=="Tabular"){

      DF<-DataTypeConversion()
      tabplot::tableplot(DF,select_string =shiny::isolate(input$SelectTabPlot),sortCol=shiny::isolate(input$SortTabPlot),from=as.numeric(shiny::isolate(input$FilterTabPlot[1])),to=as.numeric(shiny::isolate(input$FilterTabPlot[2])))


    })

    # Mosaic Plot
    output$MosaicPlot<-shiny::renderPlot(if(input$PlotType=="Mosaic"){

      DF<-DataTypeConversion()
      x<-ffbase::table(DF[,input$Mosaic1st],DF[,input$Mosaic2nd])
      graphics::mosaicplot(x,shade=T,legend=T,xlab = input$Mosaic1st, ylab = input$Mosaic2nd,main ="")


    })

    # Association Plot
    output$AssocPlot<-renderPlot(if(input$PlotType=="Mosaic"){

      DF<-DataTypeConversion()
      x<-ffbase::table(DF[,input$Mosaic1st],DF[,input$Mosaic2nd])
      vcd::assoc(x,xlab = input$Mosaic1st,ylab = input$Mosaic2nd,main ="",shade=T)


    })



    # tabPlot
    output$Plots<- shiny::renderUI({

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Processing is going on..Kindly wait")

      if(input$PlotType=="Box"){
        shinydashboard::box(shiny::uiOutput("BoxParams"),
                            shiny::uiOutput("GrByBox"),
                            #fluidRow(column(6,actionButton("Go1", "Plot"))),
                            plotly::plotlyOutput("BoxPlot",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Histogram"){
        shinydashboard::box(shiny::uiOutput("HistParams"),
                            #fluidRow(column(6,actionButton("Go1", "Plot"))),
                            plotly::plotlyOutput("Hist",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Scatter"){
        shinydashboard::box(shiny::uiOutput("XaxisTypes"),
                            shiny::uiOutput("YaxisTypes"),
                            #fluidRow(column(6,actionButton("Go1", "Plot"))),
                            plotly::plotlyOutput("ScatterSinglePair",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Tabular"){
        shinydashboard::box(shiny::uiOutput("SelectTabPlots"),
                            shiny::uiOutput("SortTabPlots"),
                            shiny::uiOutput("FilterTabPlots"),
                            shiny::fluidRow(shiny::column(6,actionButton("Go1", "Plot"))),
                            shiny::plotOutput("tabPlot",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Scatter Matrix"){
        shinydashboard::box(
          #fluidRow(column(6,actionButton("Go1", "Plot"))),
          shiny::fluidRow(shiny::plotOutput("ScatterAllPairs",height=1000,width=1000)),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Correlation"){
        shinydashboard::box(shiny::uiOutput("CorRanges"),
                            shiny::fluidRow(shiny::column(6,actionButton("Go1", "Plot"))),
                            shiny::plotOutput("CorPlot",height=1000,width=1000),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Missing"){
        shinydashboard::box(
          #fluidRow(column(6,actionButton("Go1", "Plot"))),
          shiny::plotOutput("MissingPattern",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Mosaic"){
        shinydashboard::box(shiny::uiOutput("MosaicFirst"),
                            shiny::uiOutput("MosaicSecond"),
                            #fluidRow(column(6,actionButton("Go1", "Plot"))),
                            shiny::plotOutput("MosaicPlot",height=500,width=1200),
                            shiny::plotOutput("AssocPlot",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else {}
    })


    })




  # ui part


  ui<-shinydashboard::dashboardPage(skin = 'purple',
                                    shinydashboard::dashboardHeader(title = shiny::h4(shiny::HTML("Complete Exploratory Data Analysis and Machine Learning")),titleWidth=430),
                                    shinydashboard::dashboardSidebar(disable=TRUE,


                                                                     shiny::tags$style(shiny::HTML("
                                                                                            .sidebar { height: 110vh; overflow-y: auto; }
                                                                                            " ))




                                                                     ),

                                    shinydashboard::dashboardBody(shinyjs::useShinyjs(),
                                                                  # Refreshing Shint APP
                                                                  shinyjs::extendShinyjs(text = jsResetCode),
                                                                  # Hiding Errors
                                                                  shiny::tags$style(
                                                                    type="text/css",
                                                                    ".shiny-output-error { visibility: hidden; }",
                                                                    ".shiny-output-error:before { visibility: hidden; }"),
                                                                  #                 tags$head(
                                                                  #                   tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell;  } .form-group { display: table-row;}")
                                                                  #                 ),
                                                                  #tags$script(HTML(jk)),

                                                                  #uiOutput("Box1"),
                                                                  shiny::tabsetPanel(
                                                                    shiny::tabPanel("Data",shiny::fluidRow(shiny::column(6,shiny::isolate(shiny::uiOutput("FileUpload1"))))),
                                                                    #column(6,isolate(uiOutput("FileUpload2"))))),
                                                                    shiny::tabPanel("Explore",shiny::uiOutput("ExploreWay"),
                                                                             #column(4,uiOutput("FactorList")),
                                                                             #column(4,uiOutput("NumericList"))),
                                                                             shiny::fluidRow(shiny::column(6,shiny::actionButton("Go", "Process"))),
                                                                             shiny::verbatimTextOutput("Summary")),
                                                                    shiny::tabPanel("Plots",
                                                                                    shiny::uiOutput("PlotTypes"),
                                                                                    shiny::uiOutput("Plots")),
                                                                    shiny::tabPanel("Run and Tune Model",
                                                                                    shiny::fluidRow(shiny::column(4,shinydashboard::box(
                                                                                      shiny::verbatimTextOutput("GuideTrain"),
                                                                                      shiny::uiOutput("FileUpload3"),
                                                                                      shiny::uiOutput("Targets"),
                                                                                      shiny::uiOutput("NumPredictors"),
                                                                                      shiny::uiOutput("Predictors"),
                                                                                      shiny::uiOutput("MetricTypes"),
                                                                                      shiny::uiOutput("NumFolds"),
                                                                                      shiny::uiOutput("MLTS"),
                                                                                      shiny::uiOutput("Tunings"),
                                                                                      shiny::uiOutput("TuningTypes"),
                                                                                      shiny::uiOutput("TuneLength"),
                                                                                      shiny::actionButton("Go2", "Process"),width=10)),
                                                                                      shiny::uiOutput("RegTun"),
                                                                                      shiny::uiOutput("DtTun"),
                                                                                      shiny::uiOutput("RFTun"),
                                                                                      shiny::uiOutput("XgbTun"),
                                                                                      shiny::uiOutput("GbmTun"),
                                                                                      shiny::uiOutput("SvmTun")),
                                                                                    shiny::verbatimTextOutput("Model")),
                                                                    shiny::tabPanel("Prediction",
                                                                                    shiny::verbatimTextOutput("TestUploadRules"),
                                                                                    shiny::uiOutput("FileUpload2"),
                                                                                    shiny::uiOutput("PredictMLTS"),
                                                                                    shiny::actionButton("Go3", "Predict"),
                                                                                    shiny::dataTableOutput("PredictModel")))

                                    )
                                                                     )



  shiny::shinyApp(ui = ui, server = server)

}

