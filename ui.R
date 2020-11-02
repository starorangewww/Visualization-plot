library(shiny)
library(rlang)
library(httpuv)
library(Rcpp)
library(rsconnect)
library(varhandle)
#library(ropls) bio
library(car)
library(ggplot2)
library(e1071)
library(neuralnet) 
library(nnet)     
library(caret)
library(randomForest)

shinyUI(
  fluidPage(
    titlePanel(h3("Visualization plot of statistically processed")),sidebarLayout(
      sidebarPanel(
        fileInput("file",label="File input"),
        textInput("IS",label = "Feature of internal standard",value="38"),
        textInput("PD",label = "Feature of parent drug",value="5"),
        textInput("G.S",label = "Group size",value="6"),
        textInput("R.S",label="Replicated size", value = "5"),
        div("--------------------------------------------------"),
        selectInput("Ty", 
                    label = h4("Type of vizualization"),
                    choices = list("Univariate:Parametric",
                                   "Univariate:Non-parametric",
                                   "Multivairate:Importance wetight"),
                    selected = "Type"),
        div("--------------------------------------------------"),
        selectInput("Cor.1",label = "Type of correlation",choices = list("pearson","spearman","kendall")),
        textInput("Cor",label = "Cutting point of y,99:No cut point",value="99"),
        selectInput("Method",label="Cutting point of x,Null:No cut point",
                    choices = list("Null",
                                   "Bonferroni's adjustment",
                                   "BH_FDR",
                                   "Logistic regression",
                                   "Support vector machine",
                                   "Arifical neural network",
                                   "Random forest"),
                    selected = "Type"),
        
        h4("Carry out"),
        actionButton("goButton","Draw")
      )
      ,mainPanel(h3("Visualization plot"),
                 plotOutput("distPlot.1"),
                 h3("Cutting point of x-axis"),
                 textOutput("XY"),
                 h3("Significant features"),
                 tableOutput("table.1"))
    ))
)
