library(shiny)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(CGPfunctions)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Stroke Prediction",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "strokeprediction",
      img(src = "stroke-banner.jpg", style ="height: 200px"),
    )),
    
    tags$br(),
    
    
    ##########################################
    ####  Panel: Main>Stroke Predictor           ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Stroke Predictor",
        ################################################
        #### Panel: Main>Stroke Predictor>Prediction ####
        ################################################
        
        # ------------------
        # ranking $ pie chart section
        # ------------------
        
        sidebarLayout(
          sidebarPanel(
            sliderInput("age",
                        "Age in years:",
                        min = 10,
                        max = 100,
                        value = 78),
            
            radioButtons("Hypertension",
                         label = strong("Suffer from Hypertension ?"),
                         choices = list("Yes" = 1, "No" = 0),
                         selected = 0,
                         inline = TRUE),
            
            radioButtons("HeartDisease",
                         label = strong("Suffer from Heart Disease ?"),
                         choices = list("Yes" = 1, "No" = 0),
                         selected = 0,
                         inline = TRUE),   
            
            numericInput("avgGlucLvl", "Average Glucose Level:", 130,
                         min = 80, max = 500, width = '400px'),
            
            numericInput("bmi", "Body Mass Index:", 32,
                         min = 10, max = 50, width = '400px')
            
            #numericInput(inputId, label, value, min = NA, max = NA, step = NA,
            #width = NULL)
            
            
          ), # end side bar layout
          
          # Main Panel --- start
          mainPanel(
            # tab1 --- start
            tabsetPanel(
              tabPanel("Prediction", 
                       strong("Predicted Output: "),
                       verbatimTextOutput("predictText")
              )
              
              
            ) # tab1 --- end
            
          ) # tabset panel --- end
          # main panel page ---end 
        ), # end side bar
        tags$hr(),
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Main> Data Visualizations                     ####
      ################################################
      
      tabPanel(
        "Data Visualizations",
        
        # --------------------
        # Bar Charts
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plots"),
            p("We use Bar Plot(cross correlation /table) to visualize data for categorical variables."),
            p("Gender - the gender does not 
              have much impact on the risk of stroke.")   ,
            p("Ever Married - the 
              the risk of stroke is higher in the married population.")   ,
            p("Work Type - the self-employed
              workers have more risk of stroke than government and private
              employees. The non-working population seems to have no risk. 
              This tells us that the risk might be related to the stress at work"),
            p("Residense Type - the risk of stroke is same irrespective of 
              the residense type."),
            p("Smoking Status - the data does not show direct correlation between
              smoking and risk of stroke"),
            p("Blood Pressure - the data shows that people with hypertension are at higher risk"),
            p("Heart Disease - the data also shows that people with heart
              disease are at higher risk to suffer stroke"),
            tags$br(),
            tags$hr()
          ),
          mainPanel(
            h3("Exploratory Analysis of conditions"),
            plotOutput(outputId = "barchart1"),
            tags$br(),
            plotOutput(outputId = "barchart2"),
            tags$br(),
            plotOutput(outputId = "barchart3"),
            tags$br(),
            plotOutput(outputId = "barchart4"),
            tags$br(),
            plotOutput(outputId = "barchart5"),
            tags$br(),
            plotOutput(outputId = "barchart6"),
            tags$br(),
            plotOutput(outputId = "barchart7"),
            tags$br()
          )
        ),
        
        # --------------------
        # Conditional Density Charts
        # --------------------        
        
        sidebarLayout(
          sidebarPanel(
            h3("Density Plots"),
            tags$br(),
            p("We use Conditional Density Plots
              to visualize data for te continuous variables."),
            p("Body Mass Index - the data shows that people
              with bmi 25 -35 are at higher risk. However it also
              shows a lot of results with no stroke")   ,
            p("Average Glucose Level - the average glucose level does 
              not seem to have much impact on the risk to suffer stroke.")   ,
            p("Age - the data shows that the risk of stroke
              increases beyond the age of 40.")   ,
            tags$br(),
          ),
          mainPanel(
            h3("Conditional Density Plots"),
            plotOutput(outputId = "densityPlot1"),
            tags$br(),
            plotOutput(outputId = "densityPlot2"),
            tags$br(),
            plotOutput(outputId = "densityPlot3"),
            tags$br()
          )
        )
        
        
        
        
      )
    )
  ),
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  tabPanel("Documentation",
           fluidPage(htmlOutput("report"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About",
           fluidPage(htmlOutput("about")))
)

