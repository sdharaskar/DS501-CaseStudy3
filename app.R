library(shiny)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(CGPfunctions)
# load libraries
library(readr)



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
    ####  Panel: Main>Summary             ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Stroke Predictor",
        ################################################
        #### Panel: Main>Summary>Tables & Pie Chart ####
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
                        value = 30),
            
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
      #### Panel: Main>Plots                      ####
      ################################################
      
      tabPanel(
        "Data Visualizations",
        
        # --------------------
        # Bar Charts
        # --------------------

        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel"),
            tags$br(),
            radioButtons(
              "categorical",
              label = "Select predictor",
              choices = list(
                "Gender" = "gender",
                "Ever Married" = "ever_married",
                "Work Type" = "work_type",
                "Residense Type" = "Residence_type",
                "Smoking Status" = "smoking_status"
                
              ),
              selected = "smoking_status"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Median Income by School (aggregate)"),
            plotOutput(outputId = "barchart1"),
            tags$br(),
            plotOutput(outputId = "barchart2"),
            tags$br(),
            plotOutput(outputId = "barchart3"),
            tags$br(),
            plotOutput(outputId = "barchart4"),
            tags$br(),
            plotOutput(outputId = "barchart5"),
            tags$br()
          )
        ),
        
        # --------------------
        # Conditional Density Charts
        # --------------------        
        
        sidebarLayout(
          sidebarPanel(
            h3("Density Plot Panel"),
            tags$br(),

            checkboxGroupInput(
              "continuous",
              label = "Select University",
              choices = list(
                "Body Mass Index" = "bmi",
                "Anerage Glucose Level" = "avg_glucose_level"
              ),
              selected = list(
                "Body Mass Index" = "bmi",
                "Anerage Glucose Level" = "avg_glucose_level"
              )
            ),
          ),
          mainPanel(
            h3("Distribution"),
            plotOutput(outputId = "densityPlot1"),
            tags$br(),
            plotOutput(outputId = "densityPlot2"),
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



# Define server logic 



server <- function(input, output) {
  # read the data
  stroke <- read_csv("stroke.csv")
  head(stroke)
  strokeData = stroke[,2:12]
  head(strokeData)
  
  # Clean the data
  
  strokeData[strokeData == "N/A"] <- NA
  strokeData <- na.omit(strokeData)
  
  strokeData$gender <- as.factor(strokeData$gender)
  strokeData$hypertension <- as.factor(strokeData$hypertension)
  strokeData$heart_disease <- as.factor(strokeData$heart_disease)
  strokeData$ever_married <- as.factor(strokeData$ever_married)
  strokeData$work_type <- as.factor(strokeData$work_type)
  strokeData$Residence_type <- as.factor(strokeData$Residence_type)
  strokeData$smoking_status <- as.factor(strokeData$smoking_status)
  strokeData$bmi <- as.numeric(strokeData$bmi)

      output$predictText <- renderText({


        # display table of the prediction
        strokeLogregModel = glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level + bmi, 
                           data=strokeData, 
                           family=binomial(link="logit"))
        
        newData <- data.frame(age = input$age, 
                              hypertension = input$Hypertension, 
                              heart_disease = input$HeartDisease,
                              avg_glucose_level = input$avgGlucLvl,
                              bmi = input$bmi)
        
        strokeProb <- predict(strokeLogregModel,
                          newData,
                          type="response")
        
        strokeProb
        
        predicted.stroke <- ifelse(strokeProb > 0.2, "You are at a higher risk to get stroke."
                                   , "You are not at a risk to get stroke.")

    })
    
    # --------------------
    # bar plot section
    # --------------------
    
    
    

    # selectedCatVariable
    output$barchart1 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(1), "percent")
      })
    

    output$barchart2 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(5), "percent")
    })

    output$barchart3 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(6), "percent")
    })

    output$barchart4 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(3), "percent")
    })

    output$barchart5 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(4), "percent")
    })
    
    # --------------------
    # density plot section
    # --------------------
    
    # filter the checkgroup input:
    
    dent <-  reactive({
      return(dent = input$continuous)
      
    })
    
    # render density plot1
    
    output$densityPlot1 <- renderPlot({
      cdplot(factor(stroke) ~ bmi, data=strokeData, main="Estimated categ prob", ylab='Stroke')
      qplot(bmi, ..count.., data=strokeData, geom="density", fill=factor(stroke), position="fill") + 
        ylab('Probability')+theme(legend.position='bottom')
      
    })
    
    # render density plot2
    
    output$densityPlot2 <- renderPlot({
      cdplot(factor(stroke) ~ avg_glucose_level, data=strokeData, main="Estimated categ prob", ylab='Stroke')
      qplot(avg_glucose_level, ..count.., data=strokeData, geom="density", fill=factor(stroke), position="fill") + 
        ylab('Probability')+theme(legend.position='bottom')
      
    })   
    
    
    ################################################
    #### Panel: Documentation                   ####
    ################################################
    
    getReportPage <- function() {
      return(includeHTML("rmarkdown-report.html"))
    }
    output$report <- renderUI({
      getReportPage()
    })
    
    
    ################################################
    #### Panel: About                           ####
    ################################################
    
    getAboutPage <- function() {
      return(includeHTML("about.html"))
    }
    output$about <- renderUI({
      getAboutPage()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
