library(shiny)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(CGPfunctions)
# load libraries

# read the global file for data
source('global.R')


# Define server logic 



server <- function(session, input, output) {


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
      PlotXTabs(strokeData, stroke, c(7), "percent")
    })

    output$barchart5 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(10), "percent")
    })
    
    output$barchart6 <- renderPlot({
      PlotXTabs(strokeData, stroke, c(3), "percent")
    })
    
    output$barchart7 <- renderPlot({
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
    
    # render density plot3
    
    output$densityPlot3 <- renderPlot({
      cdplot(factor(stroke) ~ age, data=strokeData, main="Estimated categ prob", ylab='Stroke')
      qplot(age, ..count.., data=strokeData, geom="density", fill=factor(stroke), position="fill") + 
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


