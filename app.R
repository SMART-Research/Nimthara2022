#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GGally)
library(DT)
library(tidyverse)
library(visdat)
library(corrplot)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("readable"),

    # Application title
    titlePanel("Exploratory Data Analysis"),

  
    sidebarLayout(
        sidebarPanel(
            fileInput('file', 'Upload the tidy data file (.csv)',
                      multiple =TRUE,
                      accept = c('.csv')),
          
            uiOutput('select1'),uiOutput("select2"),uiOutput("select3"),

            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        
        mainPanel(
          tabsetPanel(type="tab",
                      tabPanel("Data",DT::dataTableOutput('contents')),
                      tabPanel("Overview",verbatimTextOutput("Glimpse"),
                               verbatimTextOutput("summary"),plotOutput("structureplot",height="200px")),
                      tabPanel("Correlations",plotOutput("corr",height = "500px",width = "100%")
                                ),
                      tabPanel("One Way",
                               fluidRow(
                                   splitLayout(cellWidths = c("50%", "50%"),plotOutput("hist"),plotOutput("barplot1"))
                               )),
                      tabPanel("Two Way",
                            fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),plotOutput("scatterplot1"),plotOutput("barplot2"))
                            )   
                               ),
                      tabPanel("Three Way",plotOutput("scatterplot2")))
        )
    )
)

server <- function(input, output) {

    data <- eventReactive(input$file,{
        data <- read.csv(input$file$datapath)
    })
    

    output$contents <- DT::renderDataTable({
        DT::datatable(data())       
    })
    
    output$Glimpse<-renderPrint({
        glimpse(data())
    })
    output$summary <- renderPrint({
        summary(data())
    })
    
   output$structureplot<-renderPlot({
       vis_dat(data())
   })
  
  output$corr<-renderPlot({
      ggcorr(data(),label=TRUE)
  })
  
  
  output$select1 <- renderUI({
      data<-data()
      numeric_variables<-select_if(data,is.numeric)
      selectInput("x", "Select First Numerical Variable:",names(numeric_variables))
  })
  
  output$select2 <- renderUI({
      data<-data()
      factor_variables<-select_if(data,is.character)
      selectInput("y", "Select Factor Variable:",names(factor_variables))
  })
  
  output$select3 <- renderUI({
      data<-data()
      numeric_variables<-select_if(data,is.numeric)
      selectInput("z", "Select Second Numerical Variable:",names(numeric_variables))
  })
  
  output$hist<-renderPlot({
      data<-data()
      data<-data[,input$x]
      bins <- seq(min(data), max(data), length.out = input$bins + 1)
      hist(data,breaks = bins,col = "light blue",main = "One Quantitative Variable")
  })
  
  output$barplot1<-renderPlot({
     data<-data()
     ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$y]))+geom_bar(stat="count",fill="steelblue")+
         theme_minimal()+labs(title="One Qualitative Variable")
  
  })
  
  output$scatterplot1<-renderPlot({
      data<-data()
      ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$x],y=colnames(data)[colnames(data)==input$z]))+geom_point()+stat_smooth(method = "lm", se = T, col = "red") +labs(title = "Relationship Between Two Quantitative Variables")
  })
  
  
  output$barplot2<-renderPlot({
      data<-data()
      ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$y],y=colnames(data)[colnames(data)==input$z]))+
          geom_col(fill="#00AFBB")+labs(title="Quantitative Variable by Qualitative Variable")
    
  })
  
 output$scatterplot2<-renderPlot({
     data<-data()
     ggplot(data,aes_string(x=colnames(data)[colnames(data)==input$x],y=colnames(data)[colnames(data)==input$z],col=colnames(data)[colnames(data)==input$y]))+
         geom_point()+labs(title="Two Quantitative Variables by a Qualitative Variable")
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
