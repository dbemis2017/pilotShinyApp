library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

d <- read.csv("BlindedData.csv")
d <- data.frame(d)
d$AbsScore <- NA
d$AbsScore <- 5*d$X + 5*d$Y + 5*d$Z

# Define UI for app
ui <- fluidPage(
  
  titlePanel("Combining Assay Outputs to Capture Subjectivity in Sample Consideration"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input
      sliderInput(inputId = "rating1",
                  label = "Weight for X:",
                  min = 0,
                  max = 10,
                  value = 5),
      sliderInput(inputId = "rating2",
                  label = "Weight for Y:",
                  min = 0,
                  max = 10,
                  value = 5),
      sliderInput(inputId = "rating3",
                  label = "Weight for Z:",
                  min = 0,
                  max = 10,
                  value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    w1 <- input$rating1 / (input$rating1 + input$rating2 + input$rating3)
    w2 <- input$rating2 / (input$rating1 + input$rating2 + input$rating3)
    w3 <- input$rating3 / (input$rating1 + input$rating2 + input$rating3)
    
    d$AbsScore <- NA
    d$AbsScore <- w1*d$X + w2*d$Y + w3*d$Z
    
    ggplot(d, aes(Group.1, AbsScore))+
      geom_col()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
}

shinyApp(ui = ui, server = server)