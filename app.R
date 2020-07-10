library(shiny)
library(ggplot2)
library(scales)
library(gridExtra)
source("SDM_GPPC.R")

# Setting up the client (page elements appear in order specified)
ui <- fluidPage(
  titlePanel("System Dynamics Modeling: The Health Sector Model"),
  a("See GitHub for resources", href="https://github.com/MaguireMaName/SDM/",target="_blank"),
  br(),br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sPopulation0_14", "0 to 14", min=0, max=5000000, value=1000000, step=50000),
      sliderInput("sPopulation15_39", "15 to 39", min=0, max=5000000, value=1500000, step=50000),
      sliderInput("sPopulation40_64",  "40 to 64", min=0, max=5000000, value=2000000, step=50000),
      sliderInput("sPopulation65_plus",  "65 plus", min=0, max=5000000, value=500000, step=25000),
      sliderInput("aCrudeBirthRate",  "Birth rate", min=0, max=0.1, value=0.02, step=0.01),
      sliderInput("aCrudeDeathRate",  "Death rate", min=0, max=0.01, value=0.007, step=0.001),
      sliderInput("SystemPressureFlag",  "System Pressure", min=0, max=1, value=0, step=1),
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("stats")
    )
  )
)

# the server Function
server <- function(input, output) {
  
  data <- reactive({
    cat(file=stderr(), "Function data (reactive function)...\n")
    # setup simulation times and time steps
    begin = 2017
    end = 2053
    timeStep = 1
    # create time vector
    times <<- seq(begin, end, by = timeStep)
    # create stocks vector with initial values. Inflows and outflows
    # can increase or decrease the stock's value over time
    stocks  <- c(

      sPopulation0_14 = as.numeric(input$sPopulation0_14), 
      sPopulation15_39 = as.numeric(input$sPopulation15_39),
      sPopulation40_64 = as.numeric(input$sPopulation40_64),
      sPopulation65_plus = as.numeric(input$sPopulation65_plus),
      sGeneralPractitioners = 4000,
      ExpectedRetirement = 100,
      sPatientsBeingTreated = 24000000
      
    )
    # create exogenous vector
    parms    <- c(aCrudeBirthRate = as.numeric(input$aCrudeBirthRate),    
                  aCrudeDeathRate = as.numeric(input$aCrudeDeathRate),    
                  aAverageGPVisits0_14 = 3, 
                  aAverageGPVisits15_39 = 4, 
                  aAverageGPVisits40_64 = 5, 
                  aAverageGPVisits65plus = 10, 
                  aStandardGPProductivity = 24,    
                  aStandardGPWorkYear = 250,       
                  aTargetCompletionTime = 1,
                  aAverageGPCareerDuration = 40,
                  aDesiredGPsPer1000sPopulation = 0.8 / 1000,
                  aAdjustmentTime = 5,
                  WorkYearFlag = 1,
                  ProductivtyFlag = 1,
                  D1 = 15,
                  D2 = 25,
                  D3 = 25,
                  DC = 3,
                  SystemPressureFlag = as.numeric(input$SystemPressureFlag)
    )
    
    # in order to simulate the model needs a set of equations that describe the relationship. The are
    # defined in the model above and called in the ode `func`    
    o <- data.frame(ode(y=stocks, times=times, func = func, 
                      parms=parms, method="euler"))
  })
  
  output$plot <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <<- data()
    
    p1 <-ggplot()+
      geom_line(data=o,aes(time, sPopulation, color="Population")) +
      geom_point() +
      scale_y_continuous(labels = comma)+
      ylab("System Stocks")+
      xlab("Year") +
      labs(color="")+
      theme(legend.position="bottom") +
      theme_minimal()
    
     p2 <-ggplot()+
      geom_line(data=o,aes(time, DesiredGPs, color="DesiredGPs")) +
       geom_line(data=o,aes(time, sGeneralPractitioners, color="GPs")) +
       scale_y_continuous(labels = comma)+
       ylab("GPs (Target & Actual")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="bottom") +
       theme_minimal()
     
     p3 <-ggplot()+
       geom_line(data=o,aes(time, GeneralPractitionerDemand, color="Demand")) +
       scale_y_continuous(labels = comma)+
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="bottom") +
       theme_minimal()
     
     p4 <-ggplot()+
       geom_line(data=o,aes(time, StandardAnnualCompletedVisits, color="Standard")) +
       geom_line(data=o,aes(time, PotentialCompletedVisits, color="Potential")) +
       geom_line(data=o,aes(time, DesiredCompletedVisits, color="Desired")) +
       scale_y_continuous(labels = comma)+
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="bottom") +
       theme_minimal()
     
     p5 <-ggplot()+
       geom_line(data=o,aes(time, SystemPressure, color="SystemPressure")) +
       scale_y_continuous(labels = comma)+
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="bottom") +
       theme_minimal()
    
    grid.arrange(p1,
                 p2, 
                 p3,
                 p4,
                 p5,
                 ncol=2)
    
  })
  
  
  output$stats <- renderPrint({
    summary(data()[,c("sPopulation")])
    
  })
  
}

# Launch the app
shinyApp(ui, server)