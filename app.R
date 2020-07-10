
source("dependencies.R")
source("SDM.R")

# Setting up the client (page elements appear in order specified)
ui <- fluidPage(
  titlePanel("System Dynamics representation of the Health Sector"),
  a("See GitHub for resources", href="https://github.com/MaguireMaName/SDM/",target="_blank"),
  br(),br(),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("aCrudeBirthRate",  "Birth rate", min=0, max=0.1, value=0.02, step=0.01),
      sliderInput("aCrudeDeathRate",  "Death rate", min=0, max=0.01, value=0.007, step=0.001),
      #sliderInput("SystemPressureFlag",  "System Pressure", min=0, max=1, value=0, step=1),
      sliderInput("aStandardGPProductivity",  "Productivity", min=20, max=30, value=24, step=1),
      sliderInput("aAverageGPCareerDuration",  "Career Duration", min=20, max=50, value=40, step=10),     
      sliderInput("aStandardGPWorkYear",  "Work Year", min=200, max=360, value=250, step=10),
      #numericInput("aDesiredGPsPer1000sPopulation",  "Desired GPs per 1000 population:", value = 0.0008),
      checkboxInput("SystemPressureFlag", "Allow response to system pressure", FALSE)
      
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

      sPopulation0_14 = 1000000, 
      sPopulation15_39 = 1500000,
      sPopulation40_64 = 2000000,
      sPopulation65_plus = 500000,
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
                  aStandardGPProductivity = as.numeric(input$aStandardGPProductivity),    
                  aStandardGPWorkYear = as.numeric(input$aStandardGPWorkYear),       
                  aTargetCompletionTime = 1,
                  aAverageGPCareerDuration = as.numeric(input$aAverageGPCareerDuration),
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
      geom_line(data=o,aes(time, sPopulation, color="Population"), size=1.15) +
      geom_point() +
      scale_y_continuous(labels = comma) +
      scale_color_viridis(begin = .4, end = .9, option = "magma", discrete=TRUE) +
      xlab("Year") +
      labs(color="")+
      theme(legend.position="top") +
      theme_minimal()
    
     p2 <-ggplot()+
       geom_line(data=o,aes(time, DesiredGPs, color="DesiredGPs"), linetype="dashed", size=1.15) +
       geom_line(data=o,aes(time, sGeneralPractitioners, color="GPs"), size=1.15) +
       scale_y_continuous(labels = comma) +
       scale_color_viridis(begin = .4, end = .9, option = "magma", discrete=TRUE) +
       ylab("GPs (Target & Actual")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="none") +
       theme_minimal()
     
     p3 <-ggplot()+
       geom_line(data=o,aes(time, GeneralPractitionerDemand, color="Demand"), size=1.15) +
       scale_y_continuous(labels = comma) +
       scale_color_viridis(begin = .3, end = .9, option = "magma", discrete=TRUE) +
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="none") +
       theme_minimal()
     
     p4 <-ggplot()+
       geom_line(data=o,aes(time, StandardAnnualCompletedVisits, color="Standard"), size=1.15) +
       geom_line(data=o,aes(time, PotentialCompletedVisits, color="Potential"), size=1.15) +
       geom_line(data=o,aes(time, DesiredCompletedVisits, color="Desired"), size=1.15) +
       scale_y_continuous(labels = comma) +
       scale_color_viridis(begin = .4, end = .9, option = "magma", discrete=TRUE) +
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="none") +
       theme_minimal()
     
     p5 <-ggplot()+
       geom_line(data=o,aes(time, SystemPressure, color="SystemPressure"), size=1.15) +
       scale_y_continuous(labels = comma) +
       scale_color_viridis(begin = .4, end = .9, option = "magma", discrete=TRUE) +
       ylab("System Stocks")+
       xlab("Year") +
       labs(color="")+
       theme(legend.position="none") +
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