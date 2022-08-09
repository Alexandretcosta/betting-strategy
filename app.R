library(shiny)
library(packHV)
setwd('C:\\Users\\Alexandre\\Documents\\github\\fibonacci_system')
source('functions_article.R')


# Define UI for random distribution app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Fibonacci System"),
  
  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
    sliderInput("t0",
                  "Start Gambling:",
                  value = 1,
                  min = 1,
                  max = 100),
      
      # Input: Select the random distribution type ----
    sliderInput("prob",
                "Probability of Sucess:",
                value = 0.5,
                min = 0,
                max = 1),
    

  
  numericInput("perc_win", "Amount gained if you win (%):",
               min = 1, max = Inf, value = 100, step = 1),
  
  numericInput("perc_lost", "Amount gained if you lose (%):",
               min = 1, max = Inf, value = 100, step = 1),
  
  selectInput("crit", "Simulation stop choice:",
               c("After N Plays" = 'true',
                 "After % profit" = 'false')),
  
  conditionalPanel(condition = "input.crit == 'false'",
                   numericInput("spread", "Profit %:",
                                min = 0, max = Inf, value = 10000, step = 100),
                   
                   numericInput("mc", "How many times do you want to repeat the simulation process?",
                                min = 1, max = 10000, value = 10, step = 1)),
  
  conditionalPanel(condition = "input.crit == 'true'",
                   numericInput("nsteps",
                               "Number of Plays:",
                               value = 100,
                               min = 2,
                               max = 100000),
                   
                   numericInput("mc1", "How many times do you want to repeat the simulation process?",
                                min = 1, max = 10000, value = 10, step = 1)),
  downloadButton('download','Download the Data')
  
  # br() element to introduce extra vertical spacing ----
  #br(),
  
  # Input: Slider for the number of observations to generate ----
  
  
),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Description",
                           h2("Objective:"),
                           p("The application was designed to simulate bets according to the player's parameters."),
                           br(),
                           h2('Betting Strategy:'),
                           p('Fibonacci System'),
                           br(),
                           h2('Parameters:'),
                           br(),
                           p("- Start Gambling: Starting point of the bet according to the fibonacci sequence;"),
                           p("- Probability of Sucess: Probability of winning the bet;"),
                           p("- Amount gained if you win (%): If you win the bet, how many you will win da sua aposta;"),
                           p("- Amount gained if you lose (%): If you lose the bet, how many you will lost da sua aposta;"),
                           p("- Simulation stop choice: Choice you have to stop the simulation. If you choose 'After N Plays', the simulation will stop after N plays. If you choose 'After % Profit', the simulation will stop after you won % of profit."),
                           p("Now the parameters depend on your choice of stopping the simulation. If you choose 'After N Plays', we have the following parameters:"),
                           p("- Number of plays: number of plays you want to simulate;"),
                           p("- Number repetion of process: If you wanna repeat process, you need put how many times you wanna repeat your simulation."),
                           p("Now, if you choose 'After % Profit', we have the following parameters:"),
                           p("- Profit %: Profit percentage based on your initial bet;"),
                           p("- Number repetion of process: If you wanna repeat process, you need put how many times you wanna repeat your simulation."),
                           h2('Results:')
                        ),
                  tabPanel("Analysis",h2("Results"),
                           plotOutput("plot")),
                  tabPanel("Data of Results", dataTableOutput('dto'))
                                              
      )
      
    #)
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
 
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    teste_numerico2(nsteps = input$nsteps,
                             n_matriz = input$nsteps,
                             prob = input$prob,
                             time_0 = input$t0,
                             percent_win = input$perc_win/100,
                             percent_lost = input$perc_win/100,
                             mc1 = input$mc1,
                             mc2 = input$mc,
                             criterio_parada = input$crit,
                             spread = input$spread/100)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    h1("Results")
    
    if("EarnedValue" %in% colnames(d()[['df']])){
    #dist <- input$dist
    #n <- input$n
    
      hist_boxplot(d()[['df']]$EarnedValue,
         main = paste("After N plays", " (", input$nsteps, ")", sep = ""),
         col = "#75AADB", border = "white",xlab ='Ganhos Totais')
    }else{
      hist_boxplot(d()[['df']]$NrBets,
           main = paste("After % profit", " (", input$spread, ")", sep = ""),
           col = "#75AADB", border = "white",xlab ='Number of Plays')
      
    }
  })
  
  output$dto <- renderDataTable({d()[['df']]})
  output$download <- downloadHandler(
    filename = function(){"data_simulation.csv"},
    content = function(fname){
      write.csv2(d()[['df']],fname)
    }
  )
  
  # Generate a summary of the data ----
#  output$descrition <- renderText({
#    "## Objective:
#The application was designed to simulate bets according to the player's parameters.
#
#Betting Strategy:
#Fibonacci System
#
#Parameters:"
#  })
  
  # Generate an HTML table view of the data ----
  #output$table <- renderTable({
  #  d()[['df']]
  #})
  #
}

# Create Shiny app ----
shinyApp(ui, server)
