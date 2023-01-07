library(packHV)
library(shiny)
library(markovchain)
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
  downloadButton('download','Download the Data Results')
  

),
    
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Description",
                           h2("Introduction:"),
                           p("The following study aims to test the Fibonacci strategy in betting. First, let's give a summary of the Fibonacci sequence. The Fibonacci sequence was created by the Italian mathematician Leonardo 'Fibonacci' in the 13th century."),
                           p("The idea behind the sequence is pretty simple. The first two terms of the sequence are '1', and '1' and the next ones will be the sum of the previous two values. So the start of the sequence is 1, 1, 2, 3, 5, 8, 13... and so on."),
                           p("The betting strategy using the Fibonacci sequence would be for betting with only two possible outcomes. For example, the coin result (heads or tails). It can also be the odd or even result of a die. In the case of the Casino, it can be used when betting red or black on a roulette wheel."),
                           p("To explain how the system works, let's take an example of heads and tails. To start, we'll bet $1, the first element of the Fibonacci sequence. Our bet will be Heads. If the result of the coin is Tails, it means that we lose, then in our next bet we will increase the bet for the next element of the sequence in this case it would be $1."),
                           p("If the result is heads, that is, we win the bet. So in the next bet we will decrease the bet amount for two previous elements of the sequence. As we have the first element, we continue betting $1, however, if we had bet $5, we would return two elements, so in the next bet we will bet $2 (2, 3, 5). How the Fibonacci System works."),
                           br(),
                           h2("Objective:"),
                           p("Our objective will be to verify in which of the scenarios it is possible to win with the strategy and which not. Also, check the risk linked to this strategy."),
                           p("The application was designed to simulate bets according to the player's parameters. We want the reader/player to be able to make several possible simulations, for example, the probability of winning the bet or the percentage he would win on his own bet, among others."),
                           p("We also created two stopping criteria and created a simulation of the process, similar to a Monte Carlo idea. At the end, the reader can download the simulation data if it is of interest."),
                           br(),
                           #h2('Betting Strategy:'),
                           #p('Fibonacci System'),
                           #br(),
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
                           br(),
                           h2('Results:'),
                           p("About the results, we will have two analyses. One for 'After N Plays' and one for '% Profit'."),
                           p("In both, we have the number of repetition of the process, that is, we want to calculate the average of the results (Profit and Number of Bets)."),
                           p("For each situation we will have a different analysis.")
                        ),
                  
                  tabPanel("Analysis",
                           
                  conditionalPanel(
                    condition = "input.crit == 'true'",
                           
                    h2("Analysis - After N Plays"),
                           p("In this topic, some variables of interest will be seen when the stopping criterion is 'After N Plays'. The most relevant variables are:"),
                           p("- Earned Value: the value that the player will earn at the end of N plays;"),
                           p("- Maximum Money Loss: risk that the player is assuming, that is, what will be the maximum that he has to leave provisioned."),
                           br(),
                    h3('Earned Value'),
                    br(),
                      p("First, let's analyze the Earned Value that is decided by betting ", strong(textOutput("text",inline = T)),"times, i.e, the amount we win at the end of the game"),
                      p("This variable is extremely important because it deals with how much money you will take home."),
                      p("Below we have some statistics of this result and a graph with the distribution."),
                    tableOutput("EarnedValue_describe1"),
                    plotOutput("EarnedValue_plot1"),
                           h3('Maximum Money Loss'),
                           br(),
                          p("In this topic we will analyze the maximum Loss that is decided by betting ", strong(textOutput("text1",inline = T)),"plays, i.e, how much do we have as a 'background' to play."),
                          p("This variable can define the risk the person wants to take. What it means? It means that if you do not continue betting according to the strategy, you may have a loss based on this statistic, however, if you continue you will have to have this value as your emergency fund."),                          
                  tableOutput("MaximumMoneyLoss_describe1"),
                  plotOutput("MaximumMoneyLoss_plot1"),
                          h3('Maximum Money Won'),
                          br(),
                          p("This stat is the opposite of Maximum Money Won, i.e, what would be the most you could win in your ", strong(textOutput("text2",inline = T))," moves."),
                          p("If you put it as probability 0.5, on average the maximum loss is greater in modulus of the maximum gain."),
                          tableOutput("MaximumMoneyWon_describe1"),
                          plotOutput("MaximumMoneyWon_plot1"),
                          h3('Maximum Bet'),
                          br(),
                          p("Finally, we have the statistic of the biggest amount you bet, that is, what was the biggest element of the Fibonacci sequence that we got in ", strong(textOutput("text3",inline = T))," moves."),
                          p("This statistic will be mainly impacted by the number of losses in a row you have, since with each loss we move 1 space forward in the sequence. You can see that if your probability of winning increases, the lower the maximum value of the bet will be."),
                          tableOutput("MaximumBet_describe1"),
                          plotOutput("MaximumBet_plot1")),
                                    
                  conditionalPanel(
                    condition = "input.crit == 'false'",
                             h2("Analysis - After % Profit"),
                             p("In this topic, some variables of interest will be seen when the stopping criterion is 'After % Profit'. Unlike the first stop criterion, the gain value is already predefined. In this case, the interest in analyzing will be the number of bets made to reach the profit. In addition, the risk linked to this stop criterion will also be seen."),
                             br(),
                            h3('Number of Bets'),
                            br(),
                            p("The analysis with the number of moves will tell us on average how many bets the player must place if he wants to make", strong(textOutput("text4",inline = T))," percent profit based on his first bet."),
                            p("Since your first bet was ", strong(textOutput("text6",inline = T))," unit(s). The average number of bets in this scenario was ", strong(textOutput("text5",inline = T)),"."),
                            tableOutput("NrBets_describe1"),
                            plotOutput("NrBets_plot1"),
                              h3('Maximum Money Loss'),
                              br(),
                              p("In this topic we will analyze the maximum Loss that is decided by betting, i.e, how much do we have as a 'background' to play."),
                              p("This variable can define the risk the person wants to take. What it means? It means that if you do not continue betting according to the strategy, you may have a loss based on this statistic, however, if you continue you will have to have this value as your emergency fund."),
                              tableOutput("MaximumMoneyLoss_describe2"),
                              plotOutput("MaximumMoneyLoss_plot2"))
                  ),
                  
                  tabPanel("Data of Results", dataTableOutput('dto'))
                                              
      )
      
    #)
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
 
  d <- reactive({
    teste_numerico(nsteps = input$nsteps,
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
  

  output$MaximumMoneyLoss_describe1 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$MaximumMoneyLoss)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  output$MaximumMoneyLoss_plot1 <- renderPlot({
    h1("Results")
      hist_boxplot(d()[['df']]$MaximumMoneyLoss,
                   main = paste("Maximum Money Loss", " (", input$nsteps, ")", sep = ""),
                   col = "#75AADB", border = "white",xlab ='Max Loss')
  })
  
  output$MaximumMoneyWon_describe1 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$MaximumMoneyWon)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  output$MaximumMoneyWon_plot1 <- renderPlot({
    h1("Results")
    hist_boxplot(d()[['df']]$MaximumMoneyWon,
                 main = paste("Maximum Money Won", " (", input$nsteps, ")", sep = ""),
                 col = "#75AADB", border = "white",xlab ='Max Won')
  })
  
  output$MaximumBet_describe1 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$MaximumBet)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  output$MaximumBet_plot1 <- renderPlot({
    h1("Results")
    hist_boxplot(d()[['df']]$MaximumBet,
                 main = paste("Maximum Bet", " (", input$nsteps, ")", sep = ""),
                 col = "#75AADB", border = "white",xlab ='Max Bet')
  })
  
  output$EarnedValue_describe1 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$EarnedValue)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  output$EarnedValue_plot1 <- renderPlot({
    h1("Results")
    hist_boxplot(d()[['df']]$EarnedValue,
                 main = paste("Earned Value", " (", input$nsteps, ")", sep = ""),
                 col = "#75AADB", border = "white",xlab ='Earned Value')
  })
  
  output$NrBets_describe1 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$NrBets)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  output$NrBets_plot1 <- renderPlot({
    h1("Results")
    hist_boxplot(d()[['df']]$NrBets,
                 main = paste("Nr Bets", " (", input$nsteps, ")", sep = ""),
                 col = "#75AADB", border = "white",xlab ='Nr Bets')
  })
  
  
  output$MaximumMoneyLoss_plot2 <- renderPlot({
    h1("Results")
    hist_boxplot(d()[['df']]$MaximumMoneyLoss,
                 main = paste("Maximum Money Loss", " (", input$nsteps, ")", sep = ""),
                 col = "#75AADB", border = "white",xlab ='Max Loss')
  })
  
  output$MaximumMoneyWon_describe2 <- renderTable({
    t(data.frame(unclass(summary(d()[['df']]$MaximumMoneyWon)), 
                 check.names = FALSE, stringsAsFactors = FALSE))
    
  })
  
  
  output$text <- renderText({ input$nsteps })
  output$text1 <- renderText({ input$nsteps })
  output$text2 <- renderText({ input$nsteps })
  output$text3 <- renderText({ input$nsteps })
  output$text4 <- renderText({ input$spread })
  output$text5 <- renderText({ mean(d()[['df']]$NrBets) })
  output$text6 <- renderText({ min(d()[['df']]$StartingPoint) })
  
  output$dto <- renderDataTable({d()[['df']]})
  
  output$download <- downloadHandler(
    filename = function(){"data_simulation.csv"},
    content = function(fname){
      write.csv2(d()[['df']],fname)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
