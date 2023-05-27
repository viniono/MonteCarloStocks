library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(tidyquant)
library(ggplot2)
library(dplyr)



##NASDAQ, NYSE, and AMEX.
AMEX <- read.csv("AMEX.csv")
NASDAQ <- read.csv("NASDAQ.csv")
NYSE <- read.csv("NYSE.csv")
nas1<-distinct(rbind(AMEX,NASDAQ,NYSE))
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),
                ##shinythemes::themeSelector(),
                
                navbarPage(
                  title=list(tags$head(tags$style()), 
                             HTML('<img src="bull_bbear.png", height="30px"    
          style="float:right"/>','<p style="color:black"></p>')),
                  tabPanel("Home",
                           fluidRow(
                             shiny::HTML("<br><br><center> <h1>WELCOME TO YOUR PERSONAL STOCK SIMULATOR!<h1> </center><br>"),
                             div(img(src="bull_bbear.png",height="100px"), style="text-align: center;"),
                             tags$hr(),
                             column(3),
                             column(6,
                                    shiny::HTML("<br><br><center> <h2>What you'll find here</h2> </center><br>"),
                                    shiny::HTML("<center><h4>An interactive tool to help you explore your interest and match it with the financial 
                                  markets. Additionally, with information from the last 10 year across all stocks listed in 
                                  NASDAQ, AMEX, and NYSE, you will be able to simulate the potential paths a given stock could follow into the 
                                  future using a simple Monte Carlo simulation.</center></h4>")
                             ),
                             column(3)
                           ),
                           fluidRow(
                             
                             style = "height:50px;"),
                           
                           # PAGE BREAK
                           tags$hr(),
                           fluidRow(
                             column(3),
                             column(6,
                                    shiny::HTML("<br><br><center> <h2>What can you get out of this app and why should you spend your time interacting with it?</h2> </center><br>"),
                                    shiny::HTML("<center><h4>In this app, you will be able to further explore your understanding of the famous 
                      “random walk” regarding  financial markets or even have your first exposure to it. Additionally, 
                      you will be able to get insights about stocks to potentially make use of this information to further 
                      explore some thesis about an investment opportunity in the Equities’s market.</center></h4>")
                             ),
                             column(3)
                           ),
                           fluidRow(
                             
                             style = "height:50px;"),
                           
                           # PAGE BREAK
                           tags$hr(),
                           fluidRow(
                             column(3),
                             column(6,
                                    shiny::HTML("<br><br><center> <h2>Intended Audience</h2> </center><br>"),
                                    shiny::HTML("<h4><center>Anyone interested in the applications of computational finance or a general interest in the 
                                  stock market with a basic understand of the equities world!</center></h4>")
                             ),
                             column(3)
                           ),
                           fluidRow(
                             
                             style = "height:50px;"),
                           
                           # PAGE BREAK
                           tags$hr(),
                           fluidRow(
                             column(3),
                             column(6,
                                    shiny::HTML("<br><br><center> <h2>Acknowledgements and Resources</h2> </center><br>"),
                                    shiny::HTML("<center><h4>This shiny app extensively uses the tools provided by the libraries Quandl and Tidyquant, as well as its datasets scrapped from 
                                  NASDAQ and Yahoo Finance.</h4></center>")
                             ),
                             column(3)
                           ),
                           fluidRow(
                             
                             style = "height:30px;")
                  ),
                  tabPanel("Design Process",fluidRow(
                    shiny::HTML("<br><br><center> <h1>Below you'll find a short description of my thought process on this project's design<h1> </center><br>"),
                    div(img(src="design.png",height="100px"), style="text-align: center;"),
                    tags$hr(),
                    column(3),
                    column(6,
                           shiny::HTML("<br><br><center> <h3> Analysis of system interactivity following SYSTEM DESIGN- T. 
                         MORAN: Dimensions of Evaluation for User-System Performance</h2> </center><br>"),
                           
                           shiny::HTML("<h4>
             <ul><li>Time: The app provides a system that renders all graphs and modeling in a very fast manner (exceptions to simulations around 1000 days).<br><br><li>
              Errors: The only error provided by the app is for any simulation lower than 120 days because of the nature of the libraries used in the construction of the app.<br><br><li>
              Learning: It is very straightforward and it does not take much time for the user to understand the limitations and possibilities within the app.<br><br><li>
Functionality: The user can  filter all publicly traded firms in NASDAQ by certain personal preferences and additionally use a monte carlo simulation model to predict/visualize the possible prices/returns of a given stock in the future.<br><br> <li>
Recall: The simplicity of the UI and UX of the app makes it easy for the user to have the same experience regardless of the exposure to the app.<br><br><li>
Fatigue: Based on a sample of 10 testing user, we have concluded that users normally get bored with the basic functionality of the app after around 7 minutes of the app, which is acceptable given the simplicity of this project.<br><br> <li>
Acceptability: Users generally really liked the idea and the UI/UX of the app. Most get excited about the probability outputs in the monte carlo simulation, customizable by stock. However,those that do not have a simple finance background do 
not know how to use the information provided by the model or even extrapolate to some general insight when it comes to investing.
</h4>")
                    ),
                    column(3)
                  ),
                  fluidRow(
                    
                    style = "height:50px;"),
                  
                  # PAGE BREAK
                  tags$hr(),
                  fluidRow(
                    column(3),
                    column(6,
                           shiny::HTML("<br><br><center> <h2>Further work required to improve my app’s User-System Performance:</h2> </center><br>"),
                           shiny::HTML("<h4><center> Possibly create a better system that flows information from tab to tab, so that users understand that all 
             of the content it is supposed to be interactive all used together. Additionally, we could potentially add extra steps or functionalities 
                         within the app to use the outputs generated from the model to a subsequent interactive tool.</center></h4>")
                    ),
                    column(3)
                  ),
                  fluidRow(
                    
                    style = "height:50px;"),
                  
                  # PAGE BREAK
                  tags$hr(),
                  fluidRow(
                    column(3),
                    column(6,
                           shiny::HTML("<br><br><center> <h2> In what ways did I ensure that the app’s concepts are simple and straightforward? </h2> </center><br>"),
                           shiny::HTML("<h4><center>By providing a straightforward title to every tab in the app and extensive but simple explanation in the first page of the app, 
             the user is able to fully understand the functionalities of my Web application and its limitations.</center></h4>")
                    ),
                    column(3)
                  ),
                  fluidRow(
                    
                    style = "height:30px;"),
                  fluidRow(
                    
                    style = "height:50px;"),
                  
                  # PAGE BREAK
                  tags$hr(),
                  fluidRow(
                    column(3),
                    column(6,
                           shiny::HTML("<br><br><center> <h2> How are my app’s concepts  mapped to the user interface? </h2> </center><br>"),
                           shiny::HTML("<h4><ul><li>I made sure the tabs of my app sequentially represented the order in which the user 
             should follow in order to have a full experience of the app.<ul><li> For example: Home→ Design process→ Stock searcher→ Modeling <li>
While the interface does suggest the above usage structure, it should still be very straightforward to the user that the modeling part should be a tool used after the user selects or decides on a given stock.
</h4>")
                    ),
                    column(3)
                  ),
                  fluidRow(
                    
                    style = "height:30px;")),
                  
                  ##NASDAQ, NYSE, and AMEX.
                  tabPanel("Table",
                           sidebarLayout(
                             sidebarPanel(
                               pickerInput(
                                 inputId = "picker_sector",
                                 label = "What industries would you like to explore?",
                                 # make sure we have the nasdqa list of stocks here in this part of the script
                                 choices = sort(as.character(unique(NASDAQ$industry))),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE,
                                   `live-search` = TRUE)), 
                               pickerInput(
                                 inputId = "picker_country",
                                 label = "What Countries would like to invest?",
                                 # make sure we have the nasdqa list of stocks here in this part of the script
                                 choices =sort(as.character(unique(NASDAQ$country))),
                                 multiple = TRUE,
                                 options = list(
                                   `actions-box` = TRUE,
                                   `live-search` = TRUE
                                 )
                               ),
                             ),
                             mainPanel(
                               h2("Stocks you might want to consider"),
                               DT::dataTableOutput("mytable")
                               #tableOutput("penguin_table")
                             )
                           )
                  ),
                  tabPanel("Simulation",
                           sidebarLayout(
                             sidebarPanel(
                               pickerInput(
                                 inputId = "pick_ticker",
                                 label = "What stock would you simulate?",
                                 # make sure we have the nasdqa list of stocks here in this part of the script
                                 choices =sort(as.character(unique(NASDAQ$symbol))),
                                 selected="AAPL",
                                 multiple = FALSE),
                               numericInput("Simulation_days", 
                                            label = "How many days would like to predict?", 
                                            value = 120),
                               sliderInput("Y_lim", label = "Set your plot Y boundaries", min = 0, 
                                           max = 800, value = c(0,800))
                             ),
                             mainPanel(textOutput("mytext"),plotOutput("myPlot"),plotOutput("myPlot2"),fluidRow(
                               
                               style = "height:30px;"),fluidRow(
                                 
                                 style = "height:30px;"))))))




server<-shinyServer(function(input, output){
  
  ## Reactive filter
  selectdata <- reactive({
    
    
    dplyr::filter(nas1, nas1$industry %in% input$picker_sector & nas1$country %in% input$picker_country)
    #}
  })
  
  output$mytable = DT::renderDataTable({
    selectdata()
  })
  
  
  ### Simulation
  
  selectdata2<-reactive({
    prices_list= pre_work()[[2]]
    plot(prices_list[1,], type='l', ylab=paste("Simulated price of ", input$pick_ticker), xlab="Days",ylim=c(input$Y_lim[1],input$Y_lim[2]))
    for(i in 1:1000) {
      lines(prices_list[i, ], type = 'l', col=i)
    }
  })
  
  
  
  pre_work = reactive({
    n=input$pick_ticker
    X=getSymbols(n, from = '2011-09-08', to = "2022-09-01", auto.assign = FALSE, warnings = FALSE)
    
    daily_mean <- mean(dailyReturn(X)) #dailyReturn function from the tidyquant library
    daily_std_dev <- sd(dailyReturn(X))
    no_of_days <- input$Simulation_days # Set variable to 120 days
    starting_price <- as.numeric(last(X[,4]))#last(X$X.Close)[[1]]
    
    set.seed(101) #Set seed for reproducibility of the random numbers
    returns <- 1+rnorm(no_of_days, mean=daily_mean, sd=daily_std_dev) #Generate random variables
    prices <- cumprod(c(starting_price, returns)) #Calculate cumulative product
    
    #plot(prices, type='l', ylab="Simulated price of SPY", xlab="Days")
    
    no_of_sims <- 1001
    returns_list <- matrix(0, nrow = no_of_sims, ncol = no_of_days) #define matrices
    prices_list <- matrix(0, nrow = no_of_sims, ncol = no_of_days+1) 
    #Note: returns_list and prices_list are actually matrices, I just chose a poor name
    
    for(i in 1:no_of_sims) { # for loop - 1001 iterations
      returns_list[i,] <- rnorm(no_of_days, mean=daily_mean, sd=daily_std_dev) #Generate random variables
      prices_list[i,] <- cumprod(c(starting_price, 1+returns_list[i,]))#Calculate cumulative product
    }
    
    total_returns <- array(NA, dim= no_of_sims, dimnames=NULL)
    for (i in 1:no_of_sims) {
      total_returns[i] <- (prices_list[i, 121]-prices_list[i, 1])/prices_list[i,1] #calculate total % return for each 120 day simulation
    }
    
    return (list(total_returns,prices_list))
  })
  
  selectdata3<-reactive({ 
    total_returns= pre_work()
    
    ggplot(, aes(x=total_returns[[1]])) +
      geom_density(fill="gray")+
      geom_vline(aes(xintercept=0), color="blue",
                 linetype="dashed")+
      labs(title="Weight density curve",x="% Total return", y = "Density")+
      theme_classic()
    
  })
  
  selectdata4<-reactive({ 
    
    total_returns= pre_work()
    count_neg_returns=sum(total_returns[[1]]<=0)/1001
    count_pos_returns=sum(total_returns[[1]]>0)/1001
    
    x=paste("Probability of making postive returns over", input$Simulation_days, " days is " , round((count_pos_returns*100),digits=3),"%", ". The probability of making negative or 0 returns is ", round((count_neg_returns*100),digits=3),"%")
    
    print(x[1])
    
  })
  
  #Drawing a plot of 50 first simulations
  output$myPlot = renderPlot({
    selectdata2()
  })
  
  
  ## This plot is not filtering 
  output$myPlot2 = renderPlot({
    selectdata3()
  })
  
  
  
  output$mytext = renderPrint({
    selectdata4()
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
