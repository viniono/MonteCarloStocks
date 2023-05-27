#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(Quandl)
library(tidyquant)
library(ggplot2)
library(dplyr)





# Define server logic required to draw a histogram
nas1 <- tidyquant::tq_exchange("nasdaq")

shinyServer(function(input, output){
  
  
  ## Reactive filter
  selectdata <- reactive({
    nas1 <- tq_exchange(input$picker_exchange)
    dplyr::filter(nas1, nas1$industry %in% input$picker_sector & nas1$country %in% input$picker_country)
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
  