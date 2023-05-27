#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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



##NASDAQ, NYSE, and AMEX.
nas <- tq_exchange("nasdaq")
nas2 <- tq_exchange("amex")
nas3 <- tq_exchange("nyse")
nas1<-rbind(nas,nas2,nas3)
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
                   inputId = "picker_exchange",
                   label = "What US stock exchange would you like to consider?",
                   # make sure we have the nasdqa list of stocks here in this part of the script
                   ##NASDAQ, NYSE, and AMEX.
                   choices =c("NASDAQ", "NYSE","AMEX"),
                   multiple = FALSE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE
                   )
                 ),
                 pickerInput(
                   inputId = "picker_sector",
                   label = "What industries would you like to explore?",
                  # make sure we have the nasdqa list of stocks here in this part of the script
                   choices = sort(as.character(unique(nas1$industry))),
                   multiple = TRUE,
                   options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE)), 
                 pickerInput(
                   inputId = "picker_country",
                   label = "What Countries would like to invest?",
                   # make sure we have the nasdqa list of stocks here in this part of the script
                   choices =sort(as.character(unique(nas1$country))),
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
                                choices =sort(as.character(unique(nas1$symbol))),
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
                              
   
  

