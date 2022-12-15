library(shiny)
library(shinydashboard)
library(shinyjs)
library(sodium)
library(tidyverse)
library(data.table)
library(prophet)
library(lubridate)
library(forecast)
library(tseries)
library(ggfortify)
library(plotly)
library(ggplot2)
library(dygraphs)
library(zoo)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                   ))
)
#sample log in and log out just in case
user_data = data.frame(
  username_id = c("a","b"),
  passod   = sapply(c("a","b"),password_store),
  permission  = c("advanced","basic"),
  models_created = c(0,0),
  Time_spent = c(0,0),
  stringsAsFactors = F
)
#utils::write.csv(user_data,"user_data.csv", row.names = FALSE)
credentials <-read.csv("user_data.csv")

#code here
df<-read.csv("user_data.csv")
header <- dashboardHeader( title = "Thesis app v 1.1", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")
server <- function(input, output, session) {
  df<-read.csv("user_data.csv")
  u_data<-df
  makeReactiveBinding("y")
  makeReactiveBinding("dataset")
  makeReactiveBinding("u_data")
  makeReactiveBinding("forecast")
  makeReactiveBinding("m")
  makeReactiveBinding("arima")
  makeReactiveBinding("tmp")
  login = FALSE
  USER <- reactiveValues(login = login)
  credentials <-read.csv("user_data.csv")
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  observeEvent(input$addrows,{
    if (!(input$nazwa_prez)=="") {
      u_data %>% add_row(username_id = input$nazwa_prez, passod = sapply(c(input$cena_prez),password_store),permission="basic") ->>u_data
      utils::write.csv(u_data,"user_data.csv", row.names = FALSE)
    }
  })
  
  observeEvent(input$deleteRows,{
    if (!is.null(input$table1_rows_selected) ) {
      if(!u_data[input$table1_rows_selected,"username_id"]==input$userName){
        u_data <<- u_data[-as.numeric(input$table1_rows_selected),]
      }
    }
    utils::write.csv(u_data,"user_data.csv", row.names = FALSE)
  })
  
  observeEvent(input$kup_prez,{
    if (!is.null(input$table1_rows_selected)) {
      if(u_data[input$table1_rows_selected,"permission"] == "basic" & !u_data[input$table1_rows_selected,"username_id"]==input$userName){
        u_data[input$table1_rows_selected,"permission"]<<-"advanced"
        utils::write.csv(u_data,"user_data.csv", row.names = FALSE)
      }
      else{
        if(!u_data[input$table1_rows_selected,"username_id"]==input$userName){
          u_data[input$table1_rows_selected,"permission"]<<-"basic"
          utils::write.csv(u_data,"user_data.csv", row.names = FALSE)
        }
      }
      
    }
  })
  output$table1 <- DT::renderDataTable({
    u_data %>% select(-c(passod))
    
  })
  output$contents <- DT::renderDataTable({
    if(input$head == F){
      head(dataset)
    }
    else{
      dataset
    }
  })
  observeEvent(input$file1,{
    inFile <- input$file1
    if (is.null(inFile)){
      
    }
    else{
      dataset<<-read.csv(inFile$datapath,header = T)
      if(names(dataset)[1]=="Historical.Data"){
        dataset<<-read.csv(inFile$datapath,skip = 3,sep = ";")
        
      }else{
        dataset<<-read.csv(inFile$datapath,header = T)
      }
    }

  })
  
  observeEvent(input$var_y,{
    inFile <- input$file1
    if (is.null(inFile) | is.null(input$var_y) ){
      
    }
    else{
      nameof_var = input$var_y
      y<<-dataset$nameof_var
    }
    
  })
  observeEvent(input$do_model_arima,{
    if(!is.null(dataset) & !is.null(input$var_y)){
      dataset %>% select(Date,input$var_y) -> df_temp
      names(df_temp)<-c('ds','y')
      df_temp$y <- as.numeric(gsub('[$,]', '', df_temp$y))
      df_temp$ds <-as.Date(parse_date_time(df_temp$ds,"mdy"))
      tseries <- read.zoo(df_temp)
      tseries_ts <- as.ts(tseries)
      tseries_ts <- na.remove(tseries_ts)
      model <- auto.arima(tseries_ts) 
      arima <<- forecast(model,input$days)
      tmp <<-fortify(arima, ts.connect = TRUE)
      tmp$Index<<-as.Date(tmp$Index)
    }
  })
  observeEvent(input$do_model,{
    if(!is.null(dataset) & !is.null(input$var_y)){
      dataset %>% select(Date,input$var_y) -> df_temp
      names(df_temp)<-c('ds','y')
      df_temp$y <- as.numeric(gsub('[$,]', '', df_temp$y))
      df_temp$ds <-as.Date(parse_date_time(df_temp$ds,"mdy"))
      m <<- prophet(df_temp,daily.seasonality=TRUE)
      future <- make_future_dataframe(m, periods = input$days)
      future %>% filter(!wday(ds)==7 &
                          !wday(ds)==1) -> future
      forecast <<- predict(m, future)
    }
  })
  output$selected_var <- renderText({
    inFile <- input$file1
    if (is.null(inFile)| is.null(input$var_y) ){
      
    }
    else{
      paste("You have selected", input$var_y)
    }

  })
  output$plot1 <- renderDygraph({
    if(!is.null(forecast)){
      dyplot.prophet(m, forecast)
    }
  })
  
  output$plot2 <- renderPlotly({
    if(!is.null(arima)){
      Date <- tmp$Index
      Historical_data <- tmp$Data
      Forecast_data <- tmp$`Point Forecast`
      plot_arima<-ggplot() + geom_line(aes(x=Date,y=Historical_data),color='black') + geom_line(aes(x=Date,y=Forecast_data),color='blue') +
      ylab('Values')+xlab('date') 
      ggplotly(plot_arima)
    }
  })
  output$instr1 <- renderText({
    paste("Hi,Welcome and Hello to my thesis!
          To create a model simply go to one of these links:")
  })
  url_euronext <- a("Euronext", href="https://live.euronext.com/en")
  url_nasdaq <- a("NASDAQ", href="https://www.nasdaq.com/market-activity/quotes/historical")
  output$instr2 <- renderText({
    paste("download csv files of historical data of the stock that you choose !!
          *remember to choose dd/mm/yy as date type and .csv as file type in euronext :)")
  })
  output$instr3 <- renderText({
    paste("Choose how many days into the future do you want your preditcion to be!")
  })
  output$instr_prophet <- renderText({
    paste("Below is the Prophet predictions presented in the form of a graph")
  })
  output$instr_arima <- renderText({
    paste("Below is the ARIMA predictions presented in the form of a graph")
  })
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out-alt"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("admin", tabName = "dashboard", icon = icon("cog"),selected = 1)
        )
      }
      else{
        sidebarMenu(
          menuItem("user area", tabName = "dashboard", icon = icon("chart-line"),selected = 1)
        )
        
      }
    }
  })
  
  output$download_prophet <- downloadHandler(
    filename = function() {
      paste("model_prophet-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(forecast, file)
    }
  )
  
  output$download_arima <- downloadHandler(
    filename = function() {
      paste("model_arima-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tmp, file)
    }
  )
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",textInput("nazwa_prez", "User Name", NULL),
            textInput("cena_prez","Users Password"),actionButton("addrows", "Add user"),actionButton("kup_prez", "Change permisions")
            ,actionButton("deleteRows", "remove user"),column(DT::dataTableOutput("table1") ,width = 6)
          )
          
          
          
        )
      } #
      else {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            textOutput("instr1"),
            tagList("Euronext historical data:", url_euronext),
            tagList("NASDAQ historical data:", url_nasdaq),
            textOutput("instr2"),
            div(style="display:inline-block",fileInput("file1", "Choose CSV File from euronext or NASDAQ",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            )),
            div(style="display:inline-block",checkboxInput("head", "Show full data set", FALSE)),
            selectInput("var_y", "Choose a variable to predict:",

                          names(dataset)[names(dataset) != "Date"]
            ),
            textOutput("instr3"),
            sliderInput("days", "Number of days:",
                        min = 1, max =730 , value = 20
            ),
            DT::dataTableOutput("contents"),
            actionButton("do_model", " Click Me to generate your prohet model ! "),
            actionButton("do_model_arima", " Click Me to generate your arima model ! "),
            textOutput("instr_prophet"),
            dygraphOutput("plot1"),
            textOutput("instr_arima"),
            plotlyOutput("plot2"),
            downloadButton("download_prophet", "Download Prophet model"),
            downloadButton("download_arima", "Download Arima model")
            )
          
          
          
        )
      }
      
    }
    else {
      loginpage
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)
