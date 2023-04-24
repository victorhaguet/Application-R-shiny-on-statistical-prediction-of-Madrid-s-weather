#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library("imputeTS")
library("tidyverse")
library(ggplot2)
library(lubridate)
library(date)
library(dplyr)
library("plotly")

# chargement des modèles

load("models.Rdata")
data=data.new

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  header = dashboardHeader(title = "Météo Madrid"),
  
  sidebar = dashboardSidebar(                 
                   # Application title
                   div("Prédictions météorologiques de Madrid : ", style = "font-size:120%"),
                   
                   # add a date input widget
                   dateInput("date",
                             label = "Date",
                             value = data$CET[6812-364],
                             min=data$CET[6812-364],
                             max = data$CET[6812],
                             width = "400px",
                             format="dd/mm/yyyy"),
                   textOutput("date")),
  
  body=dashboardBody(tags$head(tags$style(HTML(".small-box {height:100px}"))),
        tabsetPanel(
          tabPanel("Prédictions de l'année 2015",
                   
                   # temperature 
                   div(class="container-fluid",
                       fluidRow(
                        valueBoxOutput("mintemp", width = 4),
                        valueBoxOutput("meantemp",width = 4),
                        valueBoxOutput("maxtemp",width = 4)
                       )
                   ),
                   
                   
                   # Seconde ligne d'information
                   div(class="container-fluid",
                       fluidRow(
                         valueBoxOutput("humidity",width = 4),
                         valueBoxOutput("wind",width = 4),
                         valueBoxOutput("event", width = 4)
                       )
                   ),
        
                   
                   # Evolution de la température du mois
                   
                   plotlyOutput("plot1"),
          
                  # R2 du modèle meanTemp
          
                  div(fluidRow(br(),
                    column(width=12, offset=8, valueBoxOutput('r2', width=4))
                    ))),
          
          tabPanel('Vos prédictions',
                           
                           # Application title
                           titlePanel(
                             h1("Renseignez vos températures : ",align="center")
                           ),
                           
                           div(class="val-to-chose",
                               fluidRow(
                                 column(3, selectInput('select_temp',
                                                       label = 'Quelles données voulez vous prédire ?',
                                                       choices=c("Température minimale", "Température moyenne", "Température maximale"),
                                                       selected="Température moyenne")),
                                 column(3,numericInput(inputId = 'temp_1',
                                                       label = 'Température Jour-1 °C',
                                                       value = round(mean(data$Mean.TemperatureC)),
                                                       min = min(data$Mean.TemperatureC)-10,
                                                       max = max(data$Mean.TemperatureC)+10,
                                                       step = 1)),
                                 column(3,numericInput(inputId = 'temp_2',
                                                       label = 'Température Jour-2 °C',
                                                       value = round(mean(data$Mean.TemperatureC)),
                                                       min = min(data$Mean.TemperatureC)-10,
                                                       max = max(data$Mean.TemperatureC)+10,
                                                       step = 1)),
                                 column(3,numericInput(inputId = 'temp_3',
                                                       label = 'Température Jour-3 °C',
                                                       value = round(mean(data$Mean.TemperatureC)),
                                                       min = min(data$Mean.TemperatureC)-10,
                                                       max = max(data$Mean.TemperatureC)+10,
                                                       step = 1))
                     
                               )
                           ),
                          div(class='prediction',
                              fluidRow(align='center',
                                column(width =8,valueBoxOutput('pred', width = 12), step=1),
                                column(width=4, valueBoxOutput('r2_pred', width=12)))
                              )
                          )
                     )
        )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Temperature
  output$mintemp <- renderValueBox({
    valueBox(
      paste(as.character(round(data$predMintemp[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))])),"C°"),
      "Température minimale", icon = tags$i(icon("temperature-low"),style="font-size: 60px"),
      color = "light-blue"
    )
  })
    
  output$meantemp <- renderValueBox({
    valueBox(
    paste(as.character(round(data$predMeanTemp[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))])),"C°"),
    "Température moyenne", icon = tags$i(icon("thermometer-half"),style="font-size: 60px"),
    color = "yellow"
    )
  })
  output$maxtemp <- renderValueBox({
    valueBox(
    paste(as.character(round(data$predMaxtemp[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))])),"C°"),
    "Température maximale", icon = tags$i(icon("temperature-high"),style="font-size: 60px"), 
    color = "red"
    )
  })
  
  # Informations suppélementaires
  output$humidity <- renderValueBox({
    valueBox(
    as.character(round(data$predHumi[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))])/100),
    "Taux d'humidité", icon=tags$i(icon("tint"),style="font-size: 60px"),
    color='blue'
    )
  })
  output$wind <- renderValueBox({
    valueBox(
    paste(as.character(round(data$predVent[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))]),1),"km/h"),
    "Vitesse du vent", icon=tags$i(icon('wind'),style="font-size: 60px")
    )
  })
  output$event <- renderValueBox({
    valueBox(
    tags$p(as.character(data$predEvent[which(format(data$CET,"%d/%m/%Y") == format(input$date,"%d/%m/%Y"))]),style = "font-size: 55%;"),
    "Evènements", icon=tags$i(icon('umbrella'),style="font-size: 60px"),
    color="green"
    )
  })
  
  # R2
  
  output$r2 <- renderValueBox({
    valueBox(
    as.character(round(r2,2)), "R-squared",
    icon = tags$i(icon('percent'), style="font-style: 60px"), 
    color="purple"
    )
  })
  
  # pred client
  
  predmoy = reactive({
    if (input$select_temp == "Température moyenne"){
      round(sum(summary(regManuelMean)$coef[c(1,2,3,4)]*c(1, input$temp_1, input$temp_2, input$temp_3)))
    }else if(input$select_temp == "Température minimale"){
      round(sum(summary(regManuelMin)$coef[c(1,2,3,4)]*c(1, input$temp_1, input$temp_2, input$temp_3)))
    }else{
      round(sum(summary(regManuelMax)$coef[c(1,2,3,4)]*c(1, input$temp_1, input$temp_2, input$temp_3)))
    }
  })
  
  output$pred <- renderValueBox({
    if (input$select_temp == "Température moyenne"){
      valueBox(
        paste(as.character(predmoy()),'°C'), 
        "Température moyenne", icon = tags$i(icon("thermometer-half"),style="font-size: 60px"),
        color = "yellow"
      )
    }else if(input$select_temp == "Température minimale"){
      valueBox(
        paste(as.character(predmoy()),'°C'), 
        "Température minimale", icon = tags$i(icon("temperature-low"),style="font-size: 60px"),
        color = "light-blue"
      )
    }else{
      valueBox(
        paste(as.character(predmoy()),'°C'), 
        "Température maximale", icon = tags$i(icon("temperature-high"),style="font-size: 60px"),
        color = "red"
      )
    }
    
  })
  
  r2p <- reactive({
    if (input$select_temp == "Température moyenne"){
      summary(regManuelMean)$r.squared
    }else if(input$select_temp == "Température minimale"){
      summary(regManuelMin)$r.squared
    }else{
      summary(regManuelMax)$r.squared
    }
  })
  
  output$r2_pred <- renderValueBox({
    valueBox(
      as.character(round(r2p(),2)), "R-squared",
      icon = tags$i(icon('percent'), style="font-style: 60px"), 
      color="purple"
    )
  })
  
  # plot de la température moyenne sur le mois
  
  month = reactive({
    as.character(seq.Date(
      floor_date(input$date, 'month'),ceiling_date(input$date, 'month')-days(1),
      by='day'))
  })
  values = reactive ({
    data$Mean.TemperatureC[seq(which(as.character(data$CET) == month()[1]),which(as.character(data$CET) == month()[length(month())]), by=1)]
  })
  pred = reactive ({
    data$predMeanTemp[seq(which(as.character(data$CET) == month()[1]),which(as.character(data$CET) == month()[length(month())]), by=1)]
  })
  df = reactive({ 
    data.frame(month=month(),values=values(), pred=pred()) 
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(ggplot(df())+
      geom_point(aes(x = month, y=values))+
      geom_line(aes(x=month, y=pred), group=1, color = 'red')+
      labs(x = "Jours", y = "Température moyenne", title = paste("Température moyenne sur le mois : ", format(input$date, "%m-%Y")))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)))%>%
      config(displayModeBar = F)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
