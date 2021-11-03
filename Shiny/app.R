library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)


load("AVG.rda")
df1 <- read_csv("total.csv")
df2 <- strawbPestiAVG
df3 <- read_csv("partial.csv")

df1$Year <- as.character(df1$Year)
df1$type <- as.character(df1$type)



#################################################################################################
#################################################################################################

# UI:
# Dashboard Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(

        menuItem("INTRODUCTION", tabName = "introl", icon = icon("info-circle")
        ),
    menuItem("MAP", icon = icon("map", tabName = "map")
    ),
    menuItem("CHART", icon = icon("signal"), tabName = "chart",
             
             menuSubItem("TABLE", icon = icon("th-list",  lib = "glyphicon"), tabName = "list"),
             menuSubItem("PLOT", icon = icon("chart-pie"), tabName = "plot")
    ),
    menuItem("COPYRIGHT", icon = icon("copyright-mark",  lib = "glyphicon"), tabName = "cpyr")
  )
)

#################################################################################################
#################################################################################################
# Dashboard Body
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "introl"),
    
    tabItem(tabName = "map",
    ),
    
    tabItem(tabName = "chart", "This Page Show Two Different Chart for Data Visualization"),
    
    tabItem(tabName = "list",
            fluidRow(
              column(4,
                     selectInput(inputId = "state", label = "Please Select the State", c("All",unique(df1$State)))
              ),
              column(4,
                     selectInput("year", "Year", c("All", unique(df1$Year)))
              ),
              column(4, selectInput("type", "Type", c("All", unique(df1$type)))
              ),
              column(4,
                     selectInput("des", "Description", c("All", unique(df1$Description))),
              ),
            ),
            dataTableOutput("list")
    ),
    
    tabItem(tabName = "plot",
            selectInput("state1", "Please Select the State", c("All", unique(df1$State))),
            uiOutput("plot")
    ),
    
    tabItem(tabName = "cpyr", "© 2021 MA615 Midterm Group 2")
    
  )
  
)



#################################################################################################
#################################################################################################
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Strawberry Data System"),
                    sidebar,
                    body
)


server <- function(input, output, session) {
  # Original dataset
  dta1 <- df1
  dta2 <- df2
  dta3 <- df3
  
  # output: list
  output$list <- DT::renderDataTable({
    if (input$state != "All") {
      dta1 <- dta1[dta1$State == input$state,]
    }
    if (input$year != "All") {
      dta1<- dta1[dta1$Year == input$year,]
    }
    if (input$type != "All") {
      dta1 <- dta1[dta1$type == input$type,]
    }
    if (input$des != "All") {
      dta1 <- dta1[dta1$Description == input$des,]
    }
    return(dta1)
  }
  )
  
  # output: plot
  output$plot <- renderUI({
    
    if (input$state1 != "All") {
      if (input$state1 == "CALIFORNIA" | input$state1 == "FLORIDA" | input$state1 == "WASHINGTON") {
        dta2 <- dta2 %>% filter(State == input$state1)
        bar1 <- plot_ly(dta2, x=~`Chemical Name`,y=~Value, color = ~`Human Toxins`, type = "bar", height = 1100)
        bar2 <- plot_ly(dta2, labels =~`Chemical Name`, values =~Value, color = ~`Human Toxins`, type = "pie", height = 500) # use box()
        subplot(
          ggplotly(bar1), ggplotly(bar2))
        
      } else {print("NO DATA AVALIABLE AT THIS TIME")}
    }
    
    else if (input$state1 == "All") {
      plot_ly(data = dta3, x= ~State, y= ~`Chemical Name`, color= ~`Human Toxins`, type = "scatter", height = 1100)
    }
  }
  )
  
}


shinyApp(ui = ui, server = server)

