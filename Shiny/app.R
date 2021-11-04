library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(maps)
library(leaflet)
library(jpeg)
library(geojsonio)


# Load the data we will use for the shiny dashboard
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
    
    menuItem("MAP", icon = icon("map"), tabName = "map"
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
    tabItem(tabName = "introl",
            h1("Welcome to Use Strawberry Data System!"),
            h4("This App might help you to explore the strawberry data you want for the United States"),
            h4("Please kepp in mind, the data we have currently work only for a few states!"),
            h4("Recommend: Using Your local browser to open it."),
            imageOutput("pic")
            ),
    
    tabItem(tabName = "map",
            uiOutput("map")
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
              column(4, selectInput("type", "Type", c("All", "FUNGICIDE", "HERBICIDE", "INSECTICIDE","OTHER"))
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
  
  # output: pic
  output$pic <- renderImage(
    list(src = "Strawberries.jpg", height = "685", width = "1375", style="display: block; margin-left: auto; margin-right: auto;"), 
    deleteFile = F
  )
  
  # output: map
  output$map <- renderUI({
    states <- geojson_read( 
      x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
      , what = "sp"
    )
    
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      states$name, states$density
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states, height = 850) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(density),
        weight = 1.75,
        opacity = 0.75,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.75,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addProviderTiles(providers$CartoDB.Voyager)
  })
  
   # output: list
  output$list <- DT::renderDataTable({
    if (input$state != "All") {
      dta1 <- dta1[dta1$State == input$state,]
    }
    if (input$year != "All") {
      dta1<- dta1[dta1$Year == input$year,]
    }
    if (input$type != "All") {
      dta1 <- dta1 %>% filter(!is.na(type))
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
        plot_ly(dta2, x=~`Chemical Name`,y=~Value, color = ~replace(`Human Toxins`, is.na(`Human Toxins`), "NA"), type = "bar", height = 700)
      } else {print("NO DATA AVALIABLE AT THIS TIME")}
    }
    
    else if (input$state1 == "All") {
      plot_ly(data = dta3, x= ~State, y= ~`Chemical Name`, color= ~replace(`Human Toxins`, is.na(`Human Toxins`), "NA"), type = "scatter", height = 800)
    }
  }
  )
  
}

shinyApp(ui = ui, server = server)

