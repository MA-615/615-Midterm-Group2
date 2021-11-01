library(shiny)


# What pesticides do each state use and how toxic are they?

# source("data_wrangle.R")
load("data/final.rda")
strawbPesti$Year <- as.character(strawbPesti$Year)

# UI 
ui <- fluidPage(
  titlePanel("Strawberry Data"),
  navlistPanel(
  
    # Title 1 for the section 
    "Data",  
  
     # Subtitle 1:
      tabPanel("About", h3("Introduction of Dataset")),
  
     # Subtitle 2:
      tabPanel("Table",
  
      # input indicator:
        selectInput(inputId = "state", label = "Please Select the State", c("All",unique(strawbPesti$State))),
        selectInput("year", "Year", c("All", unique(strawbPesti$Year))),
      
      # output dataset
        dataTableOutput('table1')),
    "Plot"
    )
)


# Server

server <- function(input, output, session) {
 data <- strawbPesti
 
   # Data output in table format
   output$table1 <- renderDataTable({
     if (input$state != "All") {
       data <- data[data$State == input$state,]
     }
     if (input$year != "All") {
       data <- data[data$Year == input$year,]
     }
     return(data)
   }
   )
}
# Debug
# runApp("Group2Midterm/app.R", display.mode = "showcase")

# Running the app.
shinyApp(ui, server)

