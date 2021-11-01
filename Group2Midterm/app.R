library(shiny)
library(tidyverse)
library(ggplot2)

# what pesticides do each state use and how toxic are they?
states <- c("WASHINGTON", "OREGON", "CALIFORNIA", "FLORIDA")


# UI 
ui <- fluidPage(
  
  # Indicate the State
  selectInput(inputId = "state", label = "Please Select the State", states),
  
  # Setup table output
  tableOutput('table')
)


# Server
# Server function has some problem and I did not fix it. Hopefully someone can help.
server <- function(input, output, session) {
 
  # Data output in table formate
  output$table <- renderTable(
   if (input$state == "CALIFORNIA") {
     df1 <- strawbPesti %>% filter(strawbPesti$State == input$state)
     return(df1)
   } else if (input$state == "WASHINGTON") {
     df2 <- strawbPesti %>% filter(strawbPesti$State == input$state)
     return(df2)
   } else if (input$state == "OREGON") {
     df3 <- strawbPesti %>% filter(strawbPesti$State == input$state)
     return(df3)
   } else {
     df4 <- strawbPesti %>% filter(strawbPesti$State == input$state)
     return(df4)
   }
 )
 
 
 
}

shinyApp(ui, server)
