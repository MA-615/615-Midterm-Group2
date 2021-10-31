library(shiny)
library(tidyverse)
library(ggplot2)

# what pesticides do each state use and how toxic are they?
df <- read.csv("data/final.csv")

states <- c("NEW YORK", "WASHINGTON", "OREGON", "NORTH CAROLINA", "CALIFORNIA", "FLORIDA")


# UI 
ui <- fluidPage(
  selectInput("state", "Please Select the State", states),
  tableOutput("table")
)


# Server
# Server function has some problem and I did not fix it. Hopefully someone can help.
server <- function(input, output, session) {
  input$state <- renderText(input$state)
  if(input$state == "CALIFORNIA") {
    df1 <- df %>% filter(df$State == "CALIFORNIA")
    output$table <- renderTable(df1)
  } else if (input$state == "NEW YORK") {
    df2 <- df %>% filter(df$State == "NEW YORK")
    output$table <- renderTable(df2) 
  } else if (input$state == "WASHINGTON") {
    df3 <- df %>% filter(df$State == "WASHINGTON")
    output$table <- renderTable(df3) 
  } else if (input$state == "OREGON") {
    df4 <- df %>% filter(df$State == "OREGON")
    output$table <- renderTable(df4) 
  } else if (input$state == "NORTH CAROLINA") {
    df5 <- df %>% filter(df$State == "NORTH CAROLINA")
    output$table <- renderTable(df5) 
  } else if (input$state == "FLORIDA") {
    df6 <- df %>% filter(df$State == "FLORIDA")
    output$table <- renderTable(df6) 
  } else stop("Warning")
}

shinyApp(ui, server)
