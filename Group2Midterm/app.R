library(shiny)
library(DT)
library(tidyverse)

# What pesticides do each state use and how toxic are they?

# source("data_wrangle.R")
load("data/final.rda")
load("data/AVG.rda")

strawbPesti$Year <- as.character(strawbPesti$Year)

library(shiny)

ui <- fluidPage(
  navbarPage("Strawberry Data System",
             
             navbarMenu("Visualization", 
                        
                        tabPanel("Table",
                                 fluidRow(
                                   column(4, 
                                          selectInput(inputId = "state", label = "Please Select the State", c("All",unique(strawbPesti$State)))
                                   ),
                                   column(4, 
                                          selectInput("year", "Year", c("All", unique(strawbPesti$Year)))
                                   ),
                                   column(4, selectInput("type", "Type", c("All", unique(strawbPesti$type)))
                                   ),
                                 ),
                                 dataTableOutput("table"),
                        ),
                        tabPanel("Plot",
                                 selectInput("state1", "Please Select the State", c("All", unique(strawbPesti$State))),
                                 uiOutput("plot")
                        )
             ),
             tabPanel("Map")
  )
  
  
)



server <- function(input, output, session) {
  
  # Original dataset
  df1 <- strawbPesti
  df2 <- strawbPestiAVG
  
  # Data output in table format
  output$table <- DT::renderDataTable({
    if (input$state != "All") {
      df1 <- df1[df1$State == input$state,]
    }
    if (input$year != "All") {
      df1<- df1[df1$Year == input$year,]
    }
    if (input$type != "All") {
      df1 <- df1[data$type == input$type,]
    }
    return(df1)
  }
  )
    
  output$plot <- renderUI({
    
    if (input$state1 != "All") {
      df2 <- df2[df2$State == input$state1,]
      bar <- ggplot(df2)+
        geom_bar(mapping = aes(x=`Chemical Name`,y=Value,fill=`Human Toxins`), position = "dodge",stat = "identity")+
        scale_fill_manual(values = c("high" = "#D55E00", "moderate"="#E69F00", "slight"="#009E73"))+
        labs(title = input$state1)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      bar2 <- ggplot(df2)+
        geom_bar(mapping = aes(x=`Chemical Name`,y=Value,fill=`Human Toxins`), position = "dodge",stat = "identity")+
        scale_fill_manual(values = c("high" = "#D55E00", "moderate"="#E69F00", "slight"="#009E73"))+
        labs(title = input$state1)
      ggplotly(bar)
    } 
    
    if (input$state1 == "All") {
      p <- ggplot(df1,aes (x= State, y= `Chemical Name`, color= `Human Toxins`)) + 
        geom_point() + labs(title= 'Chemicals Used by Each State') +
        scale_color_manual(values = c("high" = "#D55E00", "moderate"="#E69F00", "slight"="#009E73"))   #colors used are from the color blind palette
      ggplotly(p, height = 1100)
    } 
  }
  )
}

shinyApp(ui, server)
