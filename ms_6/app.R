library(shiny)
library(tidyverse)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)

### Read in Data
drugoverdosedeaths<- read.csv("VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

drugoverdosedeaths <- drugoverdosedeaths %>%
    clean_names() %>%
    select("state", "year", "month", "indicator", 
           "data_value", "state_name", "predicted_value") %>%
    filter(str_detect(indicator, "Drug Overdose Deaths")) %>%
    filter(state_name != "United States") %>%
    mutate(time = paste(month, year, sep = " ")) %>%
    mutate(time=mdy(time)) %>%
    group_by(time) %>%
    arrange(time)
drugoverdosedeaths$data_value <-as.numeric(drugoverdosedeaths$data_value)
drugoverdosedeaths$predicted_value <-as.numeric(drugoverdosedeaths$predicted_value)

ui <- fluidPage(
    titlePanel("Drug Overdoses by State Over Time"),
    sliderInput("time", "Time Slider", min(drugoverdosedeaths$time),
    max(drugoverdosedeaths$time),
              value = min(drugoverdosedeaths$time),
              step = 1),
    plotOutput("overdose_counts")
)


server <- function(input, output, session) {
    output$overdose_counts <- renderPlot({
       drugoverdosedeaths2<- drugoverdosedeaths %>%
        filter(time == input$time)
        ggplot(drugoverdosedeaths2, aes(x= reorder(state_name,-data_value), y= data_value)) +
            geom_col() +
            labs(title = "Drug Overdose Deaths by State Over Time", 
                 x = "State", 
                 y = "Drug Overdose Deaths") +
            
            theme_classic()+ 
            
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
}
shinyApp(ui, server)
