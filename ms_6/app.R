library(shiny)
library(tidyverse)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(shinyWidgets)

### Read in Data
drugoverdosedeaths<- read.csv("VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

drugoverdosedeaths <- drugoverdosedeaths %>%
  clean_names() %>%
  select("state", "year", "month", "indicator", 
         "data_value", "state_name", "predicted_value") %>%
  filter(str_detect(indicator, "Drug Overdose Deaths")) %>%
  filter(state_name != "United States") %>%
  mutate(time = paste(month, year, sep = " ")) %>%
  mutate(time=mdy(time) - 19) %>%
  mutate(month_number = month(time)) %>%
    group_by(time) %>%
    arrange(time)
drugoverdosedeaths$data_value <-as.numeric(drugoverdosedeaths$data_value)
drugoverdosedeaths$predicted_value <-as.numeric(drugoverdosedeaths$predicted_value)

ui <- fluidPage(
  titlePanel("Drug Overdoses by State Over Time"),
  sliderTextInput("time", "Time", 
                            from_min = as.Date("2015-01-01"),
                            from_max =as.Date("2019-07-01"),
                            timeFormat="%b %Y",
                            choices = levels(as.factor(drugoverdosedeaths$time))),
                plotOutput("overdose_counts")
)

server <- function(input, output, session) {
    output$overdose_counts <- renderPlot({
       drugoverdosedeaths2<- drugoverdosedeaths %>%
        filter(time == input$time)
        ggplot(drugoverdosedeaths2, aes(x= state_name, y= data_value)) +
            geom_col() +
            labs(title = "Drug Overdose Deaths by State Over Time", 
                 x = "State", 
                 y = "Drug Overdose Deaths") +
            
            theme_classic()+ 
            
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
}
shinyApp(ui, server)
