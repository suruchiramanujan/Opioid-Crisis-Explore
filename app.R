library(shiny)
library(tidyverse)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)
library(maps)

### Read in Data

# https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=3F8131C534663BDD3F79E66994FCF12E

death_by_year_and_state <- read_csv("Drug Overdose Deaths by Year and State.csv")

us_state <- map_data("state") %>%
  rename(state_full = region)

death_by_year_and_state <- death_by_year_and_state %>%
  filter(`UCD - Drug/Alcohol Induced Code` == "D") %>%
  rename(state_full = State) %>%
  mutate(state_full = tolower(state_full)) %>%
  left_join(us_state, by = "state_full" ) %>%
  drop_na(`Crude Rate`)

death_by_year_and_state$`Crude Rate` <-as.numeric(death_by_year_and_state$`Crude Rate`)

ui <- navbarPage(theme = shinytheme("flatly"),
                 "Opioid Trends Across the United States",
                 tabPanel("About",
                          column(7,
                                 h1("Background"),
                                 p("This project is meant to look into trends 
                                  regarding the issue of opioid overuse. In the
                                   1990's doctors overprescribed pain relievers 
                                   as a temporary bandage to disease without 
                                   knowing the potential for addiction. 
                                   Since then, the misuse of opioids has ignited 
                                   a national crisis that has contributed to 
                                   worsening health outcomes for thousands and 
                                   their families throughout the United States. 
                                   Opioid misuse is not only an emotional burden 
                                   for patients and their families, 
                                   it also loses the United States billions of 
                                   dollars yearly. Thus, this study looks into 
                                   some of the spending trends of opioid use in 
                                   conjunction with death rates."),
                                 
                                 p("The plan for this project is to examine the 
                                   distribution of opioid deaths across states
                                   over the past twenty years. Having looked at 
                                   this data, I also sought to look at the 
                                   distribution of treatment centers to see if 
                                   the treatment center distribution matches up 
                                   with opioid death distribution: are the 
                                   states that need the most attention getting 
                                   the most attention?"),
                                 
                                 p("Following this, I analyzed the data for 
                                   the state nearest and dearest to our hearts, 
                                   Massachusetts. How does the distribution 
                                   of opioid deaths in Massachusetts look and 
                                   how are treatment centers distributed 
                                   according to deaths per capita in 
                                   different counties?"),
                                
                                 h1("The Data"),
                                 p("Most data comes from ________"),
                                 p("My project code can be found on my",
                                   a("GitHub",
                                     href = "https://github.com/suruchiramanujan/final-project.git",)),
                                 h1("About Me"),
                                 p("My name is Suruchi Ramanujan and I am a 
                                   senior in Quincy House studying 
                                   Molecular and Cellular Biology. In my free time, 
                                   I enjoy performing Indian Classical dance, running, 
                                   and grinding through my DataCamp exercises."),
                                 p("You can reach me at ",
                                   a("suruchi_ramanujan@college.harvard.edu",
                                     href = "mailto: suruchi_ramanujan@college.harvard.edu",),
                                   "or ",
                                   a("LinkedIn",
                                     href = "https://www.linkedin.com/in/suruchi-ramanujan-791007115/")))),
                          
                 tabPanel("United States Opioid Data",
                          h1("Violent Crimes"),
                            column(7,
                                   p("The graph below shows CDC data for the number of deaths per year for the two top leading causes of death 
                                     in the United States: heart disease or cardiovascular disease (CVD) and cancer. 
                                     Simultaneously, I have graphed the number of deaths per year caused by opioid overdoses in the United States.")),
                          column(7, 
                                 imageOutput("commondiseases", height = "100%"),
                                 p("While this graph may make it look like opioids do not have as great a toll on the United States
                                   as other diseases like cancer and cardiovascular disease, just because fewer people are affected
                                   does not mean the issue is less important. In fact, if we normalize the number of deaths due to
                                   opioid use to those seen in 1990, we see the greatest proportion change in deaths by opioid overdose
                                   compared to cancer and cardiovascular disease as shown below. Thus, even though we are slowly making the changes
                                   needed to curtail cancer and cardiovascular disease deaths, we are still seeing sharp increases in death by
                                   opioid overdose.")),
                          column(7, 
                                 imageOutput("propcommondiseases", height = "100%", width = "100%"),
                                 p("However, the distribution of opioid overdose deaths per capita differs based on state.
                                   The graph below shows how opioid deaths per capita changed between 1998 and 2018.")),
                            column(7, 
                                   fluidPage(
  titlePanel("Drug Overdoses Per Capita by State Over Time"),
  sliderTextInput("Year", "Year", 
                            from_min = 1998,
                            from_max =2018,
                            choices = levels(as.factor(death_by_year_and_state$Year))),
                plotOutput("overdose_counts")
               
))),
tabPanel("Massachusetts Opioid Data",
         # find a way to increase width
         column(7, 
                imageOutput("madeathrates", height = "100%")))
         
         )

server <- function(input, output, session) {
    output$overdose_counts <- renderPlot({
      death_by_year_and_state %>%
      filter(Year == input$Year) %>%
        ggplot(aes(x = long, y = lat,
                             fill = `Crude Rate`, group = group)) + 
        geom_polygon(color = "gray90", size = 0.05) + 
        theme_map() +
        scale_fill_gradient(low = "white", high = "#CB454A") +
        guides(fill = guide_legend(nrow = 1)) + 
        theme(legend.position = "bottom")
    })
    
    output$madeathrates <- renderImage({
      # Return a list containing the filename
      list(src = "plot_mavsus_death.gif",
           contentType = 'image/gif'
           # width = 800,
           # height = 600,
           # alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    output$commondiseases <- renderImage({
      # Return a list containing the filename
      list(src = "plot_deathsbycommondiseases.gif",
           contentType = 'image/gif'
           # width = 800,
           # height = 600,
           # alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    output$propcommondiseases <- renderImage({
      # Return a list containing the filename
      list(src = "prop_deathsbycommondiseases.gif",
           contentType = 'image/gif'
           # width = 800,
           # height = 600,
           # alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
}
shinyApp(ui, server)
