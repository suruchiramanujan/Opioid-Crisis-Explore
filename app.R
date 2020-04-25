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
library(leaflet)

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

treatment_locations <- read_csv("Drug Treatment Centers.csv") %>%
  select(`Program Name`, Street, City, State, Zipcode)

# state populations

state_pop <- read_csv("State Population Estimate.csv") %>%
  rename(state_full = NAME) %>%
  mutate(state_full = tolower(state_full))

# https://simplemaps.com/data/us-cities

longlatinfo <- read_csv("uscitieslonglat.csv") %>%
  rename(City = city) %>%
  rename(State = state_id)

treatment_locations_map <- treatment_locations %>%
  left_join(longlatinfo, by = c("City", "State")) %>%
  group_by(State) %>%
  mutate(count = n()) %>%
  mutate(state_name = tolower(state_name)) %>%
  rename(state_full = state_name) %>%
  left_join(us_state, by = "state_full") 

treatment_locations_map_per_cap <- treatment_locations_map %>%
  left_join(state_pop, by = "state_full") %>%
  group_by(State) %>%
  mutate(count_per_pop = n()/POPESTIMATE2019)

treatment_locations_map_mas <- treatment_locations %>%
  left_join(longlatinfo, by = c("City", "State")) %>%
  filter(State == "MA") %>%
  drop_na()

counties <- read_csv("zip_codes_states.csv") %>%
  filter(state == "MA") %>%
  rename(County = county) %>%
  rename(Municipality = city)

madeathbycounty <- read_csv("MAAverageAnnualOpioidRelatedDeathRateper100,000People.csv")

countypop <- read_csv("countypop.csv") %>%
  mutate(Pop = Pop/100000) %>%
  rename(subregion = CTYNAME) %>%
  mutate(subregion = tolower(subregion)) %>%
  select(subregion, Pop)

madeathbycountywlonglat <- madeathbycounty %>%
  left_join(counties, by = "Municipality")

madeathbycountywlonglat <- madeathbycountywlonglat %>%
  select(County,
         `Confirmed Opioid Related Death Count 2001-2005`,
         `Confirmed Opioid Related Death Count 2006-2010`,
         `Confirmed Opioid Related Death Count 2011-2015`,
         `latitude`,
         `longitude`,
         `Municipality`) %>%
  distinct(Municipality, .keep_all= TRUE) %>%
  na.omit() %>%
  rename(subregion = County) %>%
  mutate(subregion = tolower(subregion)) %>%
  group_by(subregion) %>%
  mutate(total_deaths_2001.5 = sum(`Confirmed Opioid Related Death Count 2001-2005`)) %>%
  mutate(total_deaths_2006.10 = sum(`Confirmed Opioid Related Death Count 2006-2010`)) %>%
  mutate(total_deaths_2011.15 = sum(`Confirmed Opioid Related Death Count 2011-2015`)) %>%
  distinct(subregion, .keep_all= TRUE)

us_county <- map_data("county") %>%
  filter(region == "massachusetts")

# https://www.indexmundi.com/facts/united-states/quick-facts/massachusetts/percent-of-people-of-all-ages-in-poverty#table

povertybycounty <- read_csv("PovertyByCounty.csv") %>%
  rename(subregion = County) %>%
  mutate(subregion = tolower(subregion))

full_data <- us_county %>%
  left_join(madeathbycountywlonglat, by = "subregion") %>%
  left_join(countypop, by = "subregion") %>%
  left_join(povertybycounty, by = "subregion") %>%
  mutate(percap_2001.5 = total_deaths_2001.5/Pop) %>%
  mutate(percap_2006.10 = total_deaths_2006.10/Pop) %>%
  mutate(percap_2011.15 = total_deaths_2011.15/Pop) 

ui <- navbarPage(theme = shinytheme("flatly"),
                 "Opioid Trends Across the United States",
                 tabPanel("About",
                          column(7,
                                 h1("Background"),
                                 p("Opioids have been prescribed for pain management for many decades, 
                                 although they are effective at treating acute, not chronic pain. 
                                   In treating chronic pain, opioids can be more harmful than useful. 
                                   Opioids are cheap, but have high rates of addiction and can lead to 
                                   dependence and ultimately death from overdose."),
                                 
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
                          h1("Opioid Deaths in America"),
                            column(7,
                                   p("The graph below shows CDC data for the number of deaths per year for the two top leading causes of death 
                                     in the United States: heart disease or cardiovascular disease (CVD) and cancer. 
                                     Simultaneously, I have graphed the number of deaths per year caused by opioid overdoses in the United States."),
                                 imageOutput("commondiseases", height = "100%"),
                                 p("While this graph may make it look like opioids do not have as great a toll on the United States
                                   as other diseases like cancer and cardiovascular disease, just because fewer people are affected
                                   does not mean the issue is less important. In fact, if we normalize the number of deaths due to
                                   opioid use to those seen in 1990, we see the greatest proportion change in deaths by opioid overdose
                                   compared to cancer and cardiovascular disease as shown below. Thus, even though we are slowly making the changes
                                   needed to curtail cancer and cardiovascular disease deaths, we are still seeing sharp increases in death by
                                   opioid overdose."),
                                 imageOutput("propcommondiseases", height = "100%", width = "100%"),
                                 p("However, the distribution of opioid overdose deaths per capita differs based on state.
                                   The graph below shows how opioid deaths per capita changed between 1998 and 2018."),
                                   fluidPage(
                  titlePanel("Drug Overdoses Per Capita by State Over Time"),
                  sliderTextInput("Year", "Year", 
                            from_min = 1998,
                            from_max =2018,
                            choices = levels(as.factor(death_by_year_and_state$Year))),
                plotOutput("overdose_counts", width = "100%"),
                               p("Unfortunately, the distribution of treatment centers per capita does not match up with the number of opioid deaths per capita. 
                                 Please note that the data for treatment centers used to create the map below is more recent than the most recent opioid death data."),
                h1("Distribution of Opioid Treatment Centers in 2020 (Per Capita)"),
                plotOutput("treatment_centers_per_capita")
                                )
               )),
tabPanel("Massachusetts Opioid Data",
         # find a way to increase width
         column(7, 
                imageOutput("madeathrates", height = "100%"),
                h2("Opioid Deaths in Massachusetts by County over Time"),
                
                # create drop down for user selecting time period
                
                sidebarPanel(
                  selectInput("select", 
                              label = h3("Select a time period"),
                              choices = list("2001-2005", "2006-2010", "2011-2015")),
                mainPanel(
                  plotOutput("plot_1"),
                  plotOutput("plot_2"),
                  plotOutput("plot_3")),
                h2("Distribution of Opioid Treatment Centers Across Massachusetts"),
                fluidPage(
                  leafletOutput("masstreat")))
         
         )))

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
    }, width = 600, 
    height = 450)
    
    output$plot_1 <- renderPlot({
    ggplot(data = full_data,
           mapping = aes(x = long, y = lat,
                         fill = percap_2001.5,
                         group = group)) + 
      geom_polygon(color = "gray90", size = 0.05) + 
      theme_map() +
      scale_fill_gradient(low = "white", high = "#CB454A",
                          breaks = c(0,20,40,60,80,100)) +
      guides(fill = guide_legend(nrow = 1)) + 
      theme(legend.position = "bottom")
    }, width = 600, 
    height = 450)
    
    output$plot_2 <- renderPlot({
      ggplot(data = full_data,
             mapping = aes(x = long, y = lat,
                           fill = percap_2006.10,
                           group = group)) + 
        geom_polygon(color = "gray90", size = 0.05) + 
        theme_map() +
        scale_fill_gradient(low = "white", high = "#CB454A",
                            breaks = c(0,20,40,60,80,100)) +
        guides(fill = guide_legend(nrow = 1)) + 
        theme(legend.position = "bottom")
    }, width = 600, 
    height = 450)
    
    output$plot_3 <- renderPlot({
      ggplot(data = full_data,
             mapping = aes(x = long, y = lat,
                           fill = percap_2011.15,
                           group = group)) + 
        geom_polygon(color = "gray90", size = 0.05) + 
        theme_map() +
        scale_fill_gradient(low = "white", high = "#CB454A",
                            breaks = c(0,20,40,60,80,100)) +
        guides(fill = guide_legend(nrow = 1)) + 
        theme(legend.position = "bottom")
    }, width = 600, 
    height = 450)
    
    output$madeathrates <- renderImage({
      # Return a list containing the filename
      list(src = "plot_mavsus_death.gif",
           contentType = 'image/gif',
           width = 600,
           height = 450,
           alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    output$commondiseases <- renderImage({
      # Return a list containing the filename
      list(src = "plot_deathsbycommondiseases.gif",
           contentType = 'image/gif',
           width = 600,
           height = 450,
           alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    output$propcommondiseases <- renderImage({
      # Return a list containing the filename
      list(src = "prop_deathsbycommondiseases.gif",
           contentType = 'image/gif',
           width = 600,
           height = 450,
           alt = "This is alternate text"
      )}, deleteFile = FALSE)
    
    output$treatment_centers_per_capita <- renderPlot({
      ggplot(data = treatment_locations_map_per_cap,
             mapping = aes(x = long, y = lat.y,
                           fill = count_per_pop, group = group)) + 
        geom_polygon(color = "gray90", size = 0.05) + 
        theme_map() +
        scale_fill_gradient(low = "white", high = "#CB454A",
                            breaks = c(0,20,40,60,80,120)) +
        guides(fill = guide_legend(nrow = 1)) + 
        theme(legend.position = "bottom") +
        labs(
             color = "Number of Treatment Centers in State Per Capita")
      }, 
      width = 600, 
      height = 450)
    
    output$masstreat <- renderLeaflet ({
      leaflet(options = leafletOptions(dragging = TRUE,
                                     minZoom = 8, 
                                     maxZoom = 9)) %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(data = treatment_locations_map_mas,
                       radius = 3,
                       label = ~`Program Name`)})
    
}
shinyApp(ui, server)
