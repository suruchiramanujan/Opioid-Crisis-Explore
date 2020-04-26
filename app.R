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
library(plotly)
library(DT)
library(broom)

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

ageopioidmodel <- read_csv("U.S. age vs. opioid deaths.csv", col_types = "dcc") %>%
  clean_names() %>%
  na.omit() %>%
  filter(age_range != "Total") %>%
  mutate(age_range = as.factor(age_range)) %>%
  mutate(number_of_deaths = as.numeric(number_of_deaths))

raceopioidmodel <- read_csv("U.S. race vs. opioid deaths.csv", col_types = "dcc") %>%
  clean_names() %>%
  na.omit() %>%
  mutate(race = as.factor(race)) %>%
  mutate(opioid_deaths = as.numeric(opioid_deaths))

ageandracemodel <- ageopioidmodel %>%
  na.omit() %>%
  full_join(raceopioidmodel, by = "year") %>%
  rename(deathsbyage = number_of_deaths) %>%
  rename(deathsbyrace = opioid_deaths)

deathsbycommondiseases <- read_csv("deathsbycommondiseases.csv", col_types = "ldddddd") %>%
  select(Year, `Deaths by Cancer`, `Deaths by Drug Overdose`, `Deaths by CVD`) %>%
  mutate(`Proportion of 1999 Deaths by Cancer` = `Deaths by Cancer`/549829) %>%
  mutate(`Proportion of 1999 Deaths by Drugs` = `Deaths by Drug Overdose`/19122) %>%
  mutate(`Proportion of 1999 Deaths by CVD` = `Deaths by CVD`/949900)

totaldeathsbycommondiseases <- deathsbycommondiseases %>%
  select(Year, `Deaths by Cancer`, `Deaths by Drug Overdose`, `Deaths by CVD`) %>%
  pivot_longer(.,
               cols = starts_with("Deaths by"),
               names_prefix = "Deaths by",
               values_to = "Deaths") %>%
  rename(`Cause of Death` = name)

propdeathsbycommondiseases <- deathsbycommondiseases %>%
  select(Year, `Proportion of 1999 Deaths by Cancer`, 
             `Proportion of 1999 Deaths by Drugs`, 
             `Proportion of 1999 Deaths by CVD`) %>%
  pivot_longer(., cols = starts_with("Proportion of"), names_prefix = "Proportion of 1999 Deaths by", values_to = "Prop.Deaths") %>%
  rename(`Cause of Death` = name)

mavsus_death <- read_csv("MAAgeAdjustedOpioidRelatedDeathRateByYear.csv", col_types = "cdc")

mavsus_death <- mavsus_death %>%
  group_by(Geography) %>%
  filter (Year != "2015") %>%
  filter (Year != "1999") %>%
  rename(deathperhundredthousand =`Age-Adjusted Opioid-Related Death Rate per 100,000 People`)

ui <- navbarPage(theme = shinytheme("flatly"),
                 "Opioid Trends Across the United States",
                 tabPanel("About",
                          column(9,
                                 h1("Background"),
                                 p("Opioids have been prescribed for pain management for many decades, 
                                 although they are effective at treating acute, not chronic pain. 
                                   In treating chronic pain, opioids can be more harmful than useful. 
                                   Opioids are cheap, but have high rates of addiction and can lead to 
                                   dependence and ultimately death from overdose.Since the 1990s, the United States
                                   has experienced a crisis created by overprescription of opioids. Thousands die yearly
                                   from uncontrollable overuse."),
                                 
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
                                 
                                 p("Finally, I sought to create a model between factors of interest such as 
                                   age and ethnicity and increases in opioid deaths over the past twenty years."),
                                 h1("The Data"),
                                 p("Data concerning the entire United States came from the CDC's Wonder Archive. Massachusetts-specific data came from
                                   chapter55.digital.mass.gov."),
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
                                   "or on ",
                                   a("LinkedIn",
                                     href = "https://www.linkedin.com/in/suruchi-ramanujan-791007115/")))),
                          
                 tabPanel("United States Opioid Data",
                        h1("Opioid Deaths in America"),
                        h3("Comparing Deaths from Opioid Overdose to Deaths from the Leading Causes of Death in America"),
                        fixedRow(column(7, plotlyOutput("commondiseases", height = "100%"), inline = TRUE),
                        column(3, offset = 1,           
                        p("The graph to the left shows CDC data for the number of deaths per year for the two top leading causes of death 
                                     in the United States: heart disease or cardiovascular disease (CVD) and cancer. 
                                     Simultaneously, I have graphed the number of deaths per year caused by opioid overdoses in the United States. 
                                     While this graph may make it look like opioids do not have as great a toll on the United States
                                     as other diseases like cancer and cardiovascular disease, just because fewer people are affected
                                     does not mean the issue is less important."))),
                        br(),
                        h3("Comparing Deaths from Opioid Overdose to Deaths from the Leading Causes of Death in America (Normalized to 1990)"),
                        fixedRow(column(7, plotlyOutput("propcommondiseases", height = "100%", width = "100%"), inline = TRUE),        
                        column(3, offset = 1,  
                        p("In fact, if we normalize the number of deaths due to
                                   opioid use to those seen in 1990 as seen to the left, we see the greatest proportion change in deaths by opioid overdose
                                   compared to cancer and cardiovascular disease as shown below. Thus, even though we are slowly making the changes
                                   needed to curtail cancer and cardiovascular disease deaths, we are still seeing sharp increases in death by
                                   opioid overdose."))),
                       
                                   fluidPage(
                  titlePanel("Drug Overdoses Per Capita by State Over Time"),
                  sliderTextInput("Year", "Year", 
                            from_min = 1998,
                            from_max =2018,
                            choices = levels(as.factor(death_by_year_and_state$Year))),
                  fixedRow(column(7, plotlyOutput("overdose_counts", width = "100%", inline = TRUE)),
                column(3, offset = 1,
                p("We cannot assume that the distribution of opioid overdose deaths per capita differs is uniform. There is certainly variation based on state.
                                   The graph to the left shows how opioid deaths per capita changed between 1998 and 2018. Some states such as West Virginia
                  have undergone immense changes in opioid deaths per capita over the past 20 years, beginning with some of the lowest death rates in 1998 and rising to 
                  have the highest state death rate per capita in 2018.")))),
                br(),
                
                h3("Distribution of Opioid Treatment Centers in 2020 (Per Capita)"),
                fixedRow(column(7,plotlyOutput("treatment_centers_per_capita")),
                column(3, offset = 1,
                       p("Unfortunately, the distribution of treatment centers per capita does not match up with the number of opioid deaths per capita. Even if we just
                       compare 2018 data, we will see that states like New Hampshire have few opioid treatment centers despite having some of the highest rates of opioid deaths
                       per capita. Furthermore, treatment centers seem to be concentrated on the East of the country.Please note that the data for treatment centers used to create the map 
                         to the left is more recent than the most recent opioid death data."))
               )),
tabPanel("Massachusetts Opioid Data",
         # find a way to increase width
             fixedRow(column(7,  plotlyOutput("madeathrates", height = "100%")),
                      column(3, offset = 1, p("The graph to the left, created using data from chapter55.digital.mass.gov shows that on average, the rate of opioid deaths per year 
                            in Massachusetts is higher than that of the United States. Despite efforts at expanding healthcare (for example, through Romneycare),
                               opioid deaths stay consistently higher than the national average"))),
                h2("Opioid Deaths in Massachusetts by County over Time"),
         br(),
                # create drop down for user selecting time period
                
              fixedRow(column(7,  sidebarPanel(
                  selectInput("plot_type", 
                              label = h3("Select a time period"),
                              choices = c("2001-2005", "2006-2010", "2011-2015"))),
                mainPanel(
                  plotlyOutput("plot_1", inline = TRUE))),
                column(3, offset = 1, p("The graph to the left shows the distribution of opioid deaths per capita across the counties of Massachusetts for three different 
                                        time periods between 2000 and 2015. Having looked at the data for total opioid deaths (not per data), deaths seem to be concentrated 
                                        in Sussex county, although this makes sense given the high population of Sussex. Thus, in order to better demonstrate the distribution
                                        of deaths, I chose to map the total deaths per capita by county."))),
                h2("Distribution of Opioid Treatment Centers Across Massachusetts"),
                fixedRow(column(7, fluidPage(
                  leafletOutput("masstreat"))),
                column(3, offset = 1, p("Similar to what we saw with the graph of the entire United States, treatment centers are not distributed based on the locations of the greatest number 
                                        of deaths per capita. Here, we see a high concentration of treatment centers in the northeast corner of the state and in the west rather than in the southwest, 
                                        where Massachusetts experiences the highest rates of opioid deaths.")))),
tabPanel("Model",
         h1("Model"),
         sidebarPanel(
           helpText("Choose a factor to see how opioid deaths change over time based on age and race."),
           selectInput("typeoffactor", 
                       label = h3("Select a factor"),
                       choices = c("Age", "Race"))),
         mainPanel(
           plotOutput("ageandrace", inline = TRUE)),
           br(),
          p("The models above demonstrate how opioid deaths have changed over time based on age group and race. 
                  Based on age group data, it looks like the total number of opioid deaths amongst people in the age groups of
                  25-34, 35-44, and 45-54 are similar. However, the numbers are increasing at the fastest rate amongst people between the ages of 
                  25 and 34 as communicated by the highest coefficient.Thus, we must focus on addressing issues pertaining to this group.
                  Examining the race data, it looks as though individuals are white at worst affected by the opioid crisis, with 
                  both the highest number of deaths amongst this group, in addition to the greatest rate of increase in deaths amongst these individuals."),
         br(),
         br(),
         sidebarPanel(
           selectInput("Age Group", 
                       label = h3("Select an Age Range"),
                       choices = c("0-24", "25-34", "35-44", "45-54", "55+"))),
         mainPanel(
           DTOutput("coefage")),
         br(),
         br(),
         sidebarPanel(
           selectInput("Race", 
                       label = h3("Select a Category of Race"),
                       choices = c("White, Non-Hispanic", "Black, Non-Hispanic", "Hispanic"))),
         mainPanel(
           DTOutput("coefrace"))
))
server <- function(input, output, session) {
    output$overdose_counts <- renderPlotly({
      map_1 <- death_by_year_and_state %>%
      filter(Year == input$Year) %>%
        ggplot(aes(x = long, y = lat,
                             fill = `Crude Rate`, group = group)) + 
        geom_polygon(color = "gray90", size = 0.05) + 
        theme_map() +
        scale_fill_gradient(low = "white", high = "#CB454A") +
        guides(fill = guide_legend(nrow = 1)) + 
        theme(legend.position = "bottom")
    
      map_1 <- ggplotly(map_1)
      })
    
    output$plot_1 <- renderPlotly({
    plot_1 <- ggplot(data = full_data,
           mapping = aes(x = long, y = lat,
                         fill = case_when(
                           input$plot_type == "2001-2005" ~ percap_2001.5,
                           input$plot_type == "2006-2010" ~ percap_2006.10,
                           input$plot_type == "2011-2015" ~ percap_2011.15),
                         group = group)) + 
      geom_polygon(color = "gray90", size = 0.05) + 
      theme_map() +
      scale_fill_gradient(low = "white", high = "#CB454A",
                          breaks = c(0,20,40,60,80,100)) +
      guides(fill = guide_legend(nrow = 1)) + 
      theme(legend.position = "bottom")
   plot_1 <- ggplotly(plot_1) 
   })
    
    output$madeathrates <- renderPlotly({
     map_2 <- ggplot(mavsus_death, aes(x = Year, y = as.numeric(deathperhundredthousand), color = Geography)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_classic() +
        labs(x = "Year",
             y = "Deaths per 100,000",
             title = "Comparison of Death Rates between Massachusetts and the United States",
             subtitle = "Between the Years of 2000 and 2014") +
        geom_point()
     
     map_2 <- ggplotly(map_2)
    })
    
    output$commondiseases <- renderPlotly({
     plot_2 <- ggplot(totaldeathsbycommondiseases, aes(x = Year, y = Deaths, color = `Cause of Death`)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_classic() +
        labs(x = "Year",
             y = "Total Number of Deaths in the United States") +
        geom_point()
     
     plot_2 <- ggplotly(plot_2) 
      
      })
    
    output$propcommondiseases <- renderPlotly({
    plot_3 <- ggplot(propdeathsbycommondiseases, aes(x = Year, y = Prop.Deaths, color = `Cause of Death`)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_classic() +
        labs(x = "Year",
             y = "Proportion of 1999 Total Deaths in the United States") +
        geom_point()
    
    plot_3 <- ggplotly(plot_3)
    
      })
    
    output$treatment_centers_per_capita <- renderPlotly({
    map_3 <- ggplot(data = treatment_locations_map_per_cap,
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
     map_3 <- ggplotly(map_3) 
     })
    
    output$masstreat <- renderLeaflet ({
      leaflet(options = leafletOptions(dragging = TRUE,
                                     minZoom = 8, 
                                     maxZoom = 9)) %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(data = treatment_locations_map_mas,
                       radius = 3,
                       label = ~`Program Name`)})
    
    
    output$ageandrace <-  renderPlot({
      if(input$typeoffactor == "Age"){
      ggplot(ageandracemodel, aes(year, deathsbyage, color = age_range)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Year", y = "Number of Opioid Deaths in America", color = "Age Range")}
      else{
      ggplot(ageandracemodel, aes(year, deathsbyrace, color = race)) +
        geom_point() +
        scale_color_viridis_d() +
        theme_classic() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Year", y = "Number of Opioid Deaths in America", color = "Race")}
    }, 
    width = 600, 
    height = 450)

    output$coefage <- renderDT({
      ageopioidmodel %>%
        filter(age_range == input$`Age Group`) %>%
        lm(number_of_deaths ~ year, data = .) %>% 
        tidy(conf.int = TRUE) %>% 
        select(term, estimate, conf.low, conf.high) %>%
        rename(Term = term) %>%
        rename(Coefficient = estimate) %>%
        rename(`Lower End` = conf.low) %>%
        rename(`Upper End` = conf.high)
    })
    output$coefrace <- renderDT({
      raceopioidmodel %>%
        filter(race == input$Race) %>%
        lm(opioid_deaths ~ year, data = .) %>% 
        tidy(conf.int = TRUE) %>% 
        select(term, estimate, conf.low, conf.high) %>%
        rename(Term = term) %>%
        rename(Coefficient = estimate) %>%
        rename(`Lower End` = conf.low) %>%
        rename(`Upper End` = conf.high)
    })}
shinyApp(ui, server)
