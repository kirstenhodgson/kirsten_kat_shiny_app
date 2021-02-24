library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(tigris)

#Read in the fire data: 
fire_data <- read.csv(here("data", "fire incidents 2013-2020.csv")) %>% 
  clean_names() %>% 
# Remove Oregon & Nevada from Data Frame: 
  filter(counties != "State of Nevada",
         counties != "State of Oregon",
         counties != "Mexico")

# Read in CA counties map data:
ca_counties <- read_sf(here("data","ca_counties"), layer = "CA_Counties_TIGER2016") %>% 
  clean_names()

#Read in CA fire perimeters data: 
fire_perimeters <- read_sf(here("data", "fire_perimeters"), layer = "California_Fire_Perimeters__all_") %>% 
  clean_names() %>% 
  filter(year %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019)) %>% 
  mutate(cause_label = case_when(cause == 1 ~ "Lightning",
                                 cause == 2 ~ "Equipment Use",
                                 cause == 3 ~ "Smoking",
                                 cause == 4 ~ "Campfire",
                                 cause == 5 ~ "Debris",
                                 cause == 6 ~ "Railroad",
                                 cause == 7 ~ "Arson",
                                 cause == 8 ~ "Playing with Fire",
                                 cause == 9 ~ "Miscellaneous",
                                 cause == 10 ~ "Vehicle",
                                 cause == 11 ~ "Powerline",
                                 cause == 12 ~ "Firefighter Training",
                                 cause == 13 ~ "Non-firefighter Training",
                                 cause == 14 ~ "Unknown/Unidentified",
                                 cause == 15 ~ "Structure",
                                 cause == 16 ~ "Aircraft",
                                 cause == 17 ~ "Volcanic",
                                 cause == 18 ~ "Escaped Prescribed Burn",
                                 cause == 19 ~ "Illegal Alien Campfire",
                                 TRUE ~ "Not Recorded"))

#system("unzip data/S_USA.EcomapSections.zip")
 
#Creating the user interface
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("California Fire",
                           tabPanel("Welcome!",
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Background",
                                                img(src = "fire.png", width = 700, height = 500),
                                                p("This app will explore incidents of fire in California counties between 2013-2020. Later on, this app will have vegetation data & explore mapping of fire incidents and vegetation. Hooray!"),
                                                img(src = "county_map.gif", width = 700),
                                                p("Map of California counties. Image source: geology.com")),
                                       tabPanel("Data Source", 
                                                h2("Data sources:"),
                                                h3("California Wildfire Incidents between 2013-2020"),
                                                a(href ="https://www.kaggle.com/ananthu017/california-wildfire-incidents-20132020/metadata", "Link"),
                                                p("This dataset contains information from CalFire. It contains a list of California wildfires between 2013 and 2020 and includes information on the fire location by county name and latitude and longitude coordinates which we will use in our exploration of fire locations."),
                                                h3("California Wildfire Perimeters 1950 - 2019"),
                                                a(href = "https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all?geometry=-138.776%2C31.410%2C-99.445%2C43.564", "Link"),
                                                p("This dataset contains information from the California Government Database. It contains spatial data of the perimeters of all California wildfires between 1950 and 2019"),
                                                h3("California Counties Spatial Data"),
                                                p("This dataset contains spatial data for all California Counties")
                                                ),
                                       tabPanel("How to Use",
                                                p("To use this app:"),
                                                p("1. Select a tab along the top"),
                                                p("2. Select a widget input as directed by widget"),
                                                p("3. View the output!"))
                                     )
                                   ) ),
                           tabPanel("Widget 1",
                                    sidebarLayout(
                                        sidebarPanel("Fire Cause",
                                                     checkboxGroupInput(inputId = "pick_cause",
                                                                        label = "Choose fire cause(s) of interest:",
                                                                        choices = unique(fire_perimeters$cause_label))
                                        ),
                                        mainPanel("OUTPUT! 1",
                                                  plotOutput("sw_plot"))
                                    )
                           ),
                           tabPanel("Widget 2",
                                    sidebarLayout(
                                        sidebarPanel("Fire Years",
                                                     selectInput(inputId = "pick_year",
                                                                 label = "Choose a Year:",
                                                                 choices = unique(fire_perimeters$year))
                                                     ),
                                        mainPanel("Output 2",
                                                  plotOutput("sw_plot_2"))
                                    )
                                    ),
                           tabPanel("Widget 3",
                                    sidebarLayout(
                                        sidebarPanel("Fire Years",
                                                     sliderInput(inputId = "choose_years",
                                                                 label = "Choose a range of fire years:",
                                                                 min = 2013,
                                                                 max = 2020,
                                                                 value = c(2013,2020))
                                                     ),
                                        mainPanel("Output 3",
                                                  plotOutput("sw_plot_3"))
                                    )
                                    ),
                           tabPanel("Widget 4",
                                    sidebarLayout(
                                        sidebarPanel("California counties",
                                                     radioButtons(inputId = "choose_county_2",
                                                                  label = "Choose a California county:",
                                                                  choices = unique(fire_data$counties)
                                                                 )
                                                     ),
                                        mainPanel("Output 4",
                                                  plotOutput("sw_plot_4"))
                                    ))
                           
                ))


#Building the server:
server <- function(input, output) {
  
 #Widget 1:
  fire_cause <- reactive({
    fire_perimeters %>% 
      filter(cause_label %in% input$pick_cause) %>% 
      group_by(cause_label, year) %>% 
      summarize(cause_count = n()) 
  })
  
  output$sw_plot <- renderPlot({
    ggplot(data = fire_cause(), aes(x = year, y = cause_count, color = cause_label)) +
      geom_line() +
      theme_minimal()
  })
  
 #Widget 2: 
  year_perimeters <- reactive({
    fire_perimeters %>% 
      filter(year == input$pick_year)
  })
  
  output$sw_plot_2 <- renderPlot({
    ggplot() +
      geom_sf(data = ca_counties, size = 0.2, color = "black", fill = "lightgray") +
      geom_sf(data = year_perimeters(), size = 0.5, color = "red") +
      theme_void() +
      labs(title = "Map of Fire Perimeters across California in the Chosen Year")
  })
  
 #Widget 3:
  fire_counts <- reactive({
    fire_counts <- fire_data %>% 
      filter(archive_year %in% input$choose_years) %>% 
      group_by(counties) %>% 
      summarize(count = n())
  })
  
  counties_fires_merged <- reactive({
    geo_join(ca_counties, fire_counts(), by_sp = 'name', by_df = 'counties')
  })
  
  output$sw_plot_3 <- renderPlot({
    ggplot() +
      geom_sf(data = counties_fires_merged(), aes(fill = count), color = "black", size = 0.1)
  })
  
 #Widget 4:    
 acres_burned <- reactive({
   fire_data %>% 
     filter(counties == input$choose_county_2) %>% 
     group_by(counties, archive_year) %>% 
     mutate(total_acres_burned = sum(acres_burned)) 
    })
  
  output$sw_plot_4 <- renderPlot({
    ggplot(data = acres_burned(), aes(x = archive_year, y = total_acres_burned)) +
      geom_line(size = 1, color = "red") +
      labs(title = "Change in Total Acres Burned across Entire Selected \n California County from 2013 - 2019",
           x = "Year",
           y = "Total Acres Burned in the County") +
      theme_gray() +
      theme(plot.title = element_text(hjust = 0.5, size = 15)) +
      scale_x_discrete(limits = c(2013, 2014, 2015, 2016, 2017,2018 ,2019))
  })
}
shinyApp(ui = ui, server = server)