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

fire_cause_counts <- fire_perimeters %>% 
    group_by(cause_label, year) %>% 
    summarize(cause_count = n()) %>% 
    ungroup()

#system("unzip data/S_USA.EcomapSections.zip")
 
#Creating the user interface
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("California Fire",
                           tabPanel("Welcome!",
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Background",
                                                p(""),
                                                img(src = "fire.png", width = 700, height = 500),
                                                p(" "),
                                                p("This app will explore incidents of fire in all California counties between 2013-2020 using data from Cal Fire and the California Government. 
                                                  This app will explore the causes of fire across study years, vizualize fire perimeters across all of california for each given year,
                                                  visualize the number of fires per county given a range of years, and explore the total acres burned in each county across the study period to gain a better 
                                                  understanding of how fire intensity and quantity has changed in the last decade."),
                                                p(" "),
                                                p(" "),
                                                img(src = "county_map.gif", width = 700),
                                                p(" "),
                                                p("Map of California counties. Image source: geology.com")),
                                       tabPanel("Data Source", 
                                                h2("Data sources:"),
                                                h3("California Wildfire Incidents between 2013-2020"),
                                                a(href ="https://www.kaggle.com/ananthu017/california-wildfire-incidents-20132020/metadata", "Link"),
                                                p("This dataset contains information from CalFire. It contains a list of California wildfires between 2013 and 2020 and includes information on the fire location by county name and latitude and longitude coordinates, and includes information regarding the acres burned in each fire event.
                                                  We will use this dataset to explore the total acreage burned per county as well as the number of fires per county across the study window."),
                                                h3("California Wildfire Perimeters 1950 - 2019"),
                                                a(href = "https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all?geometry=-138.776%2C31.410%2C-99.445%2C43.564", "Link"),
                                                p("This dataset contains information from the California Government Database. It contains spatial data of the perimeters of all California wildfires between 1950 and 2019 as well as information regarding the causes of fire.
                                                  We will use this dataset to map the perimeter of California fires and explore how the causes of fires change across the study period."),
                                                h3("California Counties Spatial Data"),
                                                a(href = "https://data.ca.gov/dataset/ca-geographic-boundaries/resource/b0007416-a325-4777-9295-368ea6b710e6", "Link"),
                                                p("This dataset contains information from the California Government Database. It contains spatial data for the perimeters of all California Counties.
                                                  We will use this dataset as a background map to plot fire information spatially across California counties.")
                                                ),
                                       tabPanel("How to Use",
                                                p(""),
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
                                                                        choices = unique(fire_perimeters$cause_label),
                                                                        selected = "Unknown/Unidentified")
                                        ),
                                        mainPanel(plotOutput("sw_plot"),
                                                  "This is where I am going to write and format a description of the graph of fire cause.
                                                  Information about fire cause classification was taken from the web-based data source.")
                                    )
                           ),
                           tabPanel("Widget 2",
                                    sidebarLayout(
                                        sidebarPanel("Fire Years",
                                                     selectInput(inputId = "pick_year",
                                                                 label = "Choose a Year:",
                                                                 choices = unique(fire_perimeters$year))
                                                     ),
                                        mainPanel("Fire perimeters",
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
                                                                 value = c(2013,2020),
                                                                 sep = "")
                                                     ),
                                        mainPanel(plotOutput("sw_plot_3"),
                                                  "This is where I am going to write and format an awesome description of my map of fire counts.")
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
                                        mainPanel("Change in total acres burned by county",
                                                  plotOutput("sw_plot_4"))
                                    ))
                           
                ))


#Building the server:
server <- function(input, output) {
  
 #Widget 1:
  fire_cause <- reactive({
    fire_cause_counts %>% 
      filter(cause_label %in% input$pick_cause)
  })
  
  output$sw_plot <- renderPlot({
    ggplot(data = fire_cause(), aes(x = year, 
                                    y = cause_count, 
                                    group = cause_label, 
                                    color = cause_label)) +
      geom_line() +
      theme_minimal() +
      labs(x = "\nYear", 
           y = "Number of fires\n", 
           color = "Fire cause",
           title = "Number of California fires by cause, 2013-2019")

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
      filter(between(archive_year, input$choose_years[1], input$choose_years[2])) %>% 
      group_by(counties) %>% 
      summarize(count = n())
  })
  
  counties_fires_merged <- reactive({
    geo_join(ca_counties, fire_counts(), by_sp = 'name', by_df = 'counties')
  })
  
  output$sw_plot_3 <- renderPlot({
    ggplot() +
      geom_sf(data = counties_fires_merged(), 
              aes(fill = count), 
              color = "black", 
              size = 0.1) +
      theme_void() +
      labs(fill = "Number of fires\nin selected years",
           title = "Number of fires by California county in selected years")
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