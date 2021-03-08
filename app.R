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
library(viridis)
library(jpeg)
library(ggpubr)
library(rmapshaper)


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

ca_simple = rmapshaper::ms_simplify(ca_counties, keep = 0.003, keep_shapes = TRUE)

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
                                 TRUE ~ "Not Recorded")) %>% 
  mutate(year = as.numeric(year)) #I was hoping that this would make the years appear in order in Widget 2 but no luck

fire_cause_counts <- fire_perimeters %>% 
    group_by(cause_label, year) %>% 
    summarize(cause_count = n()) %>% 
    ungroup()

big_fires <- fire_perimeters %>% 
  filter(gis_acres > 100) %>% 
  dplyr::select(year)

big_fires_simple = rmapshaper::ms_simplify(big_fires, keep = 0.003, keep_shapes = TRUE)

fire_acres <- fire_data %>% 
  dplyr::select(acres_burned, archive_year, counties, name) %>%
  group_by(counties) %>% 
  filter(acres_burned > 0) %>% 
  top_n(5, acres_burned) %>% 
  unite(name_year, c(name, archive_year), sep = " ")

fire <- readJPEG("fire.jpg")

#Creating the user interface
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("Fire in California",
                           tabPanel("Welcome!",
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Background",
                                                p(""),
                                                img(src = "fire.png", width = 700, height = 500, style = "display:block; margin-left: auto; margin-right: auto;"),
                                                p(" "),
                                                p("Wildfire devastation and impact across the state of California has been an increasingly worsening issue in recent decades in the face of climate change.
                                                  The 10 largest fires in the state's history have all occured since 2000. Contributors to the growing threat of wildfire in the state include increasing temperatures, 
                                                  intense drought conditions, strong Santa Ana winds, and adverse effects caused by humans. Examples of relevant human activities are
                                                  downed powerlines and the now infamous gender reveal party fireworks, which sparked the destructive El Dorado Fire that burned more than 20,000 acres in 2020."),
                                                p("This app will explore incidents of fire in all California counties between 2013-2020 using data from Cal Fire and the government of California. 
                                                  It will explore the causes of fire across study years, vizualize fire perimeters across all of california for each given year,
                                                  visualize the number of fires per county given a range of years, and explore the five largest fires by acreage in each county across the study period to gain a better 
                                                  understanding of how fire intensity and quantity has changed in the last decade."),
                                                p(" "),
                                                p(" "),
                                                img(src = "county_map.gif", width = "80%", height = "80%", style = "display:block; margin-left: auto; margin-right: auto;"),
                                                p(" "),
                                                p("Map of California counties. Image source: geology.com")),
                                       tabPanel("Data Sources", 
                                                h2("Data sources:"),
                                                h3("California Wildfire Incidents between 2013-2020"),
                                                a(href ="https://www.kaggle.com/ananthu017/california-wildfire-incidents-20132020/metadata", "Link"),
                                                p("This dataset contains information from CalFire. It contains a list of California wildfires between 2013 and 2020 and includes information on the fire location by county name and latitude and longitude coordinates, and includes information regarding the acres burned in each fire event.
                                                  We will use this dataset to explore the total acreage burned by the largest fires per county as well as the number of fires per county across the study window."),
                                                h3("California Wildfire Perimeters 1950 - 2019"),
                                                a(href = "https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all?geometry=-138.776%2C31.410%2C-99.445%2C43.564", "Link"),
                                                p("This dataset contains information from the California Government Database. It contains spatial data of the perimeters of all California wildfires between 1950 and 2019 as well as information regarding the causes of fire.
                                                  We will use this dataset to map the perimeter of California fires and explore how the causes of fires change across the study period."),
                                                h3("California Counties Spatial Data"),
                                                a(href = "https://data.ca.gov/dataset/ca-geographic-boundaries/resource/b0007416-a325-4777-9295-368ea6b710e6", "Link"),
                                                p("This dataset contains information from the California Government Database. It contains spatial data for the perimeters of all California Counties.
                                                  We will use this dataset as a background map to plot fire information spatially across California counties."),
                                                img(src = "paradise.png", width = 700, height = 500, style = "display:block; margin-left: auto; margin-right: auto;"),
                                                p("Paradise, California Destruction after the Camp Fire 2019. Image source: New York Times"),
                                                a(href = "https://www.nytimes.com/2019/02/28/business/energy-environment/pge-camp-fire.html", "Photo Source")
                                                ),
                                       tabPanel("How to Use",
                                                p(" "),
                                                p(" "),
                                                p("This app contains widgets that allow the user to explore the number of fires in California by cause, view a map of fire perimeters in each year, view a map of the number of fires per county, and explore the number of acres burned per county."),
                                                h2("To use this app:"),
                                                p(" "),
                                                p("1. Select a tab along the top of the app to view a widget."),
                                                p("2. Select a widget input on the left hand side as directed."),
                                                p("3. View the output created by your selection!"),
                                                img(src = "thomas.png", width = 700, height = 500, style = "display:block; margin-left: auto; margin-right: auto;"),
                                                p("View of Santa Barbara During the Thomas Fire in 2017. Image Source: Santa Barbara Independent."),
                                                a(href = "https://www.independent.com/2017/12/13/closing-schools-and-moving-finals-due-thomas-fire/", "Photo Source"))
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
                                        mainPanel(h3("Changes in fire cause through time"),
                                                  p(" "),
                                                  p("This widget charts the number of fires in California per year, grouped by the selected fire cause. You can
                                                  select as many possible causes as you would like to explore trends over time"),
                                                  plotOutput("sw_plot"),
                                                  p(" "),
                                                  p("Information about fire cause classification was taken from the website of the data source:"),
                                                  a(href = "https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-fire-perimeters-all?geometry=-138.776%2C31.410%2C-99.445%2C43.564", "CALFIRE Perimeter Data"),
                                                  )
                                    )
                           ),
                           tabPanel("Widget 2",
                                    sidebarLayout(
                                        sidebarPanel("Fire Years",
                                                     selectInput(inputId = "pick_year",
                                                                 label = "Choose a Year:",
                                                                 choices = unique(big_fires_simple$year),
                                                                 selected = "2016")
                                                     ),
                                        mainPanel(h3("Map of Area Burned by Wildfire per Year"),
                                                  p(" "),
                                                  p("This widget creates a map of california countines and overlays all fire perimeters larger than 100 acres across the state for the selected year to
                                                    help users visualize the amount of land burned by fire."),
                                                    plotOutput("sw_plot_2"),
                                                  "Information used to build this map was available from the California government database.")
                                    )
                                    ),
                           tabPanel("Widget 3",
                                    sidebarLayout(
                                        sidebarPanel("Fire Years",
                                                     sliderInput(inputId = "choose_years",
                                                                 label = "Choose a range of fire years:",
                                                                 min = 2013,
                                                                 max = 2019,
                                                                 value = c(2013,2019),
                                                                 sep = "")
                                                     ),
                                        mainPanel(h3("Fire frequency by county, 2013-2019"),
                                                  p(" "),
                                                  p("This widget creates a chloropleth map of California that shows the number of fires by county
                                                    in the selected time range. Slide the buttons to the desired year range to see which counties had the
                                                    most fires in that time."),
                                                  p(" "),
                                                  plotOutput("sw_plot_3"),
                                                  p(" "),
                                                  p("Lighter colors indicate more fires in the selected year range, while darker colors indicate fewer fires."),
                                                  p("Imperial County (shown in gray) had no recorded wildfire incidents within our dataset for the years 2013-2019."),
                                                  p("The data for this chloropleth map was available from the California Wildfire Perimeters 1950-2019 and California
                                                    Counties Spatial datasets, listed under the Data Sources tab on the welcome page."))
                                    )
                                    ),
                           tabPanel("Widget 4",
                                    sidebarLayout(
                                        sidebarPanel("California Counties",
                                                     radioButtons(inputId = "choose_county_2",
                                                                  label = "Choose a California county:",
                                                                  choices = unique(fire_acres$counties),
                                                                  selected = "Santa Barbara"
                                                                 )
                                                     ),
                                        mainPanel(h3("Five Largest Fires by Acres Burned across the Study Period"),
                                                  p(" "),
                                                  p("This widget charts the five largest fires across the study period, from 2013 to 2020, in the selected county by acres burned."),
                                                  plotOutput("sw_plot_4"),
                                                  "Information used to build this map was available on the California government database.")
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
           title = "Number of California fires by cause, 2013-2019") +
      theme(title = element_text(color = "firebrick4"),
            axis.text = element_text(color = "firebrick4")) +
      scale_color_manual(values = c("firebrick4",
                                    "black",
                                    "goldenrod3",
                                    "mediumvioletred",
                                    "tan4",
                                    "dodgerblue3",
                                    "lightseagreen",
                                    "green4",
                                    "darkblue",
                                    "coral3",
                                    "darkslategray",
                                    "lightpink1",
                                    "palegreen3",
                                    "orchid",
                                    "cadetblue1",
                                    "azure4"))

  })
  
 #Widget 2: 
ranges <- reactiveValues(x = NULL, y = NULL)
  
  year_perimeters <- reactive({
    big_fires_simple %>% 
      filter(year == input$pick_year)
  }) %>% 
    bindCache(input$pick_year)
  
  output$sw_plot_2 <- renderPlot({
    ggplot() +
      geom_sf(data = ca_simple, size = 0.3, color = "bisque4", fill = "burlywood4", alpha = 0.7) +
      geom_sf(data = year_perimeters(), size = 0.75, color = "red", fill = "orangered2") +
      theme_void() +
      theme(plot.background = element_rect(fill = "gray10", color = "white")) +
      labs(title = "Map of Fire Perimeters \n Across California") +
      theme(plot.title = element_text(size = 15, hjust = 1, color = "orangered2")) +
      coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  }) %>% 
    bindCache(input$pick_year)
 
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
           title = "Number of fires by California county in selected years") +
      scale_fill_viridis(option = "inferno") #Something weird might be happening in Riverside - check it out
  })
  
 #Widget 4:    
 acres_burned <- reactive({
   fire_acres %>% 
     filter(counties == input$choose_county_2) 
    })
  
  output$sw_plot_4 <- renderPlot({
    ggplot(data = acres_burned(), aes(x = name_year, y = acres_burned)) +
      background_image(fire) +
      geom_col(fill = "white", alpha = 0.8) +
      labs(y = "Acres Burned",
           x = "") +
      geom_text(aes(label = scales::comma(acres_burned)), hjust = -0.05, size = 2.5, color = 'white')  +
      theme_minimal() +
      coord_flip() +
      theme(axis.text.y = element_text(face = 'bold', colour = "orangered2")) +
      theme(axis.title.x = element_text(size = 10, face = 'bold', colour = "orangered2")) +
      theme(axis.text.x = element_text(size = 8, colour = "gray88")) +
      theme(plot.background = element_rect(fill = "gray10", color = "white"))
  })
}
shinyApp(ui = ui, server = server)