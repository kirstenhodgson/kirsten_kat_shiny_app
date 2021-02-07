library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(rgdal)
library(raster)

#Read in the data: 
fire_data <- read.csv(here("data", "fire incidents 2013-2020.csv")) %>% 
    clean_names()

system("unzip data/S_USA.EcomapSections.zip")

veg_data <- shapefile("S_USA.EcomapSections")

#Creating the user interface
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("California Fire",
                           tabPanel("Welcome!",
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Background",
                                                p("This is our information")),
                                       tabPanel("Data Sources", 
                                                h2("Data sources:"),
                                                h3("California Wildfire Incidents between 2013-2020"),
                                                a(href ="https://www.kaggle.com/ananthu017/california-wildfire-incidents-20132020/metadata", "Link"),
                                                p("This dataset contains information from CalFire. It contains a list of California wildfires between 2013 and 2020 and includes information on the fire location by county name and latitude and longitude coordinates which we will use in our exploration of fire locations.")
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
                                        sidebarPanel("Vegetation Type",
                                                     checkboxGroupInput(inputId = "pick_veg",
                                                                        label = "Choose vegetation type:",
                                                                        choices = unique(starwars$species))
                                        ),
                                        mainPanel("OUTPUT! 1",
                                                  plotOutput("sw_plot"))
                                    )
                           ),
                           tabPanel("Widget 2",
                                    sidebarLayout(
                                        sidebarPanel("California counties",
                                                     selectInput(inputId = "pick_county",
                                                                 label = "Choose a California county:",
                                                                 choices = unique(fire_data$counties))
                                                     ),
                                        mainPanel("Output 2",
                                                  plotOutput("sw_plot_2"))
                                    )
                                    ),
                           tabPanel("Widget 3",
                                    sidebarLayout(
                                        sidebarPanel("Fire years",
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
    
    
}
shinyApp(ui = ui, server = server)