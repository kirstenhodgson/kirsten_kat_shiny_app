library(tidyverse)
library(shiny)
library(shinythemes)

#Creating the user interface
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("California Fire",
                           tabPanel("Welcome!",
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Background",
                                                p("This is our information")),
                                       tabPanel("Data Sources", 
                                                p("Data sources:"),
                                                p("FIRST SOURCE")
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
                                                                 choices = unique(starwars$species))
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
                                                                 min = 1950,
                                                                 max = 2020,
                                                                 value = c(1950,2000))
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
                                                                  choices = unique(starwars$species)
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