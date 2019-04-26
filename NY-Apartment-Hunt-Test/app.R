# New York Apartment Hunt

# Loading necessary packages
library(shiny)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)

# Loading two-bedroom apartments in New York dataset from StreetEasy
median_asking_rent <- read_xlsx("medianAskingRent_TwoBd.xlsx") 

rent <- median_asking_rent %>% 
  filter(`Borough` == "Manhattan") %>%
  filter(`areaType` == "neighborhood") %>%
  gather(key = "year_month", value = "asking_price", -c(areaName, areaType, Borough)) %>%
  select(areaName, year_month, asking_price) %>%
  separate(year_month, c("Year", "Month"))

# Define UI for application 
ui <- 
  navbarPage("New York Apartment Hunt",
             
  theme = shinytheme("yeti"),

  tabPanel("Home",
           fluidPage(
             fluidRow(
               h1("Welcome!"), 
               h3("What does this app let you do?"),
               p("Housing prices in New York have skyrocketed over the past few years. Using data gathered from StreetEasy, this app lets you explore the prices of 2-bedroom rental units in Manhattan's 33 neighborhoods between January 2010 and February 2019."),
               br(),
               h3("Why Manhattan? "), 
               p("In October 2018, I signed my offer with Deloitte Consulting in New York. As my roommate next year and I are actively looking for a new place to call home, I thought I could help our apartment hunt process by examining trends in the prices of 2-bedroom apartments to find the best neighborhood for us to live. Although we are only considering living in 10 of Manhattan's 33 neighborhoods due to proximity to our offices, this project includes data for all of Manhattan's neighborhoods. If you're also looking to move to New York soon, I hope this app helps you find the best neighborhood to live in, too!"),
               br(),
               h3("How was this app made?"),
               p("Check out the code behind this app here: ")
             )
           )
           ),
  
  tabPanel("Price",
           fluidRow(
             fluidPage(
               h1("Price Trends"), 
               br(),
               p("This is an analysis of the median asking rent price for all neighborhoods in Manhattan between January 2000 and February 2019. This allows you to compare and contrast price trends for each of Manhattan's neighborhoods over the past 9 years."), 
               br(),
  
  # Sidebar layout with input and output definitions
  
  sidebarLayout(
    
    # Sidebar panel for interactive input
    
    sidebarPanel(
      
      pickerInput("areaName", "Neighborhood", choices = c("Battery Park City",
                                                          "Central Harlem",
                                                          "Central Park South",
                                                          "Chelsea",
                                                          "Chinatown",
                                                          "Civic Center",
                                                          "East Harlem",
                                                          "East Village",
                                                          "Financial District",
                                                          "Flatiron",
                                                          "Gramercy Park",
                                                          "Greenwich Village",
                                                          "Hamilton Heights",
                                                          "Inwood",
                                                          "Little Italy",
                                                          "Lower East Side",
                                                          "Manhattanville",
                                                          "Marble Hill",
                                                          "Midtown",
                                                          "Midtown East",
                                                          "Midown South",
                                                          "Midtown West",
                                                          "Morningside Heights",
                                                          "Nolita",
                                                          "Roosevelt Island",
                                                          "Soho",
                                                          "Stuyvesant Town/PCV",
                                                          "Tribeca",
                                                          "Upper East Side",
                                                          "Upper West Side",
                                                          "Washington Heights",
                                                          "West Harlem",
                                                          "West Village"),
                  selected = c("Battery Park City",
                               "Central Harlem",
                               "Central Park South",
                               "Chelsea",
                               "Chinatown",
                               "Civic Center",
                               "East Harlem",
                               "East Village",
                               "Financial District",
                               "Flatiron",
                               "Gramercy Park",
                               "Greenwich Village",
                               "Hamilton Heights",
                               "Inwood",
                               "Little Italy",
                               "Lower East Side",
                               "Manhattanville",
                               "Marble Hill",
                               "Midtown",
                               "Midtown East",
                               "Midown South",
                               "Midtown West",
                               "Morningside Heights",
                               "Nolita",
                               "Roosevelt Island",
                               "Soho",
                               "Stuyvesant Town/PCV",
                               "Tribeca",
                               "Upper East Side",
                               "Upper West Side",
                               "Washington Heights",
                               "West Harlem",
                               "West Village"), 
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      pickerInput("Year", "Year:", choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                  selected = c("2010" = "2010",
                               "2011" = "2011",
                               "2012" = "2012",
                               "2013" = "2013",
                               "2014" = "2014",
                               "2015" = "2015",
                               "2016" = "2016",
                               "2017" = "2017",
                               "2018" = "2018"), 
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
                
      pickerInput("Month", "Month:", choices = c("January" = "01",
                                                 "February" = "02",
                                                 "March" = "03",
                                                 "April" = "04",
                                                 "May" = "05",
                                                 "June" = "06",
                                                 "July" = "07",
                                                 "August" = "08",
                                                 "September" = "09",
                                                 "October" = "10",
                                                 "November" = "11",
                                                 "December" = "12"),
                  selected = c("January" = "01",
                               "February" = "02",
                               "March" = "03",
                               "April" = "04",
                               "May" = "05",
                               "June" = "06",
                               "July" = "07",
                               "August" = "08",
                               "September" = "09",
                               "October" = "10",
                               "November" = "11",
                               "December" = "12"), 
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      br(),
      helpText("Note: You must select at least one neighborhood, one year, and one month to see the graph.")),
 
      mainPanel(
        plotOutput("rentPlot")))
    )
    )
  )
  ) 

server <- function(input, output) {
  
  output$rentPlot <- renderPlot({
    ggplot(data = rent, aes(x = input$Month, y = `asking_price`, color = input$areaName)) +
      geom_line() +
      labs(x = "Month",
           y = "Price in USD",
           title = "Median Rental Price of 2-Bedroom Rental Units by Neighborhood in Manhattan",
           caption = "Source: StreetEasy") +
      theme(legend.position = "none")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
