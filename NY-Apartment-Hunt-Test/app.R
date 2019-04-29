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

# Cleaning up the data, calling it rent for future use

rent <- median_asking_rent %>% 
  
  # I'm only interested in Manhattan, so I filter by borough for the relevant information
  
  filter(`Borough` == "Manhattan") %>%
  
  # Filtering by neighborhood only
  
  filter(`areaType` == "neighborhood") %>%
  
  # Reorganizes the data by year and month, so it's easier to read
  
  gather(key = "year_month", value = "asking_price", -c(areaName, areaType, Borough)) %>%
  
  # Selecting the relevant variables, areaName, year_month, and asking_price
  
  select(areaName, year_month, asking_price) %>%
  
  # year_month is currently grouped together, so this separates the variable into two variables, year and month 
  
  separate(year_month, c("Year", "Month")) %>%
  
  # Creating a new column through mutate, such that the labels read January, February, etc. as opposed to "01" and "02"
  mutate(Month = factor(Month,
                        labels = c("January", "February", "March", "April",
                          "May", "June", "July", "August", "September", "October", "November","December"))) %>%
  
  # Creating a new column that combines areaName and Year columns together, for plot purposes later
  
  mutate(areaNameYear = paste(areaName, Year))

# Define UI for application 
ui <- 
  
  # Creates my navigation bar
  
  navbarPage("New York Apartment Hunt",
             
  # I like the yeti shinytheme!
  
  theme = shinytheme("yeti"),

  # Creates my first panel, the Home page
  
  tabPanel("Home",
           
           # fluidPage helps create a nice layout
           
           fluidPage(
             
             # fluidRow ensures that all my text below is stacked evenly on top of one another
             
             fluidRow(
               
               # First header in a larger font size
               h1("Welcome!"), 
               
               # Second header in a smaller font size
               
               h3("What does this app let you do?"),
               
               # Description of my project 
               
               p("Housing prices in New York have skyrocketed over the past few years. Using data gathered from StreetEasy, this app lets you explore the prices of 2-bedroom rental units in Manhattan's 33 neighborhoods between January 2010 and February 2019."),
               
               # Gives the user the chance to look at the data I used themselves
               
               p("To learn more about this data, click ",
               tags$a(href = "https://streeteasy.com/blog/data-dashboard/",
                      "here.")),
               
               # Creates a break in the text, so it's easier to read
               
               br(),
               
               # Another head, same font size as the second header
               
               h3("How was this app made?"),
               
               # Now you can take a look at my R code and replicate my project!
               
               p("For the R code, please check out my ",
                 tags$a(href = "https://github.com/jenniferli97/NY-Apartment-Hunt",
                        "GitHub repository.")))
             )
           ),
  
  # Creates my second panel, About Us
  
  tabPanel("About Us",
           
           # Like above, this helps create a nice layout
           
           fluidPage(
             
             # Again, this helps ensure that all my text below is stacked evenly on top of one another
             
             fluidRow(
               
               # First header 
               h1("About Us"),
               
               # Second header
               h3("Why Manhattan?"), 
               
               # The answer to the question above! 
               
               p("In October 2018, I signed my offer with Deloitte Consulting in New York. As my roommate next year and I are actively looking for a new place to call home, I thought I could help our apartment hunting process by examining trends in the prices of 2-bedroom apartments to find the best neighborhood for us to live. Although we are only considering living in 10 of Manhattan's 33 neighborhoods due to proximity to our offices, this project includes data for all of Manhattan's neighborhoods. If you're also looking to move to New York soon, I hope this app helps you find the best neighborhood to live in, too!"),
               
               # Helps break up the text, so it's more visually-pleasing
               
               br(),
               
               # Another header!
               
               h3("Where we're working"), 
               
               # Answer to question above
               
               p("My roommate, Lena, will be working at Credit Suisse, which is located in the Flatiron area. My office building is in Midtown. *include map of offices*"),
               
               # Breaks up text
               
               br(),
               
               # Who doesn't love fun facts?
               
               h3("Fun facts"), 
               
               p("Varsity athletes, we currently don't live together, but can see each other's rooms from our windows, we go to Flour Bakery at least 5x every week, and more!")
            
              ))),
  
  # Third panel, price! This is where the interactive graph lies.
  
  tabPanel("Price",
           
           # Creates nice layout
           
          fluidPage(
            
            # Helps ensure that all my text below is stacked evenly on top of one another
            fluidRow(
              
              # First header!
              
              h1("Price Trends"), 
               
              # Breaks up text
              
              br(),
              
              # A description of the page
              
              p("This is an analysis of the median asking rent price for all neighborhoods in Manhattan between January 2000 and February 2019. This allows you to compare and contrast price trends for each of Manhattan's neighborhoods over the past 9 years."), 
              
              # Breaks up text
              
              br(),
  
  # Sidebar layout with input and output definitions
  
    sidebarLayout(
      
      # Sidebar panel for interactive input
      
      sidebarPanel(
        
        # Allows the user to pick which Neighborhoods they want to investigate. Gives the user the option to select all choices if so desired!
        
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
 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        
        # Allows the user to pick which Year they want to investigate. Gives the user the option to select all choices if so desired!
        
        pickerInput("Year", "Year:", choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
                  
        # Allows the user to pick which Month they want to investigate. Gives the user the option to select all choices if so desired!
        
        pickerInput("Month", "Month:", choices = c("January",
                                                   "February",
                                                   "March",
                                                   "April",
                                                   "May",
                                                   "June",
                                                   "July",
                                                   "August",
                                                   "September",
                                                   "October",
                                                   "November",
                                                   "December"),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        
        # Breaks up picker inputs and text below
       
        br(),
        
        # Tells the user that at least one neighborhood, year, and month, must be selected
        
        helpText("Note: You must select at least one neighborhood, one year, and one month to see the graph."),
        
        # Breaks up note below
        
        br(),
        
        # If you play around, you'll notice that sometimes when you select multiple months, there is a break in the line graph. 
        # This means that there was no data for that month, which is what I'm telling the user here.
        
        helpText("Note: If there is no value on the graph, then that means there was no data for that month.")), 
   
        # Standard for Shiny. Creates a main panel containing my output.
        
        mainPanel(
          
          # Calling the plot, rentPlot! 
          
          plotOutput("rentPlot")))

      )
    )
  ),

# My final tab! This is where the "story" lies.

 tabPanel("Takeaways",
          
          # Creates a nice layout
          
          fluidPage(
            
            fluidRow(
              h1("Takeaways"),
              h3("What I Learned"), 
              p("General trends blah blah blah
                            *maybe insert graph of the 10 neighborhoods Lena and I want to live in and describe which neighborhoods we should focus our attention on as a result of this project*"),
              br(),
              h3("Future steps"), 
              p("The dataset I used only looked at 2-bedroom rental units throughout Manhattan. In the future, it would be interesting to compare other units, such as studios, 1-bedroom apartments, and 3-bedroom apartments. It would also be fascinating to look at the square footage of the average rental unit for each neighboorhood and compare that data across neighborhoods. However, because my roommate, Lena, and I are more concerned about the proximity of our apartment to our respective offices, we are not as concerned about the price/square footage of each neighborhood."),
              br(),
              h3("Contact me!"), 
              p("This project was created by Jennifer Li for GOV1005: Data in the Spring of 2019. If you have any questions or suggestions about my app, please feel free to send me a message at jenniferli@college.harvard.edu."),
              br(),
              tags$i(h3("Happy house hunting!"), align = "center")
                  )
                )
             )
           )
  
server <- function(input, output) {
  
  output$rentPlot <- renderPlot({
    
    price <- rent %>%
      filter(Month %in% input$Month) %>%
      filter(areaName %in% input$areaName) %>%
      filter(Year %in% input$Year)
    
    ggplot(data = price, aes(x = Month, y = asking_price, color = areaName)) +
      geom_point() +
      geom_line(aes(linetype = Year, group = areaNameYear)) +
      labs(x = "Month",
           y = "Price in USD ($)",
           color = "Neighborhood",
           title = "Median Rental Price of 2-Bedroom Rental Units by Neighborhood in Manhattan",
           caption = "Source: StreetEasy")
      
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)

