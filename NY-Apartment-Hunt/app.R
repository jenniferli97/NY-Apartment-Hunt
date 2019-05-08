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
                        "GitHub repository."))),
             
             # Used break for formatting purposes
             
             br(),
            
             imageOutput("manhattan"),
             
             # Used breaks to add the source, as shown below
             
             br(),
             
             # Used breaks to add the source, as shown below 
             
             br(),
             
             # Used breaks to add the source, as shown below
             
             br(),
             
             # Used breaks to add the source, as shown below
             
             br(),
             
             # Used breaks to add the source, as shown below
             
             br(),
             
             # Used breaks to add the source, as shown below
             
             br(),
             
             # Adds the source to the image of Manhattan's skyline
             
             h5("Source: Viator", align = "center")
            
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
               
               p("In October 2018, I signed my offer with Deloitte Consulting in New York. My roommate next year, Lena, will be working at Credit Suisse. As we are actively looking for a new place to call home, I thought I could help our apartment hunting process by examining trends in the prices of 2-bedroom apartments to find the best neighborhood for us to live. This project includes data for all of Manhattan's neighborhoods. If you're also looking to move to New York soon, I hope this app helps you find the best neighborhood to live in, too!"),
               
               # Helps break up the text, so it's more visually-pleasing
               
               br(),
               
               # Another header!
               
               h3("Where we're working"), 
               
               # Answer to question above
               
               p("Lena's office is located in Flatiron, while my building is in Midtown. Lena and I both hope to live within walking distance from our offices (with some flexibility, if we find an amazing apartment!). Since our offices are 1.5 miles away from each other, it seems to make the most sense for us to look at apartments just below Central Park and right above Gramercy Park. Let's see what the median rental prices look like on the Prices tab!"), 
               
               # Breaks up text
               
               br(),
               
               # Who doesn't love fun facts?
               
               h3("Bonus: Fun Facts"), 
               
               # Sets up my ordered list
               
               tags$ul(
                 
                 # First bullet point, describing first fun fact
                 
                 tags$li("Both varsity athletes (she's on the sailing team and I'm on the rowing team)"),
                 
                 # Second bullet point, describing second fun fact
                 
                 tags$li("Currently don't live together, but can see each other's rooms from our windows"),
                 
                 # Final bullet point, describing third fun fact. Use tags$a to create an HTML link to Flour's website
                 
                 tags$li("We have an unhealthy obsession with",
                        tags$a(href = "https://flourbakery.com/", "Flour Bakery.")) 
               )

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
              
              p("This is an analysis of the monthly median asking rental price for all neighborhoods in Manhattan between January 2000 and February 2019. This allows you to compare and contrast price trends for each of Manhattan's neighborhoods over the past 9 years."), 
              
              # Breaks up text
              
              br(),
  
  # Sidebar layout with input and output definitions
  
    sidebarLayout(
      
      # Sidebar panel for interactive input
      
      sidebarPanel(
        
        # Allows the user to pick which Neighborhoods they want to investigate. Gives the user the option to select all choices if so desired! Default is the 9 neighborhoods we are considering living in.
        
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
                                                            "Midtown South",
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
                    
                                                  select = c("Battery Park City",
                                                             "Central Park South",
                                                             "Chelsea",
                                                             "Flatiron",
                                                             "Gramercy Park",
                                                             "Midtown",
                                                             "Midtown East",
                                                             "Midtown South",
                                                             "Midtown West"),
 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        
        # Allows the user to pick which Year they want to investigate. Gives the user the option to select all choices if so desired! Default is 2018.
        
        pickerInput("Year", "Year:", choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                                     select = c("2018"),
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
                                      
                                      select = c("January",
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
          
          plotOutput("rentPlot"))),
  
          br(),
          
          h3("What do you notice?"),
          
          p("The default neighborhoods, year, and month show the 9 neighborhoods that Lena and I are interested in living in, due to proximity to our offices. It seems that we should start our apartment hunt by looking for rental units in Gramercy Park and Midtown East! Feel free to explore the data yourself.")
          
  
      )
    )
  ),

# My final tab! This is where the "story" lies.

 tabPanel("Takeaways",
          
          # Creates a nice layout
          
          fluidPage(
            
            # Ensures that all my text below is stacked evenly on top of one another
            
            fluidRow(
              
              #First header
              
              h1("Takeaways"),
              
              #Second header
              
              h3("What I Learned"), 
              
              # Description of key takeaways
              
              p("In general, rental prices seem to spike in May and October for almost every neighborhood and every year between 2000 and 2019. 
                For 2018, Central Park South had the highest median rental price (around $9,000) out of all the neighborhoods in Manhattan, while Hamilton Heights and Marble Hill appeared to be the cheapest (<$2300)."),
              
              # Breaks up text, makes it easier to read
             
              br(),
              
              # Header, similar font size to second header
              
              h3("Future steps"), 
              
              # What could be improved!
              
              p("The dataset I used only looked at 2-bedroom rental units throughout Manhattan. In the future, it would be interesting to compare other units, such as studios, 1-bedroom apartments, and 3-bedroom apartments. It would also be fascinating to look at the square footage of the average rental unit for each neighboorhood and compare that data across neighborhoods. However, because my roommate, Lena, and I are more concerned about the proximity of our apartment to our respective offices, we are not as concerned about the price/square footage of each neighborhood."),
              
              # Breaks up text
              
              br(),
              
              # Final header
              
              h3("Contact me!"), 
              
              # My contact information
              
              p("This project was created by Jennifer Li for GOV1005: Data, Spring 2019. If you have any questions or suggestions about my app, please feel free to send me a message at jenniferli@college.harvard.edu."),
              
              # Breaks up text
              
              br(),
              
              # Italicizes and centers my text
              
              tags$i(h3("Happy house hunting!"), align = "center")
                  )
                )
             )
           )

# Server code

server <- function(input, output) {
  
  # Renders a photo of the Manhattan skyline on my home page
  
  output$manhattan <- renderImage({
    
    # Calls the Manhattan photo and aligns the image to the middle
    
    list(src="manhattan.jpg", style="display: block; margin-left: auto; margin-right: auto;",
         contentType="image/gif")
  }, deleteFile = FALSE)
  
  # Renders a reactive plot for rentPlot
  
  output$rentPlot <- renderPlot({
    
    # Storing rent as price for future use
    
    price <- rent %>%
      
      # Filters Month by referring to the Month inputs
      
      filter(Month %in% input$Month) %>%
      
      # Filters areaName by referring to areaName inputs
      
      filter(areaName %in% input$areaName) %>%
      
      # Filters Year by referring to Year inputs
      
      filter(Year %in% input$Year)
    
    # Creates a ggplot using the price dataset, with Month as the x-axis and asking_price as the y-axis.
    # Set color to areaName, so each neighborhood would represent a different color
    
    ggplot(data = price, aes(x = Month, y = asking_price, color = areaName)) +
      
      # Creates a point graph of the selected data
      
      geom_point() +
      
      # Creates a line graph, connecting the points by areaName and Year
      
      geom_line(aes(linetype = Year, group = areaNameYear)) +
      
      # Defines the x- and y-axes, color changes areaName to Neighborhood, which is a more accurate label
      # for the legend. Also creates a title and caption, so that the reader understands what they are 
      # looking at
  
      labs(x = "Month",
           y = "Price in USD ($)",
           color = "Neighborhood",
           title = "Median Rental Price of 2-Bedroom Rental Units by Neighborhood in Manhattan",
           caption = "Source: StreetEasy")
      
  })
  
}


# Runs the application
shinyApp(ui = ui, server = server)

