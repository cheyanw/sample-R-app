#Creating a map and plot to compare NPS data in California~~~~~~~~~~~~~~~~~~~~~

#Load Packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(leaflet)
library(shiny)
library(leaflet.minicharts)

#Set Working Directory~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Sample")

#Get Data Ready~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parks <- read.csv("parks.csv") %>%
  filter(State == "CA")

species <- read.csv("species.csv") 

df <- merge(parks, species,all.x=T) %>%
  select("Park.Name", "Latitude", "Longitude", "Category", "Nativeness", 
         "Conservation.Status", "Acres", "Order")

#Beginning of UI~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  #Title the Page~
  titlePanel("National Parks in California: A Brief Comparison"),
  #Begin Side Layout~
  sidebarLayout(
    sidebarPanel(
      h5("Choose a park to view comparison of species type present."),
      #Select Park for Plots~
      selectInput(inputId = "Park.Name", 
                  label = "Park Name:", 
                  choices = unique(df$Park.Name)),
      plotOutput("plot"),
      width = 6),
    position = c("right"),
    #Place for Map~
    mainPanel(
      h3("Acreage of Parks"),
      h5("Click for park name."),
      leafletOutput("map"), 
      width = 6)
  )
)

#Server Time~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function (input, output) {
  
  dfInput <- reactive({
    df %>% filter(Park.Name == input$Park.Name)
  })
  #Create Leaflet Map~
  output$map <- renderLeaflet({
    CA_map <-  leaflet() %>%
      setView(lng = -119, lat = 36.85042, zoom= 5) %>%
      addTiles() %>% 
      addMinicharts(
           df$Longitude, df$Latitude,
            showLabels = TRUE,
            chartdata = df$Acres,
            width = 60,
           #Pop Up Stuff~
           popup = popupArgs(
             html = paste0(
               "<div>",
               "<h4>",
               df$Park.Name,
               "</h4>",
               "Acreage: ",
               df$Acres,
               "</div>"
             )
           )
        )
  })
  #Plotting Species~
  output$plot <- renderPlot({
    df1 <- dfInput()
    ggplot(df1, aes(Category)) + 
      geom_bar(fill = "#7ACF90") +
      theme_minimal() +
      labs(x = "Species Category", y = "Number of Species") +
      coord_flip()
  })
}

#Ready to run!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)