#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(mclust)
library(tidyverse)
data = readRDS("data.rds")
countries = data$country %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gaussian Mixture Models"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(
        inputId = "groups_number", 
        label = "Number of groups", 
        value = 2, min = 1, max = 10
      ),
      
      selectInput(
        inputId = "model_name", label = "Model name (Family, Volume, Shape, Orientation)", 
        choices = c(
          "Spherical-Equal-Equal-NA (EII)" = "EII",
          "Spherical-Variablle-Equal-NA (VII)" = "VII",
          "Diagonal-Equal-Equal-Axes (EEI)" = "EEI",
          "Diagonal-Variable-Equal-Axes (VEI)" = "VEI",
          "Diagonal-Equal-Variable-Axes (EVI)" = "EVI",
          "Diagonal-Variable-Variable-Axes (VVI)" = "VVI",
          "General-Equal-Equal-Equal (EEE)" = "EEE",
          "General-Equal-Variable-Equal (EVE)" = "EVE",
          "General-Variable-Equal-Equal (VEE)" = "VEE",
          "General-Variable-Variable-Equal (VVE)" = "VVE",
          "General-Equal-Equal-Variable (EEV)" = "EEV",
          "General-Variable-Equal-Variable (VEV)" = "VEV",
          "General-Equal-Variable-Variable (EVV)" = "EVV",
          "General-Variable-Variable-Variable (VVV)" = "VVV"
        )
      ),
      
      selectInput(
        inputId = "country_selected",
        label = "Country",
        choices = countries
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  gmm_fit_reac = reactive({
    input$groups_number 
    input$model_name
    isolate({
      Mclust(
        data = data[, 1:4], 
        G = input$groups_number, 
        modelNames = input$model_name
      )
    })
    
  })
  
  output$distPlot = renderPlot({
    
    gmm_fit = gmm_fit_reac()
    
    gmm_fit$z[data$country == input$country_selected, ] %>%
      as.data.frame() %>%
      mutate(t = 1:nrow(.)) %>%
      gather(z, prob, -t) %>%
      ggplot(aes(x=t, y=prob, color=z)) + 
      geom_point() + 
      geom_line()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
