library(tidyverse)
library(shiny)
library(ggcorrplot)
library(ggplot2)
library(kableExtra)

# data wrangling for correlation heatmap
MidwestCor <- read_csv("data/MidwestCor.csv") %>%
  select(-State)
# add state names
row.names(MidwestCor) <- c("Wisconsin",
                           "South Dakota",
                           "Ohio",
                           "North Dakota",
                           "Nebraska",
                           "Missouri",
                           "Minnesota",
                           "Michigan",
                           "Kansas",
                           "Iowa",
                           "Indiana",
                           "Illinois")

NortheastCor <- read_csv("data/NortheastCor.csv") %>%
  select(-State)
row.names(NortheastCor) <- c("Vermont",
                             "Rhode Island",
                             "Pennsylvania",
                             "New York",
                             "New Jersey",
                             "New Hampshire",
                             "Massachusetts",
                             "Maine",
                             "Connecticut")

SouthCor <- read_csv("data/SouthCor.csv") %>%
  select(-State)
row.names(SouthCor) <- c("West Virginia",
                         "Virginia",
                         "Texas",
                         "Tennessee",
                         "South Carolina",
                         "Oklahoma",
                         "North Carolina",
                         "Mississippi",
                         "Maryland",
                         "Louisiana",
                         "Kentucky",
                         "Georgia",
                         "Florida",
                         "Delaware",
                         "Arkansas",
                         "Alabama")

WestCor <- read_csv("data/WestCor.csv") %>%
  select(-State)
row.names(WestCor) <- c("Wyoming",
                        "Washington",
                        "Utah",
                        "Oregon",
                        "New Mexico",
                        "Nevada",
                        "Montana",
                        "Idaho",
                        "Hawaii",
                        "Colorado",
                        "California",
                        "Arizona",
                        "Alaska")

# combine each region into a list to build vector needed for heatmap function
correlation_vector <- list(MidwestCor,
                           NortheastCor,
                           SouthCor,
                           WestCor)

#create an input name vector
region_input_names <- c("Midwest",
                        "Northeast",
                        "South",
                        "West")
region_input_values <- c(1, 2, 3, 4)
names(region_input_values) <- region_input_names


shinyApp(
  ui = fluidPage(
    titlePanel("Correlation Heatmap of Polling Results by Region"),
    sidebarPanel(
      # create menu to select region
      selectInput(
        inputId = "region",
        label = "Region:",
        choices = region_input_values,
        selected = 1
      )
    ),
    mainPanel(plotOutput(outputId = "themap"))
  ), 
  server = function(input, output) {
    output$themap <- renderPlot({
      # use ggcorrplot package to create heatmap
      # use index for input region to select the correct region from the correlation vector
      ggcorrplot(as.data.frame(correlation_vector[as.numeric(input$region)])) 
    })
  }
)