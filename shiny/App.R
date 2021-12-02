library(tidyverse)
library(shiny)
library(ggplot2)
library(kableExtra) # for example code; delete if not needed

input_names <- c("Date",
                 "Stock Market", 
                 "Trading Volume",
                 "Daily COVID Deaths", 
                 "Daily COVID Cases", 
                 "Energy Industry", 
                 "Healthcare Industry", 
                 "Industrials Industry",
                 "Democratic-Linked Companies",
                 "Republican-Linked Companies",
                 "Electoral College Win Probability (D)",
                 "Popular Vote Win Probability (D)",
                 "Expected Electoral Votes (D)",
                 "Expected Vote Percentage (D)",
                 "None"
)

# create input value vector using current variable names from dataset
input_values <- c("date",
                  "snp_close", 
                  "snp_volume",
                  "d_deaths", 
                  "d_cases", 
                  "energy_close", 
                  "health_close", 
                  "industrials_close",
                  "dem_close",
                  "gop_close",
                  "ecwin_chal",
                  "popwin_chal",
                  "ev_chal",
                  "national_voteshare_chal",
                  0
)

# attach variable names to variables
names(input_values) <- input_names

# Define UI for app that creates a line plot for a given name
ui1 <- fluidPage(
  # Application title
  titlePanel("Factors Influencing the National Vote"),
  sidebarLayout(
    sidebarPanel(
      # selector bar for x-axis variable
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = input_values,
                  selected = "date"),
      # selector bar for y-axis variable
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = input_values,
                  selected = "national_voteshare_chal"),
      # selector bar for variable to determine point size
      selectInput(inputId = "sizevar",
                  label = "Size by:",
                  choices = input_values,
                  selected = "d_deaths"),
      # selector bar for variable to determine point color
      selectInput(inputId = "colorvar",
                  label = "Color by:",
                  choices = input_values,
                  selected = "snp_close"),
    ),
    mainPanel(plotOutput(outputId = "scatter"))
  )
)

# Define server logic required to draw a scatterplot
server1 <- function(input, output) {
  output$scatter <- renderPlot({
    # read in data
    pres <- read_csv("scattering.csv")
    # create plot
    ggplot(
      data = pres, 
      aes_string(
        x = input$xvar, 
        y = input$yvar,
        size = input$sizevar,
        color = input$colorvar
      )
    ) + 
      # add scatterplot feature
      geom_point() + 
      theme_bw() + 
      # color for red to blue (for theme)
      scale_colour_gradient(low = "#E0010B", high = "#0664BC") + 
      # add labels and legend
      labs(
        x = input_names[input_values == input$xvar], 
        y = input_names[input_values == input$yvar],
        size = input_names[input_values == input$sizevar],
        color = input_names[input_values == input$colorvar]
      )
  })
}

# Run the application 
shinyApp(ui = ui1, server = server1)