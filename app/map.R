library(tidyverse)
library(shiny)
library(usmap)
library(maps)
library(ggcorrplot)
library(ggplot2)
library(kableExtra)

# read in dataset, change dates to date and remove DC (major outlier that also does not appear on map)
states <- read_csv("data/state_toplines.csv") %>%
  mutate(date = as.Date(modeldate, "%m/%d/%Y")) %>%
  filter(state != "District of Columbia")

ui <- fluidPage(
  # Slider to input date
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "slider",
                  label = "Date",
                  # set dates as minimum and maximum values for slider
                  min = as.Date("6/1/2020","%m/%d/%Y"),
                  max = as.Date("11/3/2020","%m/%d/%Y"),
                  value = as.Date("11/3/2020", "%m/%d/%Y"),
                  timeFormat = "%m/%d/%Y")
    ),
    mainPanel(plotOutput(outputId = "map"))
  )
)


# Define server logic required to draw a scatterplot
server <- function(input, output) {
  output$map <- renderPlot({
    # select date for data
    data_for_map <- reactive({
      data <- states %>%
        select(
          state, date, margin
        ) %>%
        # mapping margin alone makes most of the map look pale; use square root to amplify the differences between states with small margins (close states)
        mutate(
          # save whether each state is biden or trump-supporting
          "sign" = as.double(margin * (abs(margin))^(-1)), 
          # square root the absolute value of the margin (some margins are negative)
          "sqrtmargin" = sqrt(abs(margin)), 
          # bring back the correct sign (biden or trump) to the square-rooted margin as the final measure of lean for the map
          "adj_margin" = sqrtmargin * sign) %>%
        # choose the adjusted margins for the input date (dataset contains entries for each state and each date)
        filter(date == input$slider) %>%
        select(
          state, adj_margin
        )
    })
    
    #plot map
    plot_usmap(data = data_for_map(),
               values = "adj_margin") + 
      # color in states, high values (positive margins) are red for Trump, low are blue for Biden
      # use gradient2 to set a midpoint (makes closest states look more pale as opposed to purple)
      scale_fill_gradient2(high = "#E0010B",
                           mid = "#FFFFFF",
                           low = "#0664BC",
                           midpoint = 0) + 
      # remove legend
      theme(legend.title = element_blank(), legend.position = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)