library(tidyverse)
library(shiny)
library(usmap)
library(maps)
library(ggcorrplot)
library(ggplot2)
library(kableExtra)

states <- read_csv("data/state_toplines.csv") %>%
  mutate(date = as.Date(modeldate, "%m/%d/%Y")) %>%
  filter(state != "District of Columbia")

ui <- fluidPage(
  # Slider to input date
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "slider",
                  label = "Date",
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
        mutate("sign" = as.double(margin * (abs(margin))^(-1))) %>%
        mutate("sqrtmargin" = sqrt(abs(margin))) %>%
        mutate("adj_margin" = sqrtmargin * sign) %>%
        filter(date == input$slider) %>%
        select(
          state, adj_margin
        )
    })
    
    #plot map
    plot_usmap(data = data_for_map(),
               values = "adj_margin") + 
      scale_fill_gradient2(high = "#E0010B",
                           mid = "#FFFFFF",
                           low = "#0664BC",
                           midpoint = 0) + 
      theme(legend.title = element_blank(), legend.position = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)