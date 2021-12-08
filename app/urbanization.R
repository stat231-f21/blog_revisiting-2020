library(tidyverse)
library(shiny)
library(ggplot2)
library(kableExtra) # for example code; delete if not needed
library(dplyr)

# wrangle datasets
Urbanization <- read_csv("data/urbanization-state.csv") %>%
  # remove territories with no corresponding electoral data
  filter(state != "American Samoa",
         state != "District of Columbia",
         state != "Guam",
         state != "Northern Marianas",
         state != "Puerto Rico",
         state != "Virgin Islands")
# read in state election data
urbanstates <- read_csv("data/state_toplines.csv") %>%
  mutate(date = as.Date(modeldate, "%m/%d/%Y")) %>%
  # drop congressional districts with no corresponding urbanization
  filter(state != "District of Columbia",
         state != "NE-3",
         state != "NE-2",
         state != "NE-1",
         state != "ME-1",
         state != "ME-2") %>%
  select(state, margin, date)

urbanstatesnov3 <- urbanstates %>%
  filter(date == "2020-11-03") %>%
  inner_join(Urbanization)
urbanstatesnov3$winner <- ifelse(urbanstatesnov3$margin > 0, "Trump", "Biden")
statenamesvector <- unique(urbanstatesnov3$state)

# create SHINY Ui
uiUrb <- fluidPage (
  titlePanel("Urbanization by State and Winning Candidates"),
  # create menu with states listed
  selectInput(inputId = "state",
              label = "Choose Which States to Include (all states are currently selected): ",
              choices = statenamesvector,
              selected = statenamesvector,
              multiple = TRUE),
  mainPanel(plotOutput(outputId = "urbanscat"))
)


# Create SHINY server
serverUrb <- function(input, output) {
  output$urbanscat <- renderPlot({
    # filter out unselected states from input to produce scatterplot
    data <- urbanstatesnov3 %>%
      filter(state %in% input$state)
    ggplot(data = data,
           aes(x = urbanindex,
               y = margin,
               color = winner)) +
      geom_point() +
      labs(title = "Urbanization vs. Margin",
           x = "Urbanization Index",
           y = "Margin") + 
      # add color
      scale_color_manual(values = c('Blue', 'Red')) + 
      theme_bw()
  })
}

shinyApp(ui = uiUrb, server = serverUrb)