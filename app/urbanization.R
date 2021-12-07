library(tidyverse)
library(shiny)
library(ggplot2)
library(kableExtra) # for example code; delete if not needed
library(dplyr)

# wrangle datasets
Urbanization <- read_csv("data/urbanization-state.csv") %>%
  filter(state != "American Samoa",
         state != "District of Columbia",
         state != "Guam",
         state != "Northern Marianas",
         state != "Puerto Rico",
         state != "Virgin Islands")
urbanstates <- read_csv("data/state_toplines.csv") %>%
  mutate(date = as.Date(modeldate, "%m/%d/%Y")) %>%
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
      scale_color_manual(values = c('Blue', 'Red')) + 
      theme_bw()
  })
}

shinyApp(ui = uiUrb, server = serverUrb)