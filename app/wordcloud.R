library(tidyverse)
library(shiny)
library(ggplot2)
library(kableExtra) # for example code; delete if not needed
library(dplyr)
library(tidytext)
library(magrittr)
library(stopwords)
library(lubridate)
library(factoextra)
library(wordcloud)

# data wrangling for the wordcloud and scatterplot using WSJ data
dates <- gsub('-',
              '/',
              as.character(seq(as.Date("2020/06/01"),
                               as.Date("2020/11/03"),
                               "days")))

dates_tmp <- gsub('-',
                  '/',as.character(seq(as.Date("2020/06/01"),
                                       as.Date("2020/11/03"),
                                       "days")))
# create dates vector
dates_compact <-substr(dates_tmp, start = 6, stop = 10)

# read in healthcare and WSJ data
titles_raw <- read_csv("wsj-titles-full.csv")
health_raw <- read_csv("vht-health.csv")

# read in health industry data
health_raw <- health_raw %>%
  mutate(volatility = (High - Low)/Volume*1000000)

titles_compact <- titles_raw %>%
  mutate(Raw_Words = apply(titles_raw[3:113],1,
                           function(x) paste(x, collapse = " "))) %>%
  select(Dates, Raw_Words)

titles_expanded <- titles_compact %>%
  set_rownames(dates_compact) %>%
  select(Dates, Raw_Words) %>%
  mutate(word = strsplit(as.character(Raw_Words), " ")) %>%
  unnest(word) %>%
  select(Dates,word) %>%
  anti_join(get_stopwords(), by = "word") %>%
  mutate(word = tolower(word))

titles_tf_idf <- titles_raw %>%
  mutate(Raw_Words = apply(titles_raw[3:113],1,
                           function(x) paste(x, collapse = " "))) %>%
  select(Dates, Raw_Words) %>%
  mutate(Words = strsplit(as.character(Raw_Words), " ")) %>%
  unnest(Words) %>%
  select(Dates,Words) %>%
  count(Dates, Words) %>%
  bind_tf_idf(Words, Dates, n) %>%
  arrange(desc(tf_idf))

rownames(titles_raw) <- dates_compact
rownames(titles_compact) <- dates_compact

uiWC <- navbarPage(
  title = "WSJ Word Cloud",
    sidebarLayout(
      sidebarPanel(
        # slider to input date range
        sliderInput("slider",
                    "Date range",
                    min = as.Date("2020-06-01","%Y-%m-%d"),
                    max = as.Date("2020-11-03","%Y-%m-%d"),
                    value = c(as.Date("2020-06-01"),as.Date("2020-11-03")),
                    timeFormat = "%Y-%m-%d")
      ),
      
      mainPanel(plotOutput(outputId = "cloud"))
    )
  )

############
# server   #
############
serverWC <- function(input, output){
  
  # TAB 1: Word Cloud
  data_for_cloud <- reactive({
    # filter out data not in the date range
    data <- filter(titles_raw, Dates <= input$slider)
  })
  
  # plot wordcloud
  output$cloud <- renderPlot({
    tmp <- data_for_cloud() %>% 
      pivot_longer(
        -Dates,
        names_to = "title_pos",
        values_to = "title_text"
      ) %>%
      # separate each word from the WSJ titles
      unnest_tokens(word, title_text) %>%
      # remove stopwords
      anti_join(get_stopwords(), by = "word") %>%
      select(Dates, word) %>%
      # count number of instances for each word
      count(word, sort = TRUE) %>%
      # remove prominent words without meaning (New York is not an important event, but is frequently mentioned because of the WSJ's headquarter location)
      filter(!(word %in% c("new","u.s","review","york", "popular", "news", "recommended", "videos", "coronavirus", "19", "covid")))
    # Get rid of coronavirus, new, trump, covid, 19
    # create wordcloud
    wordcloud(
      words = tmp$word,
      freq = tmp$n,
      max.words = 100,
      random.order = FALSE,
      color = tmp$n,
      scale=c(3.5,0.25)
    )
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = uiWC, server = serverWC)