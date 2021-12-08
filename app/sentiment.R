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

# Create date sequence for row names
dates <- gsub('-',
              '/',
              as.character(seq(as.Date("2020/06/01"),
                               as.Date("2020/11/03"),
                               "days")))

dates_tmp <- gsub('-',
                  '/',as.character(seq(as.Date("2020/06/01"),
                                       as.Date("2020/11/03"),
                                       "days")))

dates_compact <-substr(dates_tmp, start = 6, stop = 10)



# Read in datasets and preprocess rows and columns
titles_raw <- read_csv("data/wsj-titles-full.csv")
industrials_raw <- read_csv("data/vis-industrials.csv")
industrials_raw <- industrials_raw %>%
  # create industrials index standardization
  mutate(volatility = (High - Low)/Volume*1000000,
         Percentage = (Close - min(Low))/(max(High)-min(Low)),
         adj_volume = (Volume - min(Volume))/(max(Volume) - min(Volume))) %>%
  rename("Dates" = "Date")

# read in lexicon indices
bing <- read_csv("data/bing.csv")[,2:3]
nrc <- read_csv("data/nrc.csv")[,2:3]
nrc_emotions <- c("Anger",
                  "Fear", 
                  "Anticipation",
                  "Trust",
                  "Surprise",
                  "Sadness",
                  "Joy",
                  "Disgust")


# Wrangle text matrix
titles_compact <- titles_raw %>%
  mutate(Raw_Words = apply(titles_raw[3:113],1,
                           function(x) paste(x, collapse = " "))) %>%
  select(Dates, Raw_Words)

# Wrangle text matrix
titles_expanded <- titles_compact %>%
  select(Dates, Raw_Words) %>%
  # split titles into words
  mutate(word = strsplit(as.character(Raw_Words), " ")) %>%
  unnest(word) %>%
  select(Dates,word) %>%
  # remove stopwords
  anti_join(get_stopwords(), by = "word") %>%
  # make words lowercase to match lexicons
  mutate(word = tolower(word))

ui <- fluidPage (
  
  tabPanel(
    title = "Sentiment Analysis - Bing Lexicon",
    sidebarLayout(
      sidebarPanel(
        # Slider input for date range 
        sliderInput("date2_bing",
                    "Date range",
                    min = ymd("2020-06-01"),
                    max = ymd("2020-11-03"),
                    value = c(ymd("2020-06-01"), ymd("2020-06-30")),
                    timeFormat = "%Y-%m-%d",
                    animate = animationOptions(
                      interval = 1000,
                      loop = TRUE)
        ),
        sliderInput("cluster_bing",
                    "Number of Clusters",
                    min = 1,
                    max = 10,
                    value = 3,
                    animate = animationOptions(
                      interval = 1000,
                      loop = TRUE)
        )),
      mainPanel(plotOutput(outputId = "bing"),
                plotOutput("regression")))),
  
  tabPanel(
    title = "Sentiment Analysis - NRC Lexicon",
    sidebarLayout(
      sidebarPanel(
        # Slider input for date range  
        sliderInput("date2_nrc",
                    "Date range",
                    min = ymd("2020-06-01"),
                    max = ymd("2020-11-03"),
                    value = c(ymd("2020-06-01"), ymd("2020-06-30")),
                    timeFormat = "%Y-%m-%d",
                    animate = animationOptions(
                      interval = 1000,
                      loop = TRUE)
        ),
        sliderInput("cluster_nrc",
                    "Number of Clusters",
                    min = 1,
                    max = 10,
                    value = 3,
                    animate = animationOptions(
                      interval = 1000,
                      loop = TRUE)
        ),
        # create selection menus for emotions on axes
        selectInput("dim1",
                    "NRC - dimension 1", 
                    choices = nrc_emotions, selected = "Anger"),
        
        selectInput("dim2",
                    "NRC - dimension 2", 
                    choices = nrc_emotions, selected = "Surprise")
      ),
      mainPanel(plotOutput(outputId = "nrc"),
                plotOutput("cost")))))


server <- function(input, output) {
  # Create data for cloud 
  bing_cluster <- reactive({
    data <- bing_data <-titles_expanded %>%
      inner_join(bing, by = "word") %>%
      group_by(Dates) %>%
      mutate(Positive = sum(ifelse(sentiment == "positive",1,0)),
             Negative = sum(ifelse(sentiment == "negative",1,0)),
             Ratio = Positive/Negative) %>%
      select(Dates, Positive, Negative, Ratio) %>%
      distinct() %>%
      filter(Dates >= ymd(input$date2_bing[1]), Dates <= ymd(input$date2_bing[2]))
    
  })
  # create nrc cluster
  nrc_cluster <- reactive({
    data <- titles_expanded %>%
      filter(Dates >= ymd(input$date2_nrc[1]),
             Dates <= ymd(input$date2_nrc[2])) %>%
      inner_join(nrc, by = "word") %>%
      group_by(Dates) %>%
      mutate(Anger = sum(sentiment == "anger"),
             Fear = sum(sentiment == "ear"),
             Anticipation = sum(sentiment == "anticipation"),
             Trust = sum(sentiment == "trust"),
             Surprise = sum(sentiment == "surprise"),
             Sadness = sum(sentiment == "sadness"),
             Joy = sum(sentiment == "joy"),
             Disgust = sum(sentiment == "disgust")) %>%
      select(-c(word, sentiment)) %>%
      distinct() 
    
  })
  
  # create bing lexicon cluster graph
  output$bing <- renderPlot({
    tmp <- bing_cluster() %>%
      mutate(Dates = substr(Dates, start = 6, stop = 10)) %>%
      column_to_rownames("Dates")
    tmp_fin <- tmp %>%
      select(Positive, Negative)
    # visualize clusters
    fviz_cluster(kmeans(tmp_fin, input$cluster_bing, iter.max = 1000, nstart = 5),
                 data = tmp_fin,
                 axes = c("Positive Sentiment","Negative Sentiment"),
                 palette = "Set1", 
                 ellipse.type = "convex",
                 labelsize = 8,
                 main = "K-Means clustering with the Bing lexicon",
                 ggtheme = theme_bw()) +
      theme(legend.position="none")
  })
  
  # create nrc cluster plot
  output$nrc <- renderPlot({
    tmp <- nrc_cluster() %>%
      mutate(Dates = substr(Dates, start = 6, stop = 10)) %>%
      column_to_rownames("Dates")
    # create in kmeans clustering for given emotions
    nrc_kmeans <- kmeans(tmp[c(input$dim1, input$dim2)], input$cluster_nrc, iter.max = 1000, nstart = 5)
    
    # visualize plot
    fviz_cluster(nrc_kmeans,
                 data = tmp,
                 choose.vars = c(input$dim1, input$dim2),
                 palette = "Set1", 
                 ellipse.type = "convex",
                 labelsize = 8,
                 main = "K-Means clustering with the NRC lexicon",
                 ggtheme = theme_bw()) + 
      theme(legend.position="none")
  })
  
  
  # create regression of WSJ sentiment on industrials index and scatterplot
  output$regression <- renderPlot({
    bing_industrials <- bing_cluster() %>%
      inner_join(industrials_raw, by = "Dates")
    industrials_model <- lm(Percentage ~ Ratio, data = bing_industrials)
    bing_industrials %>%
      ggplot(aes(x = Percentage, y = Ratio)) +
      geom_point() +
      geom_smooth(method = "lm",se=F) + 
      labs(x = "Relative position of industrials index",
           y = "Ratio of positive/negative sentiment") + 
      theme_bw()
  })
  
  
  output$cost <- renderPlot({
    
    kmeans_data <- nrc_cluster()[c(input$dim1, input$dim2)]
    cost <- c()
    for(i in 1:10){
      nrc_kmeans <- kmeans(kmeans_data, i, iter.max = 1000, nstart = 5)
      tmp_cost <- 0
      for(a in 1:nrow(kmeans_data)){
        tmp_cost <- tmp_cost + norm(kmeans_data[a,] - nrc_kmeans$centers[nrc_kmeans$cluster[a],],type = "2")
      }
      cost <- append(cost, tmp_cost)
    }
    cost_plot <- cbind(1:10, cost)
    df_cost <- data.frame(cost_plot)
    df_cost %>%
      ggplot(aes(x = V1, y = cost)) +
      geom_line() +
      labs(title="Sum of Squared Error by Number of Clusters",
           x ="Number of Clusters", y = "Sum of Squared Error") + 
      theme_bw()
    
  })
}

shinyApp(ui = ui, server = server)