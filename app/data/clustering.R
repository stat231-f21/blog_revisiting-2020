knitr::opts_chunk$set(echo = TRUE)
library(lubridate)
library(factoextra)


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
  
  
  output$bing <- renderPlot({
    tmp <- bing_cluster() %>%
      mutate(Dates = substr(Dates, start = 6, stop = 10)) %>%
      column_to_rownames("Dates")
    tmp_fin <- tmp %>%
      select(Positive, Negative)
    
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
  
  output$nrc <- renderPlot({
    tmp <- nrc_cluster() %>%
      mutate(Dates = substr(Dates, start = 6, stop = 10)) %>%
      column_to_rownames("Dates")
    nrc_kmeans <- kmeans(tmp[c(input$dim1, input$dim2)], input$cluster_nrc, iter.max = 1000, nstart = 5)
    
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
  
  
  
  output$regression <- renderPlot({
    bing_industrials <- bing_cluster() %>%
      inner_join(industrials_raw, by = "Dates")
    industrials_model <- lm(Percentage ~ Ratio, data = bing_industrials)
    bing_industrials %>%
      ggplot(aes(x = Percentage, y = Ratio)) +
      geom_point() +
      geom_smooth(method = "lm",se=F) + 
      labs(x = "Relative position of industrials index",
           y = "Ratio of positive/negative sentiment")
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
                      x ="Number of Clusters", y = "Sum of Squared Error") 
      
  })
}

shinyApp(ui = ui, server = server)