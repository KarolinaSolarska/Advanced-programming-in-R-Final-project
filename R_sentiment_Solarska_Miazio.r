# Checking if required packages are installed, and install them if necessary
if (!require(AER)) {
  install.packages(c("shiny", "tidyverse", "tidytext", "wordcloud", "R6", "sentiment"))
}
# # Load necessary libraries
library(devtools)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(plotly)
library(shiny)
library(stringr)
library(tidytext)
library(tidyverse)
library(tm)
library(wordcloud)
library(plotrix)
library(sentiment)
library(R6)

# Install the package
# path_to_package <- "sentiment_0.0.0.9000.tar.gz"
# install.packages(path_to_package, repos = NULL, type = "source")

# Reading the data
data <- read.csv('Womens Clothing E-Commerce Reviews.csv')
data<- data[1:200, ]
# data

# Sentiment analysis on the data
df_with_sentiment <- addSentimentToDf(data, "Review.Text")
df_with_sentiment$sentiment_score <- df_with_sentiment$positive - df_with_sentiment$negative
summary(df_with_sentiment)

# Creating a class for text preprocessing
textPreprocessing <- R6Class("textPreprocessing",
                             public = list(
                               getAllWordsSentiments2 = function(column){
                                 all_words <- data.frame()
                                 for (i in 1:length(column)){
                                   tokens <- tidytext::unnest_tokens(data.frame(text = column), word, text)
                                   sentiment <- merge(tokens, tidytext::get_sentiments("bing"), by = "word")
                                   all_words<- rbind(all_words, sentiment)
                                 }
                                 
                                 return(sentiment)
                               },
                               changeText = function(df, column) {
                                 preprocess_corpus <- VCorpus(VectorSource(df$Review.Text))
                                 
                                 corpus <- tm_map(preprocess_corpus, removePunctuation)
                                 corpus <- tm_map(corpus, content_transformer(tolower))
                                 corpus <- tm_map(corpus, removeWords, stopwords("english"))
                                 corpus <- tm_map(corpus, removeNumbers)
                                 corpus <- tm_map(corpus, stripWhitespace)
                                 return(corpus)
                               }
                             )
)



processor<-textPreprocessing$new()
processor$getAllWordsSentiments2(data$Review.Text)
df<- processor$getAllWordsSentiments2(data$Review.Text)

################################### Shiny app ###################################

# Defining the UI
ui <- fluidPage(
  
  headerPanel("Clothing reviews sentiment analysis"),
  
  # Getting User Inputs
  
  sidebarPanel(
    textInput("text_input", "Enter your review"),
    actionButton("analyze_btn", "Analyse", class = "btn-primary") 
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Sentiment Analysis",
               h3("Sentiment Analysis Results"),
               HTML("<div><h4>Positive:</h4></div>"),
               verbatimTextOutput("p_sentiment_output"),
               HTML("<div><h4>Negative:</h4></div>"),
               verbatimTextOutput("n_sentiment_output"),
               HTML("<div><h4>Sentiment Score:</h4></div>"),
               verbatimTextOutput("sentiment_output")
      ),
      
      tabPanel("WordCloud",
               HTML("<div><h3>Positive Sentiment WordCloud</h3></div>"),
               plotOutput("wordcloud_p"),
               HTML("<div><h3>Negative Sentiment WordCloud</h3></div>"),
               plotOutput("wordcloud_n"),
               HTML("<div><h4>The word cloud provides a quick overview of the most commonly used words in the reviews. It represents words as varying sizes, with larger sizes indicating higher frequency. </h4></div>"),
               
      ),
      
      tabPanel("Histogram",
               HTML("<div><h3>Histogram of positive sentiment</h3></div>"),
               plotOutput("histogram_pos"),
               HTML("<div><h3>Histogram of negative sentiment</h3></div>"),
               plotOutput("histogram_neg"),
               HTML("<div><h3>Histogram of sentiment score</h3></div>"),
               plotOutput("histogram_score"),
               HTML("<div><h4>The histogram visually represents the distribution of positive and negative sentiment and score.</h4></div>"),
      ),
      
      tabPanel("Pie Chart",
               HTML("<div><h3>Pie Chart</h3></div>"),
               plotOutput("piechart"),
               HTML("<div><h4>A pie chart represents the distribution of sentiment categories in the clothing e-commerce reviews. It divides the data into different sectors, with each sector representing a sentiment category (positive or negative). The size of each sector corresponds to the proportion or percentage of reviews belonging to that sentiment category.</h4></div>")
      ),
      
      tabPanel("Bar Plot",
               HTML("<div><h3>Bar Plot</h3></div>"),
               plotOutput("barplot"),
               HTML("<div><h3>Grouped Bar Plot</h3></div>"),
               plotOutput("grouped_barplot"),
               HTML("<div><h4>The bar plot allows for a comparison of sentiment scores across departments</h4></div>"),
      ),
      
      tabPanel("Summary Statistics",
               HTML("<div><h3>Summary Statistics</h3></div>"),
               tableOutput ("summary_stats"),
               verbatimTextOutput("null_counts"),
               HTML("<div><h4>The summary statistics provide a concise summary of the sentiment scores in the clothing e-commerce reviews dataset.</h4></div>"),
      )
    ) 
  ) 
  
)

# Defining the server
server <- function(input, output) {
  
  # Positive sentiment score
  p_sentiment_score <- eventReactive(input$analyze_btn, {
    text <- input$text_input
    sentiment <- GetPositiveSentiment(text)
    if (sentiment == 1) {
      paste("yes, Score:", sentiment)
    } else {
      paste("no, Score:", sentiment)
    }
  })
  
  # Negative sentiment score
  n_sentiment_score <- eventReactive(input$analyze_btn, {
    text <- input$text_input
    sentiment <- GetNegativeSentiment(text)
    if (sentiment == 1) {
      paste("yes, Score:", sentiment)
    } else {
      paste("no, Score:", sentiment)
    }
  })
  
  # Overall sentiment score
  sentiment_score <- eventReactive(input$analyze_btn, {
    text <- input$text_input
    sentiment <- GetSentiment(text)
    sentiment
  })
  
  # Render positive sentiment score
  output$p_sentiment_output <- renderText({
    p_sentiment_score()
  })

  # Render negative sentiment score
  output$n_sentiment_output <- renderText({
    n_sentiment_score()
  })
  
  # Render overall  sentiment score
  output$sentiment_output <- renderText({
    sentiment_score()
  })  
  

  # WORDCLOUD
  
  # Creating a reactive function for positive sentiment word cloud
  text_word_positive <- reactive({
    preprocess_corpus <- VCorpus(VectorSource(df[df$sentiment == "positive", "word"]))
    corpus <- tm_map(preprocess_corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
  })

  # Creating a reactive function for negative  sentiment word cloud  
  text_word_negative <- reactive({
    preprocess_corpus <- VCorpus(VectorSource(df[df$sentiment == "negative", "word"]))
    corpus <- tm_map(preprocess_corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
  })
  
  # Rendering the positive sentiment word cloud
  output$wordcloud_p <- renderPlot({
    wordcloud(
      words = df$word[df$sentiment == "positive"],
      random.order = FALSE,
      max.words = 80,
      colors = brewer.pal(8, "Dark2"),
      main = "Positive Sentiment Word Cloud",
      scale = c(3, 0.8)
    )
  })
  
  # Rendering the negative sentiment word cloud
  output$wordcloud_n <- renderPlot({
    wordcloud(
      words = df$word[df$sentiment == "negative"],
      random.order = FALSE,
      max.words = 80,
      colors = brewer.pal(8, "Dark2"),
      main = "Negative Sentiment Word Cloud",
      scale = c(3, 1)
    )
  })

# HISTOGRAM
  
  # Rendering the histogram for positive sentiment scores
  output$histogram_pos<- renderPlot({ hist(df_with_sentiment$positive, col='purple',  main="", xlab = "Positive Score") })
  
  # Rendering the histogram for negative sentiment scores
  output$histogram_neg<- renderPlot({ hist(df_with_sentiment$negative, col='blue',  main="", xlab = "Negative Score") })
  
  # Rendering the histogram for overall sentiment scores
  output$histogram_score<- renderPlot({ hist(df_with_sentiment$sentiment_score, col='steelblue', main="", xlab = "Overall Score") })

# PIE CHART 
 
  # Calculating the slices for the pie chart
  slices <- reactive ({ slices <- c(sum(df_with_sentiment$positive), sum(df_with_sentiment$negative)) })
  labels <- c("Positive", "Negative")
  
  # Rendering the pie chart
  output$piechart <- renderPlot({ pie(slices(), labels = labels, col = c('pink', 'steelblue'),explode=0.00, main="Sentiment Analysis") })
  
# BAR PLOT  
  
  # Rendering the bar plot
  output$barplot<- renderPlot({ ggplot(df_with_sentiment, aes(x = Department.Name, y = sentiment_score)) +
      geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
      labs(x = "Department", y = "Average Sentiment Score") })
  
  # Rendering the grouped bar plot
  output$grouped_barplot<- renderPlot({ ggplot(df_with_sentiment, aes(x = Department.Name, fill = Class.Name)) +
      geom_bar(position = "fill") +
      labs(x = "Department", y = "Proportion") })


# SUMMARY STATISTICS
  
  # Calculating number of null values
  null_count <- reactive({
    sum(is.na(df_with_sentiment$sentiment_score))
  })
  
  # Rendering the summary statistics table
  output$summary_stats <- renderTable({
    summary_stats <- summary(df_with_sentiment$sentiment_score)
    data.frame(Measure = names(summary_stats), Value = as.character(summary_stats), stringsAsFactors = FALSE)
  })
  
  # Rendering the count of null values
  output$null_counts <- renderText({
    paste("Null Count:", null_count())
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)























































