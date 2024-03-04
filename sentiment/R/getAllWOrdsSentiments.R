#' A function that adds sentiment columns to df
#'
#' The function below calculates the sentiment scores of texts and returns a dataframe with
#' collected sentiments added to existing dataframe
#'
#' @author Zuzanna Miazio Karolina Solarska
#' @param column a text column of dataframe df
#' @return a dataframe with calculated sentiment is returned- the positive, negative
#' @import dplyr tidyverse tidytext stringr
#' @examples getAllWOrdsSentiments(c("I love strawberries", "I hate chocolate"))
#' @export



getAllWOrdsSentiments<- function(column){
  all_words <- data.frame()
  for (i in 1:length(column)){
    tokens <- tidytext::unnest_tokens(data.frame(text = column), word, text)
    sentiment <- merge(tokens, tidytext::get_sentiments("bing"), by = "word")
    all_words<- rbind(all_words, sentiment)
  }

  return(sentiment)
}

#' @importFrom graphics text
NULL





