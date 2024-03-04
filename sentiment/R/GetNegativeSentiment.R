
#' A function that calculates sentiment based on a dataframe column
#'
#' The function below calculates the sentiment scores of texts and returns a dataframw eith collected sentiments
#'
#' @name GetSentimentNeg
#' @author Zuzanna Miazio Karolina Solarska
#' @param file text column of a dataframe
#' @return a dataframe with calculated sentiment is returned- the positive, negative and overall score
#' @import tidytext stringr dplyr utils tidyverse
#' @examples
#' GetPositiveSentiment("I like ice cream, I hate chocolate")
#' @export

GetNegativeSentiment <- function(file) {
  if (!is.character(file)) {
    return("Input must be a character vector or a column of a data frame.")
  }

  file <- gsub("\\$", "", file)
  tokens <- tidytext::unnest_tokens(data.frame(text = file), word, text)

  sentiment <- merge(tokens, tidytext::get_sentiments("bing"), by = "word")
  negative_sentiment <- sum(sentiment$sentiment == "negative")

  return(negative_sentiment)
}
#' @importFrom graphics text
NULL
