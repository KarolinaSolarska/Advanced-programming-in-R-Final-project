
#' A function that calculates sentiment based on a dataframe column
#'
#' The function below calculates the sentiment scores of texts and returns a dataframw eith collected sentiments
#'
#' @name GetSentimentAll
#' @author Zuzanna Miazio Karolina Solarska
#' @param file text column of a dataframe
#' @return a dataframe with calculated sentiment is returned- the positive, negative and overall score
#' @import tidytext stringr dplyr utils tidyverse
#' @examples
#' GetSentiment("I like ice cream, I hate chocolate")
#' @export

GetSentiment <- function(file) {
  sentiment<- GetPositiveSentiment(file) - GetNegativeSentiment(file)
  return(sentiment)
}

#' @importFrom graphics text
NULL
