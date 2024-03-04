#' A function that adds sentiment columns to df
#'
#' The function below calculates the sentiment scores of texts and returns a dataframe with
#' collected sentiments added to existing dataframe
#'
#' @author Zuzanna Miazio Karolina Solarska
#' @param df a dataframe
#' @param column a text column of dataframe df
#' @return a dataframe with calculated sentiment is returned- the positive, negative and overall score
#' @import dplyr tidyverse tidytext stringr
#' @examples addSentimentToDf(data.frame(Review.Text = c("I love strawberries", "I hate chocolate")))
#' @export

addSentimentToDf <- function(df, column = NULL) {
  if (!is.data.frame(df)) {
    return("1st argument must be of format data.frame")
  }

  if (is.null(column)) {
    if (ncol(df) != 1) {
      return("The data.frame must have exactly one column if 'column' argument is not specified")
    }
    column <- colnames(df)[1]
  } else {
    if (!is.character(column) || !(column %in% colnames(df))) {
      return("2nd argument must be a character vector specifying a valid column in the data.frame")
    }
  }

  n <- nrow(df)

  df$positive <- rep(NA, n)
  df$negative <- rep(NA, n)
  df$sentiment_score <- rep(NA, n)
  for (i in 1:n) {
    tryCatch({
      df$sentiment_score <- GetSentiment(df[[column]][i])
      df$positive[i] <- GetPositiveSentiment(df[[column]][i])
      df$negative[i] <- GetNegativeSentiment(df[[column]][i])
    }, error = function(e) {
      df$positive[i] <- NA
      df$negative[i] <- NA
      df$sentiment_score[i] <- NA
    })
  }
  return(df)
}
