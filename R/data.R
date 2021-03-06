#' High Value Terms
#'
#' Dataset containing the raw words, bigrams, and trigrams that
#' were determined to be statistically significant in prior research.
#'
#' @format A data frame with 189 rows and 4 variables:
#' \describe{
#'   \item{token}{actual string of high value term}
#'   \item{freqYES}{Frequency of term in development set for opioid related events}
#'   \item{freqNO}{Frequency of term in development set for non-opioid related events}
#'   \item{type}{word, bigram, or trigram}
#' }
"high_value_terms"

#' Stop Words
#'
#' Exact copy of tidytext::stop_words
#'
#' @format A data frame with 1149 rows and 2 variables:
#' \describe{
#'   \item{word}{actual token}
#'   \item{lexicon}{lexicon the stop word comes from}
#' }
"stop_words"
