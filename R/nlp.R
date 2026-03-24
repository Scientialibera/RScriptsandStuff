#' @title NLP Module
#' @description Spam detection (Naive Bayes) and sentiment analysis utilities.
#' @name nlp
NULL

#' Build a Naive Bayes spam detector
#'
#' Converts text into a document-term matrix, applies frequency filtering, and
#' trains a Naive Bayes model via \pkg{e1071}.
#'
#' @param texts Character vector of message texts.
#' @param labels Factor vector of labels (e.g. "spam" / "ham").
#' @param train_fraction Proportion of data for training. Default `0.75`.
#' @param min_term_freq Minimum term frequency to keep a feature. Default `7`.
#' @param laplace Laplace smoothing parameter. Default `1`.
#' @param seed Random seed.
#' @return A list with components:
#'   \item{model}{Trained Naive Bayes model.}
#'   \item{dtm}{The DocumentTermMatrix.}
#'   \item{freq_terms}{Character vector of retained terms.}
#'   \item{train_size}{Number of training documents.}
#' @export
build_spam_detector <- function(texts, labels,
                                train_fraction = 0.75,
                                min_term_freq = 7L,
                                laplace = 1,
                                seed = 125L) {
  set.seed(seed)
  n <- length(texts)
  idx <- sample(seq_len(n))
  texts <- texts[idx]
  labels <- labels[idx]

  corpus <- tm::VCorpus(tm::VectorSource(texts))
  dtm <- tm::DocumentTermMatrix(corpus, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    removePunctuation = TRUE,
    stemming = TRUE
  ))

  freq_terms <- tm::findFreqTerms(dtm, lowfreq = min_term_freq)

  n_train <- round(n * train_fraction)
  train_mat <- as.matrix(dtm[1:n_train, freq_terms])
  train_mat <- apply(train_mat, 2, function(x) ifelse(x > 0, "Yes", "No"))

  model <- e1071::naiveBayes(train_mat, labels[1:n_train], laplace = laplace)

  list(
    model = model,
    dtm = dtm,
    freq_terms = freq_terms,
    train_size = n_train
  )
}

#' Predict spam/ham labels with a trained detector
#'
#' @param detector Output from \code{build_spam_detector}.
#' @param newdtm A DocumentTermMatrix of new documents, or NULL to use the
#'   held-out test portion of the original DTM.
#' @return A factor vector of predictions.
#' @export
predict_spam <- function(detector, newdtm = NULL) {
  if (is.null(newdtm)) {
    n <- nrow(detector$dtm)
    test_idx <- (detector$train_size + 1):n
    test_mat <- as.matrix(detector$dtm[test_idx, detector$freq_terms])
  } else {
    test_mat <- as.matrix(newdtm[, detector$freq_terms])
  }
  test_mat <- apply(test_mat, 2, function(x) ifelse(x > 0, "Yes", "No"))
  predict(detector$model, test_mat)
}

#' Perform Bing sentiment analysis on a character vector of text
#'
#' Tokenises text, joins with the Bing sentiment lexicon, and returns
#' per-document sentiment scores.
#'
#' Requires the \pkg{tidytext} package.
#'
#' @param texts Character vector where each element is one document/paragraph.
#' @return A tibble with columns `doc_id`, `positive`, `negative`, `sentiment`.
#' @export
analyze_sentiment <- function(texts) {
  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("Package 'tidytext' is required. Install with: install.packages('tidytext')")
  }

  text_df <- tibble::tibble(doc_id = seq_along(texts), text = texts)

  tokens <- text_df %>%
    tidytext::unnest_tokens(word, text)

  bing <- tidytext::get_sentiments("bing")

  sentiment <- tokens %>%
    dplyr::inner_join(bing, by = "word") %>%
    dplyr::count(doc_id, sentiment) %>%
    tidyr::spread(sentiment, n, fill = 0) %>%
    dplyr::mutate(sentiment = positive - negative)

  sentiment
}
