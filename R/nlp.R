#' @title NLP Module
#' @description Text classification (Naive Bayes spam detector) and sentiment
#'   analysis with Bing lexicon. Includes full input validation and structured
#'   error handling.
#' @name nlp
#' @importFrom stats predict
NULL

#' Build a Naive Bayes spam detector
#'
#' Converts raw text into a document-term matrix, applies frequency filtering,
#' and trains a Naive Bayes model via \pkg{e1071}. Returns a self-contained
#' detector object that can be passed to \code{\link{predict_spam}}.
#'
#' @param texts Character vector of message texts.
#' @param labels Factor vector of labels (e.g. \code{"spam"} / \code{"ham"}).
#' @param train_fraction Proportion of data for training. Default \code{0.75}.
#' @param min_term_freq Minimum term frequency to keep a feature. Default \code{7}.
#' @param laplace Laplace smoothing parameter. Default \code{1}.
#' @param seed Random seed.
#' @return A list:
#'   \describe{
#'     \item{model}{Trained Naive Bayes model.}
#'     \item{dtm}{The DocumentTermMatrix.}
#'     \item{freq_terms}{Character vector of retained terms.}
#'     \item{train_size}{Number of training documents.}
#'     \item{labels}{Shuffled label vector (for later evaluation).}
#'   }
#' @export
build_spam_detector <- function(texts, labels,
                                train_fraction = 0.75,
                                min_term_freq = 7L,
                                laplace = 1,
                                seed = 125L) {
  stopifnot(
    "texts must be a character vector" = is.character(texts),
    "labels must be a factor"          = is.factor(labels),
    "texts and labels must have equal length" = length(texts) == length(labels),
    "train_fraction must be in (0,1)" = train_fraction > 0 && train_fraction < 1,
    "min_term_freq must be >= 1"      = min_term_freq >= 1L,
    "need at least 10 documents"      = length(texts) >= 10L
  )

  set.seed(seed)
  n <- length(texts)
  idx <- sample(seq_len(n))
  texts  <- texts[idx]
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
  if (length(freq_terms) == 0L) {
    stop("No terms meet the minimum frequency threshold of ", min_term_freq,
         ". Try lowering min_term_freq.", call. = FALSE)
  }

  n_train <- round(n * train_fraction)
  train_mat <- as.matrix(dtm[seq_len(n_train), freq_terms])
  train_mat <- apply(train_mat, 2, function(x) ifelse(x > 0, "Yes", "No"))

  model <- e1071::naiveBayes(train_mat, labels[seq_len(n_train)], laplace = laplace)

  list(
    model = model,
    dtm = dtm,
    freq_terms = freq_terms,
    train_size = n_train,
    labels = labels
  )
}

#' Predict spam / ham labels with a trained detector
#'
#' @param detector Output from \code{\link{build_spam_detector}}.
#' @param newdtm A \code{DocumentTermMatrix} of new documents, or \code{NULL}
#'   to use the held-out test portion of the original DTM.
#' @return A factor vector of predictions.
#' @export
predict_spam <- function(detector, newdtm = NULL) {
  stopifnot(
    "detector must be a list from build_spam_detector" =
      is.list(detector) && all(c("model", "dtm", "freq_terms", "train_size") %in% names(detector))
  )

  if (is.null(newdtm)) {
    n <- nrow(detector$dtm)
    if (detector$train_size >= n) {
      stop("No held-out test data. Provide newdtm explicitly.", call. = FALSE)
    }
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
#' per-document sentiment scores. Requires \pkg{tidytext}.
#'
#' @param texts Character vector where each element is one document.
#' @return A tibble with columns \code{doc_id}, \code{positive},
#'   \code{negative}, \code{sentiment}.
#' @export
#' @examples
#' \dontrun{
#' analyze_sentiment(c("I love sunshine", "Rain is gloomy"))
#' }
analyze_sentiment <- function(texts) {

  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("Package 'tidytext' is required. Install with: install.packages('tidytext')",
         call. = FALSE)
  }
  stopifnot(
    "texts must be a character vector" = is.character(texts),
    "texts must not be empty"          = length(texts) >= 1L
  )

  text_df <- tibble::tibble(doc_id = seq_along(texts), text = texts)

  tokens <- text_df %>%
    tidytext::unnest_tokens(word, text)

  bing <- tidytext::get_sentiments("bing")

  sentiment <- tokens %>%
    dplyr::inner_join(bing, by = "word") %>%
    dplyr::count(doc_id, sentiment) %>%
    tidyr::spread(sentiment, n, fill = 0)

  if (!"positive" %in% names(sentiment)) sentiment$positive <- 0L
  if (!"negative" %in% names(sentiment)) sentiment$negative <- 0L

  sentiment$sentiment <- sentiment$positive - sentiment$negative
  sentiment
}
