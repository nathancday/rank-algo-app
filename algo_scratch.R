# g <- 4
# g_max <- 5
# 
# ((2^g) - 1 ) / (2^g_max)
# docs <- c(.99, .8, .5, .25) # relevancy scores for docs
docs <- c(4,3,4,2,3,2,3,4,3,2,1)
docs_binary = c(0,1,0,1,1,0,0,1)


# Precision ---------------------------------------------------------------

#' requies incoming `docs` are scaled to binary (1 = relevant)
#' @param r The rank of a document in results set
#' @param docs The relevancy scores of the doc set
precision <- function(r, docs) {
  doc_subset <- docs[1:r]
  
  sum(doc_subset) / length(doc_subset)
}

precision(4, docs_binary)
precision(2, docs_binary)
precision(1, docs_binary)

# Recall ---------------------------------------------------------------

#' requires incoming `docs` are scaled to binary (1 = relevant)
#' @param r The rank of a document in results set
#' @param docs The relevancy scores of the doc set
recall <- function(r, docs) {
  total_rel <- sum(docs)
  doc_subset <- docs[1:r]
  
  sum(doc_subset) / total_rel
}

recall(4, docs_binary)
recall(1, docs_binary)


# Average Precision -------------------------------------------------------

avg_precision <- function(r, docs) {
  doc_subset <- docs[1:r]
  
  vals_to_avg <- vector("numeric")
  count = 0
    
  for (i in seq_along(doc_subset)) {
    if (doc_subset[i] > 0) {
      count = count + 1
      vals_to_avg <- c(vals_to_avg, precision(i, doc_subset))
    }
  }
  sum(vals_to_avg) / count
}

avg_precision(2, docs_binary)
avg_precision(3, docs_binary)
avg_precision(4, docs_binary)
avg_precision(5, docs_binary)
avg_precision(7, docs_binary)


grade_to_prob <- function(score, max) ((2^score) - 1) / (2^max)
grade_to_prob(3, 4)



# ERR -------
# from Chapelle
# split into multiple functions

gain <- function(doc, max_grade = 4) {
  ((2^doc) - 1) / (2^max_grade)
}

satisfied_at <- function(r, docs, max_grade = 4) {
  rels <- numeric(length(r))
  before_w <- 1
  for (p in 1:r) {
    pos_gain <- gain(docs[p], max_grade)
    rels[p] <- before_w * pos_gain
    before_w <- 1- pos_gain
  }
  prod(rels)
}

satisfied_at(1, docs)
satisfied_at(3, docs)

map_dbl(1:10, ~satisfied_at(., docs))


# this could be any function `f(rank)` that goes from 1 >>> 0 as rank >>> Inf
utility <- function(r) {
  1 / r
}

utility(2)

err <- function(r, docs, max_grade = 4) {
  to_sum <- numeric(r)
  for (i in 1:r) {
    sat_at <- satisfied_at(i, docs, max_grade)
    to_sum[i] <- utility(i) * sat_at
  }
  sum(to_sum)
}


err(1, docs)
err(2, docs)
err(3, docs)
err(4, docs)
err(5, docs)

err(2, rev(docs))

# Cumulative Gain ---------------------------------------------------------

cg <- function(r, docs) {
  doc_subset <- docs[1:r]
  
  sum(doc_subset)
}

cg(4, docs)
cg(2, docs)


# Discount cumulative gain ---------------------------------------------------------
# goal: reward systems that put highly ranked docs first
# useful as an evaluation metric for different boosting strategies
# two formulas on wikipedia: https://en.wikipedia.org/wiki/Discounted_cumulative_gain#Discounted_Cumulative_Gain

dcg <- function(r, docs) {
  
  doc_subset <- docs[1:r]
  
  scores_to_sum <- vector("numeric", length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    
    scores_to_sum[i] <- docs[i] / log(i + 1, base = 2)
    
  }
  sum(scores_to_sum)
}

dcg2 <- function(r, docs) {
  
  doc_subset <- docs[1:r]
  
  scores_to_sum <- vector("numeric", length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    
    scores_to_sum[i] <- (2^docs[i] - 1) / (log(i + 1, base = 2))
    
  }
  sum(scores_to_sum)
}

dcg2(2, docs)

bad_docs <- rev(docs)

dcg2(2, bad_docs)

# Normalized DCG ---------------------------------------------------------
# goal: Pull DCG back into 0-1 range
# this implements localized DCG (highest rank in results returned)

ndcg <- function(r, docs) {
  
  real <- dcg(r, docs)
  ideal <- dcg(r, sort(docs, decreasing = TRUE))
  
  real / ideal
  
}

ndcg(2, docs)

bad_docs <- rev(docs)

ndcg(3, bad_docs)


# Rank based precision ---------------------------------------------------------
# goal: balace utility with effort required

#' @param p A parameter describing how persistant as user is.
#' @details 
#' * p = .95 ~ 60% chance of reaching the second page (.95^10)
#' * p = .5 ~ 0.1% chance of reaching the second page (.5^10)
#' * p = 0 ~ I'm feeling lucky
#'
rbp <- function(r, docs, p) {
  doc_subset <- docs[1:r]
  
  scores_to_sum <- vector("numeric", length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    
    scores_to_sum[i] <- docs[i] * p^(i-1)
    
  }
  (1- p) * sum(scores_to_sum)
}
rbp(2, docs, .5)
