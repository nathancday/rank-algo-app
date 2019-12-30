# g <- 4
# g_max <- 5
# 
# ((2^g) - 1 ) / (2^g_max)
docs <- c(.99, .8, .5, .25) # relevancy scores for docs


# Precision ---------------------------------------------------------------

#' requies incoming `docs` are scaled to binary (1 = relevant)
#' @param r The rank of a document in results set
#' @param docs The relevancy scores of the doc set
precision <- function(r, docs) {
  doc_subset <- docs[1:r]
  
  sum(doc_subset) / length(doc_subset)
}

precision(4, docs > .5)
precision(2, docs > .5)


# satisfied -------

#' @param r The rank of a document in results set
#' @param docs The relevancy scores of the doc set
prob_satisfied <- function(r, docs) {
  
  doc_subset <- docs[1:r]
  
  scores_to_prod <- vector("numeric", length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    if (i == 1) {
      scores_to_prod[i] <- doc_subset[i]
    } else {
      scores_to_prod[i] <- (1-docs[i-1]) * docs[i]
    }
  }
  
  prod(scores_to_prod)
}

prob_satisfied(3, docs)

# utility ---------
# this could be any function `f(rank)` that goes from 1 >>> 0 as rank >>> Inf

utility <- function(r) 1 / r # from Chapelle

utility(2)

# Expected reciprocal rank ------------------------------------------------------

err <- function(r, docs) {
  
  doc_subset <- docs[1:r]
  
  scores_to_sum <- vector("numeric", length = length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    scores_to_sum[i] <- utility(i) * prob_satisfied(i, doc_subset[1:i])
  }
  
  sum(scores_to_sum)
  
}

err(4, docs)


# Cumulative Gain ---------------------------------------------------------

cg <- function(r, docs) {
  doc_subset <- docs[1:r]
  
  sum(doc_subset)
}

cg(4, docs)
cg(2, docs)


# Discount cumulative gain ---------------------------------------------------------
# goal: reward systems that put highly ranked docs first
# useful as an evaluation metric for differnent boosting strategies

dcg <- function(r, docs) {
  
  doc_subset <- docs[1:r]
  
  scores_to_sum <- vector("numeric", length(doc_subset))
  
  for (i in seq_along(doc_subset)) {
    
    scores_to_sum[i] <- (2^docs[i] - 1) / (log(i + 1))
    
  }
  sum(scores_to_sum)
}

dcg(2, docs)

bad_docs <- rev(docs)

dcg(2, bad_docs)


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
