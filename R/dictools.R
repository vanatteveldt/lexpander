#' Expand words using wildcard expansion
#' Default uses 'glob' style wildcards ("finan*"), set type='regex' to use
#' regular expressions
#' @param model_or_dfm a fastTextR model or quanteda dfm
#' @param words a character vector of words containing wildcards
#' @return a character vector containing the expanded terms
#' @export
expand_wildcards = function(model_or_dfm, words, type=c("glob","regex")) {
  type = match.arg(type, c("glob","regex"))
  if (type == "glob") words = glob2rx(words)
  pattern = paste0("(?:", words, ")", collapse="|")
  if (class(model_or_dfm) == "dfm") {
    vocabulary = colnames(model_or_dfm)
  }  else {
    vocabulary = fastTextR::ft_words(model_or_dfm)
  }
  stringr::str_subset(vocabulary, pattern)
}

#' Compute k nearest neighbours from multiple seed words
#' (by iterating over seed words and taking k words with max similarity)
#' @param ft_model a fastTextR model
#' @param words a character vector of words
#' @param k_total the number of neighbours to return
#' @param k_per_word the number of neighbours to get per seed word
#' @return ordered list of neighbour words with similarities
#' @export
nearest_neighbors = function(ft_model, words, k=1000, k_per_word=k) {
  nb = purrr::map(words, fastTextR::ft_nearest_neighbors, model=ft_model, k=k_per_word)
  nb = tibble::enframe(unlist(nb))
  nb = dplyr::summarize(dplyr::group_by(nb, name), value=max(value))
  nb = dplyr::arrange(nb, -value)
  nb = head(nb, k)
  setNames(nb$value, nb$name)
}

#' Helper function to compute F_beta score
f = function(prec, rec, beta=1) {
  ifelse(prec+rec==0, 0, (1+beta^2)*prec*rec / (beta^2*prec + rec))
}

#' Evaluate expansion candidates
#' @param candidates an ordered character vector of expansion candidates
#' @param test a character vector of (held-out) test candidates
#' @export
evaluate_candidates = function(candidates, test)  {
  d = tibble::tibble(word=candidates, rank=seq_along(candidates))
  dplyr::mutate(d,
                in_test = as.numeric(word %in% test),
                cum_pos = cumsum(in_test),
                precision = cum_pos/rank,
                recall = cum_pos/length(test),
                f4=f(precision, recall, beta=4),
                cum_pos = NULL)
}

#' Helper function to extract and normalize (ie |row|=1) embeddings
normalized_vectors = function(ft_model, words) {
  vectors = fastTextR::ft_word_vectors(ft_model, words)
  t(apply(vectors, 1, function(x) x/sqrt(sum(x^2))))
}

#' Pairwise similarities between a set of words
#' @param ft_model a FastTextR model
#' @param words a character vector of words
#' @return a long-format tibble containing columns word1, word2, and dist
#' @export
pairwise_similarities = function(ft_model, words) {
  vectors = normalized_vectors(ft_model, words)
  sim = vectors %*% t(vectors)
  sim = tibble::as_tibble(sim, rownames="word1")
  sim = tidyr::pivot_longer(sim, -word1, names_to="word2", values_to="similarity")
  dplyr::filter(sim, word2 > word1)
}

#' Compute the distance of each word to the centroid
#' @param ft_model a FastTextR model
#' @param words a character vector of words
#' @return a long-format tibble containing columns word and dist
#' @export
distance_from_centroid = function(ft_model, words) {
  vectors = normalized_vectors(ft_model, words)
  centroid = colMeans(vectors)
  centroid = centroid / sqrt(sum(centroid^2))
  (vectors %*% centroid)[,1]
}


#' Create a similarity graph
#' @param similarities a data frame of word-word similarities (e.g. output from pairwise_similarity)
#' @param frequences an optional data frame of word-level metadata.
#'                   The first column should be the word names.
#'                   If a 'freq' column is included, it is used for node sizes
#' @return an igraph graph object
#' @export
similarity_graph = function(similarities, word_metadata=NULL, threshold=.5) {
  g = igraph::graph_from_data_frame(sim, directed=F, vertices=word_metadata)
  g = igraph::delete_edges(g, igraph::E(g)[igraph::E(g)$similarity<threshold])
  igraph::E(g)$width = 1 + scales::rescale(igraph::E(g)$similarity)^3*10
  igraph::V(g)$shape = "none"
  if ("freq" %in% names(igraph::vertex.attributes(g))) {
    igraph::V(g)$label.cex = .5 + scales::rescale(log(igraph::V(g)$freq))
  }
  g
}

#' Use word embedding to automatically expand seed set and compute metrics
#'
#' This will split the terms in train and test sets, use `nearest_neighbours`
#' to generate similar words, and test whether the generated words
#' occur in the test set, giving a pseudo-precision and pseudo-recall.
#' Note that both will be lower than actualy precision and recall as we assume
#' the seed words are incomplete (why else expand it), but it can serve as an
#' indication of how many terms to consider.
#'
#' @param ft_model a FastTextR model
#' @param words a character vector of words
#' @param split Faction to use as training data
#' @param vocabulary If given, limit results to words from this vocabulary
#'                   (e.g. only words occurring in the target corpus)
#' @param k Number of candidates to investigate
#' @return A tibble containing terms and (pseudo-)metrics
#' @export
expand_terms = function(ft_model, terms, vocabulary=NULL, split=.5, k=1000)  {
  train = sample(terms, split*length(terms))
  test = setdiff(terms, train)
  candidates = nearest_neighbors(ft_model, train, k)
  candidates = names(candidates)
  if (!is.null(vocabulary)) candidates = intersect(candidates, vocabulary)
  evaluate_candidates(candidates, test)
}
