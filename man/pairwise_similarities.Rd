% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictools.R
\name{pairwise_similarities}
\alias{pairwise_similarities}
\title{Pairwise similarities between a set of words}
\usage{
pairwise_similarities(ft_model, words)
}
\arguments{
\item{ft_model}{a FastTextR model}

\item{words}{a character vector of words}
}
\value{
a long-format tibble containing columns word1, word2, and dist
}
\description{
Pairwise similarities between a set of words
}
