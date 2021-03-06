% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictools.R
\name{distance_from_centroid}
\alias{distance_from_centroid}
\title{Compute the distance of each word to the centroid}
\usage{
distance_from_centroid(ft_model, words)
}
\arguments{
\item{ft_model}{a FastTextR model}

\item{words}{a character vector of words}
}
\value{
a long-format tibble containing columns word and dist
}
\description{
Compute the distance of each word to the centroid
}
