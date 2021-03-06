% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_forest_helpers.R
\name{partial_plot_predict}
\alias{partial_plot_predict}
\title{Function to calculate partial dependencies from a \code{randomForest} model.}
\usage{
partial_plot_predict(list_model, df, variable, n = 100,
  as.character = FALSE)
}
\arguments{
\item{list_model}{A \code{randomForest} model.}

\item{df}{Data frame to be used for prediction.}

\item{variable}{The variable to calculate the partial dependency of.}

\item{n}{Number of observations to predict?}

\item{as.character}{Should the variables be forced to be a character data 
type? This is helpful when binding many returns from 
\code{partial_plot_predict}.}
}
\value{
Data frame.
}
\description{
Function to calculate partial dependencies from a \code{randomForest} model.
}
\author{
Stuart K. Grange
}
