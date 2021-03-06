% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{methods}
\name{plot.BigVAR}
\alias{plot,BigVAR,ANY-method}
\alias{plot.BigVAR}
\title{Plot a BigVAR object}
\usage{
\S4method{plot}{BigVAR,ANY}(x, y = NULL, ...)
}
\arguments{
\item{x}{BigVAR object created from \code{ConstructModel}}

\item{y}{needed to mantain compatibility with generic, otherwise ignored}

\item{...}{additional arguments}
}
\value{
NA, side effect is graph
}
\description{
Plot a BigVAR object
}
\details{
Uses plot.zoo to plot each individual series of \code{Y} on a single plot
}
\seealso{
\code{\link{constructModel}}
}

