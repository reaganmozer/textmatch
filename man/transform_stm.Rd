% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_stm.R
\name{transform_stm}
\alias{transform_stm}
\title{Refits a STM with a content-based covariate so that all document-level topic-proportions
are estimated "as-treated". Also allows for calculation of the SR sufficient reduction and optional coarsening
to reduce the dimension of the data, reduce noise, or apply other design rules
(e.g. - to exclude words that occur in too few or too many documents).}
\usage{
transform_stm(mod, out, Z, calc.SR = FALSE, coarsen = FALSE, simplex = FALSE)
}
\arguments{
\item{mod}{a fitted \code{\link[stm]{stm}} object}

\item{out}{the original call to the STM}

\item{Z}{an indicator for treatment assignment}

\item{calc.SR}{an indicator for returning the sufficient reduction. Default is TRUE.}

\item{coarsen}{an indicator for returning the coarsened STM}
}
\value{
A bounded DFM
}
\description{
Refits a STM with a content-based covariate so that all document-level topic-proportions
are estimated "as-treated". Also allows for calculation of the SR sufficient reduction and optional coarsening
to reduce the dimension of the data, reduce noise, or apply other design rules
(e.g. - to exclude words that occur in too few or too many documents).
}
