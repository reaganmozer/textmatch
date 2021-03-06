% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment_with_LIWC.R
\name{augment_with_LIWC}
\alias{augment_with_LIWC}
\title{Augment a text data set with features generated by Linguistic Inquiry Word Count (LIWC) software.}
\usage{
augment_with_LIWC(file.path, meta = NULL)
}
\arguments{
\item{file.path}{path to the LIWC output file}

\item{meta}{an optional data.frame containing document metadata.}
}
\value{
a \code{data.frame} containing LIWC-generated features for a set of text documents.
}
\description{
Augment a text data set with features generated by Linguistic Inquiry Word Count (LIWC) software.
}
