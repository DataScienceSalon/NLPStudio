% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tokenizer.R
\docType{class}
\name{Tokenizer}
\alias{Tokenizer}
\title{Tokenizer}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Tokenizer
}
\arguments{
\item{x}{Corpus object}
}
\value{
Tokenizer object
}
\description{
\code{Tokenizer} Wrapper class for the tokenizer package.
}
\details{
Creates character, word, sentence and paragraph tokens and nGrams using the
tokenizer package.
}
\section{Methods}{

 \itemize{
  \item{\code{new()}{Initializes an object of the Tokenizer class.}}
  \item{\code{character(x)}}
  \item{\code{word(x)}}
  \item{\code{sentence(x)}}
  \item{\code{paragraph(x)}}
  \item{\code{nGrams(x, n)}}
 }
}

\seealso{
Other Corpus Tokenizer Family of Classes: \code{\link{Token}}

Other CorpusStudio Family of Classes: \code{\link{CorpusStudio}},
  \code{\link{KFold}}, \code{\link{Sample0}},
  \code{\link{Sample}}, \code{\link{Segment}},
  \code{\link{Split}}, \code{\link{TokenizerNLP}},
  \code{\link{TokenizerQ}}, \code{\link{Token}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
\concept{Corpus Tokenizer Family of Classes}
\concept{CorpusStudio Family of Classes}
\keyword{datasets}
