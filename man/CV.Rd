% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CV.R
\docType{class}
\name{CV}
\alias{CV}
\title{CV}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
CV
}
\arguments{
\item{name}{Character string containing the name for the CV object.}

\item{purpose}{Character string used to indicate how the document will be used, e.g. 'train', 'test'.}

\item{note}{Character string containing a comment associated with a call to the
text method. The texts of the note variable are written to the CVs
log. This is used to track changes to the text, perhaps made during preprocessing.}
}
\value{
CV object, containing the CV text, the metadata and
the methods to manage both.
}
\description{
\code{CV} Class containing training, test and an optional validation corpora objects.
}
\details{
Class conitains a cross-validation set of corpora objects. CV objects are
collections including a training, test, and an optional validation Corpus objects.
}
\section{Core Methods}{

 \itemize{
  \item{\code{new(name = NULL)}}{Initializes an object of the CV class.}
  \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
  parameters are presented, the current text is returned.  Otherwise, the text
  is updated with the texts of the character vector 'x'. Sentence, word, token, type,
  sentence and word length statistics are also computed and the metadata is updated
  accordingly.}
  \item{\code{summary()}}{Summarizes the CV object.}
 }
}

\author{
John James, \email{jjames@datasciencesalon.org}
}
\concept{CV Classes}
\keyword{datasets}
