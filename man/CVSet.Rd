% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CVSet.R
\docType{class}
\name{CVSet}
\alias{CVSet}
\title{CVSet}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
CVSet
}
\arguments{
\item{name}{Character string containing the name for the CVSet object.}

\item{purpose}{Character string used to indicate how the document will be used, e.g. 'train', 'test'.}

\item{note}{Character string containing a comment associated with a call to the
text method. The texts of the note variable are written to the CVSets
log. This is used to track changes to the text, perhaps made during preprocessing.}
}
\value{
CVSet object, containing the CVSet text, the metadata and
the methods to manage both.
}
\description{
\code{CVSet} Class containing training, test and an optional validation corpora objects.
}
\details{
Class conitains a cross-validation set of corpora objects. CVSet objects are
collections including a training, test, and an optional validation Corpus objects.
}
\section{Core Methods}{

 \itemize{
  \item{\code{new(name = NULL)}}{Initializes an object of the CVSet class.}
  \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
  parameters are presented, the current text is returned.  Otherwise, the text
  is updated with the texts of the character vector 'x'. Sentence, word, token, type,
  sentence and word length statistics are also computed and the metadata is updated
  accordingly.}
  \item{\code{summary()}}{Summarizes the CVSet object.}
 }
}

\seealso{
Other CVSet Classes: \code{\link{CVSetKFold}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
\concept{CVSet Classes}
\keyword{datasets}
