#' CVFactory0
#'
#' \code{CVFactory0} Abstract factory class for the family of  cross validation factory classes.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract class.}
#'   \item{\code{build()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param x Corpus object for which the CVSetHoldOut object is to be created.
#' @param unit Character string indicating whether the unit for random sampling is
#' the vector or sentence. If the latter, the documents will be tokenized into sentences
#' prior to sampling.
#' @param stratify Logical. If TRUE, sampling will be conducted at the individual
#' document level. Otherwise, the Document objects will be combined into a single
#' Document object prior to sampling.
#' @param proportions Numeric vector of length 2 or 3, indicating the proportions to allocate
#' to the training (validation) and test sets.  If the length of the vector is 2, a
#' training and test set will be created in accordance with the proportions indicated. If
#' the length is 3, a validation set will be created in addition to the training and
#' test sets.
#' @param k The number of K-Fold validation sets to create.
#' @param seed Integer used to set the seed for random sampling
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVFactory0 <- R6::R6Class(
  classname = "CVFactory0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {stop("Method not implemented for this abstract class")},
    build = function() {stop("Method not implemented for this abstract class")}
  )
)
