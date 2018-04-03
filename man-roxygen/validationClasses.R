#' @section Validation Class Family Overview:
#'
#' The following classes comprise the validation apparatus in this package.
#' \describe{
#'  \item{Validate}{Class embedded by composition into all classes in which parameters must be validated.
#'  This class contains methods for each function requiring validation. Each method is responsible for
#'  dispatching the appropriate validation method on the Validator class. }
#'  \item{Validator}{Class is reponsible for dispatching the appropriate concrete validation visitor
#'  class to the object requiring validation.}
#'  \item{VVInit}{Class validates the parameters of the initialization methods.}
#'  \item{VVRead}{Class responsible for validation read requests.}
#'  \item{VVSource}{Class responsible for Corpus sourcing methods. This includes methods for
#'  validating vector, file, and directory sources. It also includes methods that validate
#'  corpus objects and packages such as \code{\link[quanteda]{corpus}} and \code{\link[tm]{VCorpus}
#'  \code{\link[tm]{SimpleCorpus} and \code{\link[tm]{PCorpus}.
#' }
#'
#' Validation is implemented using the Visitor design pattern, one of the
#' twenty-three GoF design patterns described in Design Patterns:
#' Elements of Reusable Object-Oriented Software by Gamma,
#' Helm, Johnson & Vissides.
#' Gamma, E. (1995). Design patterns: Elements of reusable object-oriented software. Reading, Mass: Addison-Wesley.
