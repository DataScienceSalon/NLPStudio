#' segment
#'
#' \code{segment} Splits a Corpus object into n segments of sizes indicated by a
#' weighting parameter.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the segment class.}
#'   \item{\code{execute(corpus, n=2, weights = c(0.75, 0.25) stratify = TRUE,
#'   seed = NULL)}}{Executes the corpus segmentation}
#'  }
#'
#' @param corpus Corpus object.
#' @param n Numeric indicator of the number of segments.
#' @param weights Numeric vector indicating the weights for each segment. If
#' NULL, all segments will have equal weight.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @return List of Corpus objects
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
segment <- function(corpus, n = 3, weights = NULL, stratify = TRUE,
                    seed = NULL) {

  splitDocument = function(document, n, weights, seed) {

    # Set weights
    if (is.null(weights)) weights <- rep(1/n, n)

    content <- unlist(document$content)

    # Set seed
    if (length(seed) > 0)  set.seed(seed)

    # Create indices and splits
    idx <- sample(n, size = length(content), replace = TRUE,
                  prob = weights)

    splits <- list()
    for (i in 1:n) {
      splits[[i]] <- Clone$new()$this(x = document, reference = FALSE)
      splits[[i]]$content <- content[idx == i]
    }

    return(splits)
  }

  validate = function(corpus, n, weights, stratify) {
    if (!(class(corpus)[1] == 'Corpus')) {
      stop("Invalid Corpus class object.")
    }
    if (!(class(n)[1] %in% c('numeric', 'integer'))) {
      stop("Invalid 'n' parameter.  Must be numeric.")
    }
    if (!is.null(weights)) {
      if (!(class(weights)[1] == 'numeric')) {
        stop("Invalid 'weights' parameter.  Must be numeric vector of length n.")
      }
    }
    if (!(class(stratify)[1] == 'logical')) {
      stop("Invalid 'stratify' parameter.  Must be logical.")
    }

    if (!is.null(weights)) {
      if (sum(weights) != 1) {
        stop("Weights must equal to one.")
      }
      if (length(weights) != n) {
        stop("Weights vector must be of length n.")
      }
    }
  }

  sumQuant = function(corpus) {
    # Update quantitative metadata
    quant <- corpus$getDocMeta(type = 'q')[[1]]
    if (nrow(quant) > 0) {
      keys <- c(names(quant), 'documents')
      values <- c(colSums(quant), nrow(quant))
      corpus$setMeta(key = keys, value = values, type = 'q')
    }
    return(corpus)
  }

  getDocuments = function(corpus, stratify) {
    docs <- corpus$getDocuments()
    # Create single Document object if stratify is FALSE
    if (stratify == FALSE) {
      content <- unlist(lapply(docs, function(d) {
        d$content
      }))
      docs <- list(Document$new(x = content, name = paste(corpus$getName(), "Document")))
    }
    return(docs)
  }

  # Perform validation and obtain documents.
  validate(corpus, n, weights, stratify)
  documents <- getDocuments(corpus, stratify)

  # segment Documents
  docSegments <- lapply(documents, function(d) {
    splitDocument(d, n, weights, seed)
  })

  # Combine Document objects into Corpus objects
  name <- corpus$getName()
  corpora <- list()
  for (i in 1:n) {
    corpora[[i]] <- Clone$new()$this(corpus, reference = FALSE)
    for (j in 1:length(docSegments)) {
      corpora[[i]]$addDocument(docSegments[[j]][[i]])
    }
    corpora[[i]] <- sumQuant(corpora[[i]])
  }

  return(corpora)
}
