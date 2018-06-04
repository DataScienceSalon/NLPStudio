#==============================================================================#
#                                   CloneMeta                                  #
#==============================================================================#
#' CloneMeta
#'
#' \code{CloneMeta} Function for copying functional and descriptive meta data between objects.
#'
#' @param from Object from which the metadata will be cloned
#' @param to Object to which the metadata will be cloned
#'
#' @return to Object to which the metadata was cloned
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
cloneMeta = function(from, to) {

  to$setName(name = from$name)

  # Clone descriptive
  descriptive <- from$getMeta(type = 'd')
  if (length(descriptive) > 0){
    keys <- names(descriptive)
    for (i in 1:length(descriptive)) {
      to$setMeta(key = keys[i], value = descriptive[[i]], type = 'd')
    }
  }

  # Clone functional
  functional <- from$getMeta(type = 'f')
  if (length(functional) > 0) {
    keys <- names(functional)
    for (i in 1:length(functional)) {
      to$setMeta(key = keys[i], value = functional[[i]], type = 'f')
    }
  }
  return(to)
}
