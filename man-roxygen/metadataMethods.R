#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this base class.}
#'   \item{\code{getIdentity(key = NULL)}}{Returns identity metadata such as
#'   family, class, identifier, and name of an object. If no key is
#'   designated, all identity variables are returned.}
#'   \item{\code{setIdentity(x, name = NULL, purpose = NULL)}}{Sets the identify
#'   of an object. The identity variables are family, class, name, and an
#'   identifier. The family is the object's parent class and the identifier
#'    is randomly generated and assigned to each object.}
#'   \item{\code{checkIdentity(id = NULL, name = NULL, class = NULL,
#'   family = NULL, purpose = NULL)}}{A logical check to determine if an
#'   object matches the query parameters.}
#'   \item{\code{getStats(key = NULL)}}{Obtains descriptive statistics data
#'   assigned to the object.}
#'   \item{\code{setStats(key, value)}}{Sets descriptive statistics data
#'   for an object.}
#'   \item{\code{getCustom(key = NULL)}}{Obtains custom metadata which has been
#'   assigned to the object.}
#'   \item{\code{setCustom(key, value)}}{Sets custom metadata using key value
#'   pairs.}
#'   \item{\code{getState()}}{Obtains an objects current state information.}
#'   \item{\code{modified(event = NULL)}}{Sets state datetime parameters
#'   when an object is modified.}
#'   \item{\code{summary()}}{Method for summarizing an objects metadata.}
#'  }
