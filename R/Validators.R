#------------------------------------------------------------------------------#
#                              Validate Class                                  #
#------------------------------------------------------------------------------#
#' validateClass
#'
#' \code{validateClass} Validates the class of one or more parameters.
#'
#' Validates the class of one or more objects. The object must
#' have a public member called params which is a list containing three
#' elements: (1) a vector of object names, (2) a vector containing
#' the objects, and (3) a list of vectors, where each vector
#' contains the valid classes for each of the objects
#'
#' @usage validateClass(object)
#'
#' @param object The object to be validated
#'
#' @return a list containing two elements: a code and a message. If the
#' validation passed, the code will be TRUE and the msg element will be NULL.
#' Otherwise, the code will be FALSE and the msg element will describe
#' the error.
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateClass <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$classes$objects) > 0) {
    for (i in 1:length(params$classes$objects)) {
      if (sum(class(params$classes$objects[[i]]) %in% params$classes$valid[[i]]) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Object, ", params$classes$names[i],
                                  ", of the ", class(params$classes$objects[i])[1],
                                  " is of an invalid class. ",
                                  class(object)[1], " class operates on ",
                                  paste0("c(",gsub(",$", "",
                                                   paste0("'", params$classes$valid[[i]],
                                                          "',", collapse = "")),
                                         ") "),
                                  " classes only. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
    }
  }
  return(status)
}

#------------------------------------------------------------------------------#
#                       Validate Discrete Parameters                           #
#------------------------------------------------------------------------------#
#' validateDiscrete
#'
#' \code{validateDiscrete} Validates a parameters that take discrete values.
#'
#' Class validates objects which take discrete parameters. The object must
#' have a public member called params which is a list containing three
#' elements: (1) a vector of variable names, (2) a vector containing
#' the values of the variables, and (3) a list of vectors, where each vector
#' contains the valid values for each of the variables.
#'
#' @usage validateDiscrete(object)
#'
#' @param object The object to be validated
#'
#' @return a list containing two elements: a code and a message. If the
#' validation passed, the code will be TRUE and the msg element will be NULL.
#' Otherwise, the code will be FALSE and the msg element will describe
#' the error.
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateDiscrete <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$discrete$values) > 0) {
    for (i in 1:length(params$discrete$values)) {
      if (!(params$discrete$values[[i]] %in% params$discrete$valid[[i]])) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid '", params$discrete$variables[i], "' parameter. ",
                                  "Valid values are ",
                                  paste0("c(",gsub(",$", "",
                                                   paste0("'",params$discrete$valid[[i]],
                                                          "',", collapse = "")),
                                         "). "),
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
    }
  }
  return(status)
}

#------------------------------------------------------------------------------#
#                             Validate Logical                                 #
#------------------------------------------------------------------------------#
#' validateLogical
#'
#' \code{validateLogical} Validates a parameters that take logical values.
#'
#' Class accepts an object with a public parameters member. The parameters
#' member must be a list with two elements: (1) a vector of variable
#' names, and (2) a vector containing the values.
#'
#' @usage validateLogical(object)
#'
#' Class accpe
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateLogical <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$logicals$values) > 0) {
    for (i in 1:length(params$logicals$values)) {
      if (!(params$logicals$values[i] %in% c('TRUE', 'FALSE', 1, 0))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("TRUE/FALSE expected for the '",
                                  params$logicals$variables[i],  "' variable. ",
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
    }
  }
  return(status)
}

#------------------------------------------------------------------------------#
#                          Validate Key Value Pairs                            #
#------------------------------------------------------------------------------#
#' validateKeyValue
#'
#' \code{validateKeyValue} Validates a parameters that contain key/value pairs
#'
#' Function accepts an object with a public parameters member. The parameters
#' member has one of two formats.
#'
#'
#' @usage validateKeyValue(object)
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateKeyValue <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  # Validate key replace
  value <- character()
  key <- character()
  if (length(params$key) > 0) {
    if (is.data.frame(params$key)) {
      if (ncol(params$key) == 2) {
        key <- as.character(params$key[,1])
        value <- as.character(params$key[,2])
      } else {
        key <- as.character(params$key[,1])
        value <- as.character(params$value)
      }
    } else {
      key <- as.character(params$key)
      value <- as.character(params$value)
    }
  }

  if (length(value) > 0 | length(key) > 0) {
    if ((length(value) != 1) &
        (length(value) != length(key))) {
      status[['code']] <- FALSE
      status[['msg']] <- paste0("Values must be of length one",
                                ifelse(length(key) == 1,"",
                                       paste0(" or of a length equal to that ",
                                              "of the key vector, ", length(key))),
                                ". See ?", class(object)[1],
                                " for further assistance")
    }
  }
  return(status)
}
