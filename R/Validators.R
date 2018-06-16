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

  if (length(params$classes$name) > 0) {
    for (i in 1:length(params$classes$name)) {
      if (sum(class(params$classes$objects[[i]]) %in% params$classes$valid[[i]]) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Class ", class(params$classes$objects[[i]])[[1]],
                                  " is invalid for the '",
                                  params$classes$name[[i]], "' parameter. ",
                                  "Valid classes include ",
                                  paste0("c(",gsub(",$", "",
                                                   paste0("'", params$classes$valid[[i]],
                                                          "',", collapse = "")),
                                         "). "),
                                  "See ?", class(object)[[1]],
                                  " for further assistance.")
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
  if (is.null(params$kv$equalLen)) params$kv$equalLen <- FALSE

  # Validate key replace
  value <- character()
  key <- character()
  if (length(params$kv$key) > 0) {
    if (is.data.frame(params$kv$key)) {
      if (ncol(params$kv$key) == 2) {
        key <- as.character(params$kv$key[,1])
        value <- as.character(params$kv$key[,2])
      } else {
        key <- as.character(params$kv$key[,1])
        value <- as.character(params$kv$value)
      }
    } else {
      key <- as.character(params$kv$key)
      value <- as.character(params$kv$value)
    }
  }

  # Handle valid request
  if (length(key) == length(value)) return(status)
  if (length(key) == 1 & params$kv$equalLen == FALSE) return(status)

  # Handle invalid request
  status[['code']] <- FALSE
  if (params$kv$equalLen) {
    status[['msg']] <- paste0("Key and value vectors must be of equal ",
                              "length. See ?", class(object)[1],
                              " for further assistance.")
  } else {
    status[['msg']] <- paste0("Values must be of length one",
                              ifelse(length(key) == 1,"",
                                     paste0(" or of a length equal to that ",
                                            "of the key vector, ", length(key))),
                              ". See ?", class(object)[1],
                              " for further assistance")
  }
  return(status)
}

#------------------------------------------------------------------------------#
#                              Validate Range                                  #
#------------------------------------------------------------------------------#
#' validateRange
#'
#' \code{validateRange} Validates a parameter that takes a range of values.
#'
#' Class accepts an object with a public parameters member. The parameters
#' member must be a list with four elements: (1) the variable name,
#' (2) the variable value, (3), the low range, and (4), the high range.
#' All ranges are inclusive.
#'
#' @usage validateRange(object)
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validateRange <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$range$value) > 0) {
    if (params$range$value < params$range$low |
        params$range$value > params$range$high ) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid ", params$range$variable,
                                  " parameter. Valid values must be between ",
                                  params$range$low, " and ", params$range$high,
                                  ". See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
    }
  return(status)
}

#------------------------------------------------------------------------------#
#                              Validate Path                                   #
#------------------------------------------------------------------------------#
#' validatePath
#'
#' \code{validatePath} Validates the existence or non-existence of a path.
#'
#' Class returns an error if the existence of a path is not expected.
#'
#' @usage validateRange(object)
#'
#' @param object The object to be validated
#'
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Functions
#' @export
validatePath <- function(object) {

  status <- list()
  status$code <- TRUE
  status$msg <- NULL

  params <- object$getParams()

  if (length(params$file$path) > 0) {
    for (i in 1:length(params$file$path)) {
      if (file.exists(params$file$path[i]) != params$file$expect[i]) {
        status[['code']] <- FALSE
        if (file.exists(params$file$path[i])) {
          status[['msg']] <- paste0("File path ", params$file$path[i], " already exists.")
        } else {
          status[['msg']] <- paste0("File path ", params$file$path[i], " does not exist.")
        }
        return(status)
      }
    }
  }
  return(status)
}

