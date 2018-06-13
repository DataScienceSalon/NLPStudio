#==============================================================================#
#                               Director                                       #
#==============================================================================#
#' Director
#'
#' \code{Director} Class responsible for executing an NLP Pipeline
#'
#' @section Director methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a Director.}
#'   \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
#'   \item{\code{validate()}}{Method that validates inputs/outputs at each stage of pipeline}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{printTasks()}}{Prints task list. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#'  @param x Character vector containing a directory,a Corpus object, quanteda corpus, tm
#'  VCorpus object, a FileSet or a series of character vectors containing text. This is
#'  the seed object for the Director - the initial object that serves as input for the
#'  first command.
#'  @param cmd The class encapsulating a particular command.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Director classes
#' @export
Director <- R6::R6Class(
  classname = "Director",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..target = character(),
    ..log = data.table(),
    ..queue = list(),
    ..tasks = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadServices(name = 'Director')
      private$..log <- data.table(n = integer(0),
                                  task = character(0),
                                  receiver = character(0),
                                  inputClass = character(0),
                                  inputName = character(0),
                                  outputClass = character(0),
                                  outputName = character(0),
                                  start = character(0),
                                  end = character(0),
                                  duration.min = numeric(0),
                                  user = character(0))

      private$..tasks <- 0
      private$..target <- x
      event <- paste0("Director object instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Command Management                            #
    #-------------------------------------------------------------------------#
    add = function(cmd) {

      private$..params <- list()
      private$..params$classes$name <- list('cmd')
      private$..params$classes$objects <- list(cmd)
      private$..params$classes$valid <- list(c('Cmd0'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'add', event = v$msg, level = "Error")
        stop()
      }

      # Add to queue
      id <- cmd$getId()
      private$..queue[[id]] <- cmd

      # Format and write log
      private$..tasks <- private$..tasks + 1
      task <- class(cmd)[1]
      r <- cmd$getReceiver()
      receiver <- class(r)[1]
      user <-  Sys.info()[['user']]
      # Write to log
      df <- data.table(n = private$..tasks,
                       task = task,
                       receiver = receiver,
                       inputClass = "",
                       inputName = "",
                       outputClass = "",
                       outputName = "",
                       start = "",
                       end = "",
                       duration.min = 0,
                       user = user)
      private$..log <- rbind(private$..log, df)

      event <- paste0("Added ", class(cmd)[1], " (", id, ") to job queue." )
      private$logR$log(method = 'add', event = event)

      invisible(self)
    },

    log = function() {
      label <- "Director Task Log"
      NLPStudio::printHeading(text = label, symbol = "=", newlines = 2)
      print(private$..log)
      invisible(private$..log)
    },

    #-------------------------------------------------------------------------#
    #                       Execute and Return Results                        #
    #-------------------------------------------------------------------------#
    execute = function() {

      if (length(private$..queue) > 0) {

        for (i in 1:length(private$..queue)) {
          inClass <- class(private$..target)[1]
          inName <- ifelse(length(private$..target) == 0, "",
                           ifelse(class(private$..target)[1] == 'character', "",
                                  private$..target$getName()))
          startTime <- Sys.time()

          private$..target <- private$..queue[[i]]$execute(private$..target)

          stopTime <- Sys.time()
          outClass <- class(private$..target)[1]
          outName <- ifelse(length(private$..target) == 0, "",
                           ifelse(class(private$..target)[1] == 'character', "",
                                  private$..target$getName()))
          duration <- round(as.numeric(difftime(stopTime, startTime, units = 'min')), 4)

          private$..log[n == i, inputClass := inClass]
          private$..log[n == i, inputName := inName]
          private$..log[n == i, outputClass := outClass]
          private$..log[n == i, outputName := outName]
          private$..log[n == i, start := as.character(startTime)]
          private$..log[n == i, end := as.character(stopTime)]
          private$..log[n == i, duration.min := duration]
          private$..log[n == i, user := Sys.info()[['user']]]
        }

        event <- paste0("Director pipeline complete." )
        private$logR$log(method = 'execute', event = event)

        invisible(private$..target)
      } else {
        event <- paste0("Director job queue is empty.")
        private$logR$log(method = 'execute', event = event, level = 'Warn')
        invisible(self)
      }
    },
    setTarget = function(target) private$..target <- target,
    getTarget = function() private$..target,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$director(self)
    }
  )
)
