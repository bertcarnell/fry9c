#' @title Class providing an object to manipulate a FR Y-9c and component schedules
#' @name fry9c
#'
#' @importFrom R6 R6Class

.fry9c <- R6::R6Class("fry9c",
   public = list(
     #' @description
     #' initialize the class
     #' @param date The date associated with the FR Y-9c
     #' @param omb_number The OMB number of the FR Y-9c documentation.  Something like "7100-0128"
     #' @param title The title of the FR Y-9c.  Something like "Consolidated Financial Statements for Holding Companies--FR Y-9c"
     initialize = function(date, omb_number, title)
     {
       assertthat::assert_that(length(date) == 1 & length(omb_number) == 1 & length(title) == 1,
                               msg = "fry9c can only initialize with length 1")
       private$date <- date
       private$omb_number <- omb_number
       private$title <- title
       private$len <- 0
     },
     #' @description
     #' add a new \code{schedule}
     #' @param sched a schedule
     add = function(sched)
     {
       assertthat::assert_that("schedule" %in% class(sched))
       private$schedules[[private$len + 1]] <- sched
       private$len <- private$len + 1
     },
     #' @description
     #' Get a \code{schedule} by the \code{desig}
     #' @param desig An identifier for a component \code{schedule}
     getSchedule = function(desig)
     {
       for (i in seq_along(private$schedules))
       {
         if (private$schedules[[i]]$getDesig() == desig)
           return(private$schedules[[i]])
       }
       # if no matching schedule is found, error
       stop(paste("Schedule ", desig, "Not Found"))
     },
     #' @description
     #' initialize the values in each \code{schedule} and \code{component}
     #' @param dat A dataset from the Fed with all FR Y-9c data for a quarter.  The dataset contains columns with names that correspond to \code{key}s
     initializeData = function(dat)
     {
       assertthat::assert_that(is.data.frame(dat))
       for (i in seq_along(private$schedules))
       {
         private$schedules[[i]]$initializeData(dat)
       }
     },
     #' @description
     #' print the \code{schedule}s and \code{component}s as a string
     print = function()
     {
       cat(paste0("  ", private$title, "\n"))
       cat(paste0("  ", private$date, "\n"))
       if (private$len > 0)
       {
         for (i in 1:private$len)
         {
           sched <- private$schedules[[i]]
           cat(paste0("    Schedule ", sched$getDesig(), " ", sched$getTitle(), "\n"))
         }
       }
     },
     #' @description
     #' add \code{bank_names} to this object
     #' @param bank_names a vector of bank names to be stored in the \code{schedule}
     addBankNames = function(bank_names)
     {
       invisible(lapply(private$schedules, function(z) z$addBankNames(bank_names)))
     },
     #' @description
     #' export an Excel file
     #' @param output_file_name output file name
     exportExcel = function(output_file_name)
     {
       wb <- openxlsx::createWorkbook("fr y-9c")
       for (i in 1:private$len)
       {
         temp_sheetname <- paste("Schedule", private$schedules[[i]]$getDesig())
         openxlsx::addWorksheet(wb, temp_sheetname)
         openxlsx::writeData(wb, temp_sheetname, private$schedules[[i]]$createDataFrame())
       }
       openxlsx::saveWorkbook(wb, file = output_file_name, overwrite = FALSE)
     },
     #' @description
     #' Get the Date
     getDate = function()
     {
       return(private$date)
     },
     #' @description
     #' get the OMB number
     getOmbNumber = function()
     {
       return(private$omb_number)
     },
     #' @description
     #' Get the title
     getTitle = function()
     {
       return(private$title)
     }
   ),
   private = list(
     date = character(),
     omb_number = character(),
     title = character(),
     len = integer(),
     schedules = list()
   )
)

#' @rdname fry9c
#'
#' @param date The date associated with the FR Y-9c
#' @param omb_number The OMB number of the FR Y-9c documentation.  Something like "7100-0128"
#' @param title The title of the FR Y-9c.  Something like "Consolidated Financial Statements for Holding Companies--FR Y-9c"
#'
#' @return an object of class \code{fry9c}
#' @export
#'
#' @examples
#' f <- Fry9c("20180331", "7100-0128",
#'            "Consolidated Financial Statements for Holding Companies--FR Y-9c")
#' x <- Schedule("HI", "Income Statement")
#' y <- Component("1.a.", "Income", "ZZZZ1234")
#' y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
#' x$add(y)
#' f$add(x)
#' f$initializeData(data.frame(ZZZZ1234 = 1:4,
#'                             ABCD5555 = 5:8))
#' print(f)
#' f$getSchedule("HI")$getDesig() == "HI"
#' f$addBankNames(paste("bank", LETTERS[1:4], sep=""))
#' \dontrun{
#'   f$exportExcel(tempfile(fileext = ".xlsx"))
#' }

Fry9c <- function(date, omb_number, title)
{
  return(.fry9c$new(date, omb_number, title))
}
