assertthat::assert_that(require(R6))

#' Class providing an object to manipulate a FR Y-9c and component schedules
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' x <- fry9c$new("20180331", "7100-0128",
#'                "Consolidated Financial Statements for Holding Companies--FR Y-9c")
#' @field date The date associated with the FR Y-9c
#' @field omb_number The OMB number of the FR Y-9c documentation.  Something like "7100-0128"
#' @field title The title of the FR Y-9c.  Something like "Consolidated Financial Statements for Holding Companies--FR Y-9c"
#' @section Methods:
#' \describe{
#'   \item{Documentation}{}
#'   \item{\code{new(date, omb_number, title)}}{}
#'   \item{\code{add(sched)}}{}
#'   \item{\code{getSchedule(desig)}}{}
#'   \item{\code{initializeData(dat)}}{}
#'   \item{\code{print()}}{}
#'   \item{\code{addBankNames(bank_names)}}{}
#'   \item{\code{exportExcel(output_file_name)}}{}
#' }

fry9c <- R6::R6Class("fry9c",
                 public = list(
                   initialize = function(date, omb_number, title){
                     private$date <- date
                     private$omb_number <- omb_number
                     private$title <- title
                     private$len <- 0
                   },
                   add = function(sched)
                   {
                     private$schedules[[private$len + 1]] <- sched
                     private$len <- private$len + 1
                   },
                   getSchedule = function(desig)
                   {
                     for (i in seq_along(private$schedules))
                     {
                       if (private$schedules[[i]]$getDesig() == desig)
                         return(private$schedules[[i]])
                     }
                   },
                   initializeData = function(dat)
                   {
                     for (i in seq_along(private$schedules))
                     {
                       private$schedules[[i]]$initializeData(dat)
                     }
                   },
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
                   addBankNames = function(bank_names)
                   {
                     invisible(lapply(private$schedules, function(z) z$addBankNames(bank_names)))
                   },
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
#' Fry9c("20180331", "7100-0128",
#'       "Consolidated Financial Statements for Holding Companies--FR Y-9c")

Fry9c <- function(date, omb_number, title)
{
  return(fry9c$new(date, omb_number, title))
}
