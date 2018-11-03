Fry9c <- function(date, omb_number, title)
{
  return(fry9c$new(date, omb_number, title))
}

fry9c <- R6Class("fry9c",
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
                     for (i in 1:private$len)
                     {
                       sched <- private$schedules[[i]]
                       cat(paste0("    Schedule ", sched$getDesig(), " ", sched$getTitle(), "\n"))
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
