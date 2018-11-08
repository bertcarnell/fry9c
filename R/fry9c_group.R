assertthat::assert_that(require(R6))

#' @title Class providing an object to manipulate a group of FR Y-9c templates, typically
#' used to manage FY Y-9c across years
#' @name fry9c_group
#'
#' @docType class
#' @importFrom R6 R6Class
#' @usage (not recommended) .fry9c_group$new(years, quarters)
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field fry9c_list a list of FR Y-9c objects
#' @field years The year associated with FR Y-9c in the list
#' @field quarters The quarter associated with each FR Y-9c in the list
#' @section Methods:
#' \describe{
#'   \item{Documentation}{}
#'   \item{\code{new(years, quarters)}}{Creates object of this class with room for FR Y-9cs associated with each \code{year} adn \code{quarter}}
#'   \item{\code{parse_fry9c(files)}}{Parse a collection of \code{files} which each represent a FR Y-9c schema.}
#'   \item{\code{initializeData(data_list, banks)}}{Parse a collection of FR Y-9c data files in \code{data_list}.  Also include a vector of \code{banks} that includes the names of the banks associated with the rows as they should appear in output.}
#'   \item{\code{get_fry9c(year, quarter)}}{Extract a \code{fry9c} object from the collection associated with a \code{year} and \code{quarter}}
#'   \item{\code{commonSize(divisor_sched, divisor_num, sched)}}{Common side the \code{sched} using the element from the \code{divisor_sched} and \code{divisor_num}.  This is usually done by common sizing an income statement with the average assets.}
#'   \item{\code{get_plot_data(sched, num)}}{Create a \code{data.frame} that can be used for plotting using \code{ggplot2} by selecting a schedule \code{sched} and element number \code{num}}
#'   \item{\code{length()}}{Return the number of fry9c objects in the collection}
#'   \item{\code{print()}}{Print a summary of the collection contents}
#' }

.fry9c_group <- R6::R6Class("fry9c_group",
                       public = list(
                         initialize = function(years, quarters)
                         {
                           assertthat::assert_that(length(years) == length(quarters))
                           private$len <- length(years)
                           private$years <- years
                           private$quarters <- quarters
                         },
                         parse_fry9c = function(files)
                         {
                           assertthat::assert_that(length(files) == private$len)
                           private$fry9c_list <- vector("list", length(files))
                           for (i in seq_along(files))
                           {
                             private$fry9c_list[[i]] <- parse_fry9c(files[i])
                           }
                         },
                         initializeData = function(data_list, banks)
                         {
                           assertthat::assert_that(length(data_list) == private$len)
                           for (i in seq_along(data_list))
                           {
                             tryCatch({
                               private$fry9c_list[[i]]$initializeData(data_list[[i]])
                             },
                             error = function(e) {
                               cat(paste0("Error in Y:", private$years[i], " Q:", private$quarters[i]))
                               e
                             }
                             )
                             private$fry9c_list[[i]]$addBankNames(banks)
                           }
                         },
                         get_fry9c = function(year, quarter)
                         {
                           ind <- intersect(which(private$years == year),
                                            which(private$quarters == quarter))
                           return(private$fry9c_list[[ind]])
                         },
                         commonSize = function(divisor_sched, divisor_num, sched)
                         {
                           for (i in seq_along(private$years))
                           {
                             divisor <- private$fry9c_list[[i]]$getSchedule(divisor_sched)$getValueFromNum(divisor_num)
                             private$fry9c_list[[i]]$getSchedule(sched)$commonSize(divisor)
                           }
                         },
                         get_plot_data = function(sched, num)
                         {
                           temp <- NULL
                           for (i in seq_along(private$years))
                           {
                             temp <- rbind(temp,
                                           data.frame(year = private$years[i],
                                                      quarter = private$quarters[i],
                                                      bank = private$fry9c_list[[i]]$getSchedule(sched)$getBankNames(),
                                                      value = private$fry9c_list[[i]]$getSchedule(sched)$getValueFromNum(num),
                                                      common_value = private$fry9c_list[[i]]$getSchedule(sched)$getCommonSizeValueFromNum(num),
                                                      stringsAsFactors = FALSE),
                                           stringsAsFactors = FALSE)
                           }
                           temp <- within(temp, annualized_value <- ifelse(quarter == 1, value*4,
                                                                           ifelse(quarter == 2, value*2,
                                                                                  ifelse(quarter == 3, value*4/3, value))))
                           years <- sort(unique(temp$year))
                           if (length(years) > 2)
                           {
                             minyear <- min(temp$year)
                             temp$diff <- 0
                             temp$yoy <- 0
                             temp$common_yoy <- 0
                             for (y in seq_along(years))
                             {
                               if (y == 1) next
                               for (q in 1:4)
                               {
                                 ind <- which(temp$year == years[y] & temp$quarter == q)
                                 indearlier <- which(temp$year == years[y - 1] & temp$quarter == q)
                                 assertthat::assert_that(all(temp$bank[ind] == temp$bank[indearlier]))
                                 temp$diff[ind] <- with(temp, value[ind] - value[indearlier])
                                 temp$yoy[ind] <- with(temp, diff[ind] / value[indearlier])
                                 temp$common_yoy[ind] <- with(temp, (common_value[ind] - common_value[indearlier]) / common_value[indearlier])
                               }
                             }
                           }
                           # if the time series includes quarters, get the differences
                           temp$qdiff <- 0
                           temp$common_qdiff <- 0
                           for (y in seq_along(years))
                           {
                             # if quarter 1 is present, use it
                             ind <- which(temp$year == years[y] & temp$quarter == 1)
                             temp$qdiff[ind] <- temp$value[ind]
                             temp$common_qdiff[ind] <- temp$common_value[ind]
                             # if quarter 1 and 2 are present, use them
                             ind2 <- which(temp$year == years[y] & temp$quarter == 2)
                             if (length(ind2) > 0 && length(ind) > 0)
                             {
                               temp$qdiff[ind2] <- temp$value[ind2] - temp$value[ind]
                               temp$common_qdiff[ind2] <- temp$common_value[ind2] - temp$common_value[ind]
                             }
                             # if quarter 2 and 3 are present
                             ind3 <- which(temp$year == years[y] & temp$quarter == 3)
                             if (length(ind3) > 0 && length(ind2) > 0)
                             {
                               temp$qdiff[ind3] <- temp$value[ind3] - temp$value[ind2]
                               temp$common_qdiff[ind3] <- temp$common_value[ind3] - temp$common_value[ind2]
                             }
                             # if quarter 3 and 4 are present
                             ind4 <- which(temp$year == years[y] & temp$quarter == 4)
                             if (length(ind4) > 0 && length(ind3) > 0)
                             {
                               temp$qdiff[ind4] <- temp$value[ind4] - temp$value[ind3]
                               temp$common_qdiff[ind4] <- temp$common_value[ind4] - temp$common_value[ind3]
                             }
                           }
                           temp$x <- paste0(temp$year, "Q", temp$quarter)
                           return(temp)
                         },
                         length = function()
                         {
                           return(private$len)
                         },
                         print = function()
                         {
                           if (private$len == 0)
                           {
                             cat("Empty FR Y-9c group\n")
                           } else if (private$len > 0 && private$len < 15)
                           {
                             cat(paste("\tyears = ", paste(private$years, collapse = ", "), "\n"))
                             cat(paste("\tquarters = ", paste(private$quarters, collapse = ", "), "\n"))
                           } else
                           {
                             cat(paste("\tyears = ", paste(private$years[1:5], collapse = ", "), "...\n"))
                             cat(paste("\tquarters = ", paste(private$quarters[1:5], collapse = ", "), "...\n"))
                           }
                         }
                       ),
                       private = list(
                         fry9c_list = list(),
                         years = integer(),
                         quarters = integer(),
                         len = integer()
                       ))

#' @rdname fry9c_group
#'
#' @param years The years associated with the fry9c objects in the group
#' @param quarters The quarters associate with the fry9c objects in the group
#'
#' @return an object of class \code{fry9c_group}
#' @export
#'
#' @examples
#' # load example data
#' fry9c_data_list <- list(
#'   read.csv(system.file(file.path("extdata", "ex_BHCF1712.csv"), package = "fry9c")),
#'   read.csv(system.file(file.path("extdata", "ex_BHCF1812.csv"), package = "fry9c")))
#'
#' my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
#'                               quarters = c(4, 4))
#' my_fry9c_group$parse_fry9c(
#'   system.file(file.path("extdata", c("FR_Y-9C20171231.xml", "FR_Y-9C20161231.xml")),
#'               package = "fry9c"))
#'
#' my_fry9c_group$initializeData(fry9c_data_list, paste("bank", LETTERS[1:10], sep=""))
#' print(my_fry9c_group)
#' length(my_fry9c_group) == 2
#'
#' class(my_fry9c_group$get_fry9c(2016, 4))[1] == "fry9c"
#'
#' my_fry9c_group$commonSize("HC-K", "5.", "HI")
#'
#' nrow(my_fry9c_group$get_plot_data("HC-K", "5.")) == 20

Fry9c_group <- function(years, quarters)
{
  return(.fry9c_group$new(years, quarters))
}

#' @rdname fry9c_group
#'
#' @param x the \code{fry9c_group} object
#' @param ... not used
#'
#' @method length fry9c_group
#' @export
#'
#' @return the number of \code{fry9c} objects in the group

length.fry9c_group <- function(x, ...)
{
  return(x$length())
}

