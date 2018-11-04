assertthat::assert_that(require(R6))

#' Class providing an object to manipulate a group of FR Y-9c templates, typically
#' used to manage FY Y-9c across years
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' x <- fry9c_group$new(c(2016, 2017), c(1, 1))
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

fry9c_group <- R6::R6Class("fry9c_group",
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
                           temp$qdiff <- 0
                           temp$common_qdiff <- 0
                           for (y in seq_along(years))
                           {
                             ind <- which(temp$year == years[y] & temp$quarter == 1)
                             temp$qdiff[ind] <- temp$value[ind]
                             temp$common_qdiff[ind] <- temp$common_value[ind]
                             for (q in 2:4)
                             {
                               ind <- which(temp$year == years[y] & temp$quarter == q)
                               indearlier <- which(temp$year == years[y] & temp$quarter == q - 1)
                               temp$qdiff[ind] <- temp$value[ind] - temp$value[indearlier]
                               temp$common_qdiff[ind] <- temp$common_value[ind] - temp$common_value[indearlier]
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
#' Fry9c_group(rep(2015:2017, each=4), rep(1:4, times=3))

Fry9c_group <- function(years, quarters)
{
  return(fry9c_group$new(years, quarters))
}

#' S3 Method for the length of a \code{fry9c_group}
#'
#' @param x the \code{fry9c_group} object
#' @param ... not used
#'
#' @return the number of \code{fry9c} objects in the group
#'
#' @examples
#' x <- fry9c_group$new(c(2016, 2017), c(1, 1))
#' length(x) == 2

length.fry9c_group <- function(x, ...)
{
  return(x$length())
}

