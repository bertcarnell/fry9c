assertthat::assert_that(require(R6))

#' Class providing an object to manipulate a schedule in a FR Y-9c
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' x <- schedule$new("HI", "Income Statement")
#' @field desig the designator of the schedule, e.g. "HI"
#' @field title the title of the schedule
#' @section Methods:
#' \describe{
#'   \item{Documentation}{}
#'   \item{\code{new(desig, title)}}{}
#'   \item{\code{initializeData(dat)}}{}
#'   \item{\code{add(comp)}}{}
#'   \item{\code{export_csv()}}{}
#'   \item{\code{print()}}{}
#'   \item{\code{getValueFromKey(key)}}{}
#'   \item{\code{getValueFromNum(num)}}{}
#'   \item{\code{getCommonSizeValueFromNum(num)}}{}
#'   \item{\code{sumLevels(pos_num_list, neg_num_list)}}{}
#'   \item{\code{getDesig()}}{}
#'   \item{\code{getTitle()}}{}
#'   \item{\code{getBankNames()}}{}
#'   \item{\code{addBankNames(bank_names)}}{}
#'   \item{\code{createDataFrame()}}{}
#'   \item{\code{getComponent(index)}}{}
#'   \item{\code{commonSize(divisor)}}{}
#' }

schedule <- R6::R6Class("schedule",
                    public = list(
                      initialize = function(desig, title)
                      {
                        private$desig <- desig
                        private$title <- title
                        private$len <- 0
                      },
                      initializeData = function(dat)
                      {
                        private$value_len <- nrow(dat)
                        if (private$len > 0)
                        {
                          lapply(private$components, function(x) x$initializeData(dat))
                        }
                      },
                      add = function(comp)
                      {
                        private$components[[private$len + 1]] <- comp
                        private$len <- private$len + 1
                      },
                      export_csv = function()
                      {
                        temp <- paste0("Schedule ", private$desig, ", ", private$title, ", ")
                        if (private$len > 0)
                        {
                          temp <- c(temp, unlist(lapply(private$components, function(z) z$export_csv())))
                        }
                        return(temp)
                      },
                      print = function()
                      {
                        cat(paste0("Schedule ", private$desig, " ", private$title), "\n")
                        if (private$len > 0)
                        {
                          lapply(private$components, print)
                        }
                      },
                      getValueFromKey = function(key)
                      {
                        if (private$len > 0)
                        {
                          for (i in 1:private$len)
                          {
                            if (!is.na(private$components[[i]]$getKey()) &&
                                private$components[[i]]$getKey() == key)
                            {
                              return(private$components[[i]]$getValue())
                            } else {
                              tmp <- private$components[[i]]$getValueFromKey(key)
                              if (!is.null(tmp)) return(tmp)
                            }
                          }
                        }
                      },
                      getValueFromNum = function(num)
                      {
                        if (private$len > 0)
                        {
                          for (i in 1:private$len)
                          {
                            if (private$components[[i]]$getNum() == num)
                            {
                              return(private$components[[i]]$getValue())
                            } else {
                              tmp <- private$components[[i]]$getValueFromNum(num)
                              if (!is.null(tmp)) return(tmp)
                            }
                          }
                        }
                      },
                      getCommonSizeValueFromNum = function(num)
                      {
                        if (private$len > 0)
                        {
                          for (i in 1:private$len)
                          {
                            if (private$components[[i]]$getNum() == num)
                            {
                              return(private$components[[i]]$getCommonSizeValue())
                            } else {
                              tmp <- private$components[[i]]$getCommonSizeValueFromNum(num)
                              if (!is.null(tmp)) return(tmp)
                            }
                          }
                        }
                      },
                      sumLevels = function(pos_num_list, neg_num_list)
                      {
                        temp <- matrix(0, ncol = length(pos_num_list), nrow = private$value_len)
                        if (all(!is.na(pos_num_list)))
                        {
                          temp <- sapply(pos_num_list, function(x) self$getValueFromNum(x))
                        }
                        tempneg <- matrix(0, ncol = length(neg_num_list), nrow = private$value_len)
                        if (all(!is.na(neg_num_list)))
                        {
                          tempneg <- sapply(neg_num_list, function(x) self$getValueFromNum(x))
                        }
                        if (length(dim(temp)) != 2)
                        {
                          cat("an item was likely not found\n")
                          for (i in seq_along(pos_num_list))
                          {
                            cat(paste("\t", pos_num_list[i], paste(self$getValueFromNum(pos_num_list[i]), collapse = ", "), "\n"))
                          }
                          stop("")
                        }
                        if (length(dim(tempneg)) != 2)
                        {
                          cat("an item was likely not found\n")
                          for (i in seq_along(neg_num_list))
                          {
                            cat(paste("\t", neg_num_list[i], paste(self$getValueFromNum(neg_num_list[i]), collapse = ", "), "\n"))
                          }
                          stop("")
                        }
                        return(apply(temp, 1, sum, na.rm = TRUE) - apply(tempneg, 1, sum, na.rm = TRUE))
                      },
                      getDesig = function()
                      {
                        return(private$desig)
                      },
                      getTitle = function()
                      {
                        return(private$title)
                      },
                      getBankNames = function()
                      {
                        return(private$bank_names)
                      },
                      addBankNames = function(bank_names)
                      {
                        assertthat::assert_that(length(bank_names) == private$value_len,
                                                msg = paste0("bank_names are wrong length: ", length(bank_names),
                                                             " vs ", private$value_len))
                        private$bank_names <- bank_names
                      },
                      createDataFrame = function()
                      {
                        dat_names <- c("Num", "Description", private$bank_names)
                        allNums <- lapply(private$components, function(z) z$getAllNums())
                        allNames <- lapply(private$components, function(z) z$getAllNames())
                        allValues <- lapply(private$components, function(z) z$getAllValues())
                        valueMat <- matrix(unlist(allValues), ncol = private$value_len, byrow = TRUE)
                        temp <- data.frame(num = unlist(allNums),
                                           name = unlist(allNames))
                        temp <- cbind(temp, valueMat)
                        names(temp) <- dat_names
                        return(temp)
                      },
                      getComponent = function(index)
                      {
                        return(private$components[[index]])
                      },
                      commonSize = function(divisor)
                      {
                        if (private$len > 0)
                        {
                          dummy <- lapply(private$components, function(z) z$commonSize(divisor))
                        }
                      }
                    ),
                    private = list(
                      desig = character(),
                      title = character(),
                      components = list(),
                      len = integer(),
                      value_len = integer(),
                      bank_names = character()
                    )
)

#' @rdname schedule
#'
#' @param desig the designator of the schedule, e.g. "HI"
#' @param title the title of the schedule
#'
#' @return an object of class \code{schedule}
#' @export
#'
#' @examples
#' Schedule("HI", "Income Statement")

Schedule <- function(desig, title)
{
  return(schedule$new(desig, title))
}
