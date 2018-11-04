assertthat::assert_that(require(R6))

#' Class providing an object to manipulate a component of a schedule in a FR Y-9c
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' x <- component$new("1.a.", "Income", "ZZZZ1234")
#' @field num The line item number of a component of a schedule
#' @field name The name of the line item number
#' @field key The lookup key associated with the line item number
#' @section Methods:
#' \describe{
#'   \item{Documentation}{}
#'   \item{\code{new(num, name, key)}}{}
#'   \item{\code{initializeData(dat)}}{}
#'   \item{\code{add(comp)}}{}
#'   \item{\code{export_csv()}}{}
#'   \item{\code{print()}}{}
#'   \item{\code{getValueFromKey(key)}}{}
#'   \item{\code{getValueFromNum(num)}}{}
#'   \item{\code{getCommonSizeValueFromNum(num)}}{}
#'   \item{\code{getValue()}}{}
#'   \item{\code{getCommonSizeValue}}{}
#'   \item{\code{getKey()}}{}
#'   \item{\code{getNum()}}{}
#'   \item{\code{getName()}}{}
#'   \item{\code{getAllValues()}}{}
#'   \item{\code{getAllNums()}}{}
#'   \item{\code{getAllNames()}}{}
#'   \item{\code{commonSize(divisor)}}{}
#' }

component <- R6::R6Class("component",
                     public = list(
                       initialize = function(num, name, key)
                       {
                         private$num <- num
                         private$name <- name
                         private$key <- key
                         private$len <- 0
                       },
                       initializeData = function(dat)
                       {
                         assertthat::assert_that(is.data.frame(dat))
                         private$value_len <- nrow(dat)
                         if (is.na(private$key) || is.na(dat))
                         {
                           private$value <- rep(NA, nrow(dat))
                         } else
                         {
                           assertthat::assert_that(private$key %in% names(dat),
                                                   msg = paste0("Key: ", private$key, " - Not Found in Data"))
                           private$value <- as.numeric(dat[,private$key])
                         }
                         if (private$len > 0)
                         {
                           lapply(private$components, function(x) x$initializeData(dat))
                         }
                         private$common_size_value <- rep(NA, nrow(dat))
                       },
                       add = function(comp)
                       {
                         private$components[[private$len + 1]] <- comp
                         private$len <- private$len + 1
                       },
                       export_csv = function()
                       {
                         temp <- paste0(private$num, ", ", private$name, ", ", paste(private$value, collapse = ","))
                         if (private$len > 0)
                         {
                           temp <- c(temp, unlist(lapply(private$components, function(z) z$export_csv())))
                         }
                         return(temp)
                       },
                       print = function()
                       {
                         cat(paste0(private$num, "\t", private$name, "\t", private$value[1], " ...(", length(private$value), ")\n"))
                         if (private$len > 0)
                         {
                           lapply(private$components, print)
                         }
                       },
                       getValueFromKey = function(key)
                       {
                         if (!is.na(self$getKey()) && self$getKey() == key)
                         {
                           return(self$getValue())
                         } else if (private$len == 0)
                         {
                           return(NULL)
                         } else
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
                         if (!is.na(self$getNum()) && self$getNum() == num)
                         {
                           return(self$getValue())
                         } else if (private$len == 0)
                         {
                           return(NULL)
                         } else
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
                         if (!is.na(self$getNum()) && self$getNum() == num)
                         {
                           return(self$getCommonSizeValue())
                         } else if (private$len == 0)
                         {
                           return(NULL)
                         } else
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
                       getValue = function() {
                         return(private$value)
                       },
                       getCommonSizeValue = function() {
                         return(private$common_size_value)
                       },
                       getKey = function() {
                         return(private$key)
                       },
                       getNum = function() {
                         return(private$num)
                       },
                       getName = function() {
                         return(private$name)
                       },
                       getAllValues = function()
                       {
                         if (private$len == 0)
                           return(self$getValue())
                         else
                           list(self$getValue(), lapply(private$components, function(z) z$getAllValues()))
                       },
                       getAllNums = function()
                       {
                         if (private$len == 0)
                           return(self$getNum())
                         else
                           list(self$getNum(), lapply(private$components, function(z) z$getAllNums()))
                       },
                       getAllNames = function()
                       {
                         if (private$len == 0)
                           return(self$getName())
                         else
                           list(self$getName(), lapply(private$components, function(z) z$getAllNames()))
                       },
                       commonSize = function(divisor)
                       {
                         private$common_size_value <- private$value / divisor
                         if (private$len > 0)
                           dummy <- lapply(private$components, function(z) z$commonSize(divisor))
                       }
                     ),
                     private = list(
                       num = character(),
                       name = character(),
                       key = character(),
                       components = list(),
                       len = integer(),
                       value = double(),
                       isDataInitialized = logical(),
                       value_len = integer(),
                       common_size_value = double()
                     )
)

#' @rdname component
#'
#' @param num The line item number of a component of a schedule
#' @param name The name of the line item number
#' @param key The lookup key associated with the line item number
#'
#' @return an object of class \code{component}
#' @export
#'
#' @examples
#' Component("1.a.", "Income", "ZZZZ1234")

Component <- function(num, name, key)
{
  return(component$new(num, name, key))
}
