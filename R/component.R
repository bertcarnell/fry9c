#' @title Class providing an object to manipulate a component of a schedule in a FR Y-9c
#' @name component
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that

.component <- R6::R6Class("component",
  public = list(
     #' @description
     #' initialize the values in each \code{component}
     #' @param num The line item number of a component of a schedule
     #' @param name The name of the line item number
     #' @param key The lookup key associated with the line item number
     initialize = function(num, name, key)
     {
       assertthat::assert_that(length(num) == 1)
       assertthat::assert_that(length(name) == 1)
       assertthat::assert_that(length(key) == 1)
       private$num <- num
       private$name <- name
       private$key <- key
       private$len <- 0
     },
     #' @description
     #' initialize object with data
     #' @param dat A dataset from the Fed with all FR Y-9c data for a quarter.  The dataset contains columns with names that correspond to \code{key}s
     initializeData = function(dat)
     {
       assertthat::assert_that(is.data.frame(dat), msg = "Data is not contained in a data.frame in Component initializeData")
       private$value_len <- nrow(dat)
       if (is.na(private$key) || all(is.na(dat)))
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
     #' @description
     #' add a sub-\code{component} to this component
     #' @param comp a component
     add = function(comp)
     {
       assertthat::assert_that("component" %in% class(comp), msg = "Can only components to this class")
       private$components[[private$len + 1]] <- comp
       private$len <- private$len + 1
     },
     #' @description
     #' export this \code{component} in CSV format
     export_csv = function()
     {
       temp <- paste0(private$num, ", ", private$name, ", ", paste(private$value, collapse = ","))
       if (private$len > 0)
       {
         temp <- c(temp, unlist(lapply(private$components, function(z) z$export_csv())))
       }
       return(temp)
     },
     #' @description
     #' print the \code{component} as a string
     print = function()
     {
       cat(paste0(private$num, "\t", private$name, "\t", private$value[1], " ...(", length(private$value), ")\n"))
       if (private$len > 0)
       {
         lapply(private$components, print)
       }
     },
     #' @description
     #' get a metric
     #' @param key The lookup key associated with the line item number
     #' @param num The line item number of a component of a schedule
     #' @param metric the metric name
     getMetricFrom = function(key, num=NA, metric=.metric$value)
     {
       if (!missing(key))
       {
         cmp <- self$getComponentFrom(key)
         if (!is.null(cmp))
         {
           if (metric == .metric$value) return(cmp$getValue())
           else if (metric == .metric$common_size_value) return(cmp$getCommonSizeValue())
           else if (metric == .metric$num) return(cmp$getNum())
           else if (metric == .metric$name) return(cmp$getName())
           else if (metric == .metric$key) return(cmp$getKey())
           else stop("metric not found")
         } else
         {
           stop(paste("Key:", key, "not found"))
         }
       } else if (missing(key) && !is.na(num))
       {
         cmp <- self$getComponentFrom(num = num)
         if (!is.null(cmp))
         {
           if (metric == .metric$value) return(cmp$getValue())
           else if (metric == .metric$common_size_value) return(cmp$getCommonSizeValue())
           else if (metric == .metric$num) return(cmp$getNum())
           else if (metric == .metric$name) return(cmp$getName())
           else if (metric == .metric$key) return(cmp$getKey())
           else stop("metric not found")
         } else
         {
           stop(paste("Num:", num, "not found"))
         }
       } else
       {
         stop("Must supply key or num")
       }
     },
     #' @description
     #' get a \code{component} value from this object or a sub-\code{component} that matches the \code{key}
     #' @param key The lookup key associated with the line item number
     getValueFromKey = function(key)
     {
       self$getMetricFrom(key, metric = .metric$value)
     },
     #' @description
     #' get a \code{component} value from the \code{component} number in this object or a sub-\code{component}
     #' @param num The line item number of a component of a schedule
     getValueFromNum = function(num)
     {
       self$getMetricFrom(num = num, metric = .metric$value)
     },
     #' @description
     #' Get a num from a key
     #' @param key The lookup key associated with the line item number
     getNumFromKey = function(key)
     {
       self$getMetricFrom(key, metric = .metric$num)
     },
     #' @description
     #' get a key from a num
     #' @param num The line item number of a component of a schedule
     getKeyFromNum = function(num)
     {
       self$getMetricFrom(num = num, metric = .metric$key)
     },
     #' @description
     #' get a \code{component} common-sized value from the \code{component} number in this object or a sub-\code{component}
     #' @param num The line item number of a component of a schedule
     getCommonSizeValueFromNum = function(num)
     {
       self$getMetricFrom(num = num, metric = .metric$common_size_value)
     },
     #' @description
     #' get a \code{component} common-sized value from the \code{component} \code{key} in this object or a sub-\code{component}
     #' @param key The lookup key associated with the line item number
     getCommonSizeValueFromKey = function(key)
     {
       self$getMetricFrom(key, metric = .metric$common_size_value)
     },
     #' @description
     #' get a Component
     #' @param num The line item number of a component of a schedule
     #' @param key The lookup key associated with the line item number
     getComponentFrom = function(key, num=NA)
     {
       if (!missing(key))
       {
         if (!is.na(self$getKey()) && self$getKey() == key)
         {
           return(self)
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
               return(private$components[[i]])
             } else
             {
               tmp <- private$components[[i]]$getComponentFrom(key)
               if (!is.null(tmp)) return(tmp)
             }
           }
         }
       } else if (missing(key) && !is.na(num))
       {
         if (!is.na(self$getNum()) && self$getNum() == num)
         {
           return(self)
         } else if (private$len == 0)
         {
           return(NULL)
         } else
         {
           for (i in 1:private$len)
           {
             if (!is.na(private$components[[i]]$getNum()) &&
                 private$components[[i]]$getNum() == num)
             {
               return(private$components[[i]])
             } else
             {
               tmp <- private$components[[i]]$getComponentFrom(num = num)
               if (!is.null(tmp)) return(tmp)
             }
           }
         }
       } else
       {
         stop("Must supply key or num")
       }
     },
     #' @description
     #' Get a component from a key
     #' @param key The lookup key associated with the line item number
     getComponentFromKey = function(key)
     {
       self$getComponentFrom(key)
     },
     #' @description
     #' Get a component from a key
     #' @param num The line item number of a component of a schedule
     getComponentFromNum = function(num)
     {
       self$getComponentFrom(num = num)
     },
     #' @description
     #' get the value of this object
     getValue = function() {
       return(private$value)
     },
     #' @description
     #' get the common-sized value of this object
     getCommonSizeValue = function() {
       return(private$common_size_value)
     },
     #' @description
     #' get the \code{key} from this object
     getKey = function() {
       return(private$key)
     },
     #' @description
     #' get the \code{num} from this object
     getNum = function() {
       return(private$num)
     },
     #' @description
     #' get the \code{name} from this object
     getName = function() {
       return(private$name)
     },
     #' @description
     #' get the values from this object and all sub-\code{component}s
     getAllValues = function()
     {
       if (private$len == 0)
         return(self$getValue())
       else
         list(self$getValue(), lapply(private$components, function(z) z$getAllValues()))
     },
     #' @description
     #' get all the \code{num}s from this object and all sub-\code{component}s
     getAllNums = function()
     {
       if (private$len == 0)
         return(self$getNum())
       else
         list(self$getNum(), lapply(private$components, function(z) z$getAllNums()))
     },
     #' @description
     #' get all the \code{name}s from this object and all sub-\code{component}s
     getAllNames = function()
     {
       if (private$len == 0)
         return(self$getName())
       else
         list(self$getName(), lapply(private$components, function(z) z$getAllNames()))
     },
     #' @description
     #' common-size this component using the \code{divisor}
     #' @param divisor a numeric that a \code{component}'s values are divided by
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

.metric <- list(
  value = 1,
  common_size_value = 2,
  num = 3,
  name = 4,
  key = 5
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
#' x <- Component("1.a.", "Income", "ZZZZ1234")
#' x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
#' x$initializeData(data.frame(ZZZZ1234 = 1:4,
#'                             ABCD5555 = 5:8))
#' x$export_csv()[1] == "1.a., Income, 1,2,3,4"
#' print(x)
#' all(x$getValueFromKey("ABCD5555") == 5:8)
#' all(x$getValueFromNum("1.a.") == 1:4)
#' all(x$getValue() == 1:4)
#' x$getKey() == "ZZZZ1234"
#' x$getNum() == "1.a."
#' x$getName() == "Income"
#' all(unlist(x$getAllValues()) == 1:8)
#' all(unlist(x$getAllNums()) == c("1.a.", "1.a.(1)"))
#' all(unlist(x$getAllNames() == c("Income", "Sub-Income")))
#' x$commonSize(100)
#' all.equal(x$getCommonSizeValueFromNum("1.a."), (1:4)/100)
#' all.equal(x$getCommonSizeValueFromKey("ZZZZ1234"), (1:4)/100)
#' all.equal(x$getCommonSizeValue(), (1:4)/100)

Component <- function(num, name, key)
{
  return(.component$new(num, name, key))
}
