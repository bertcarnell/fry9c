
Fry9c_group <- function(years, quarters)
{
  return(fry9c_group$new(years, quarters))
}

#' Class providing an object to manipulate a group of FR Y-9c templates, typically
#' used to manage FY Y-9c across years
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' fry9c_group$new(c(2016, 2017), c(1, 1))
#' @field fry9c_list a list of FR Y-9c objects
#' @field years The year associated with FR Y-9c in the list
#' @field quarters The quarter associated with each FR Y-9c in the list
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{}
#'   \item{\code{new(years, quarters)}}{Creates object of this class with room for FR Y-9cs associated with each \code{year} adn \code{quarter}}
#'   \item{\code{parse_fry9c(files)}}{Parse a collection of \code{files} which each represent a FR Y-9c schema.}

#'   \item{\code{createsession(sessionname = "")}}{This method creates new session on the server with optionally given name in \code{sessionname}.}
#'   \item{\code{usesession(sessionid)}}{This method changes currently used session on the server to the one with id given in \code{sessionid} parameter.}
#'   \item{\code{openviz(vizid = NA)}}{This method by default opens most recently created by this object visualization. If \code{vizid} parameter is given, it opens a visualization with given id instead.}
#'   \item{\code{enableautoopening()}}{This method enables auto opening of every visualisation that you create since that moment. Disabled by default.}
#'   \item{\code{disableautoopening()}}{This method disables auto opening of every visualisation that you create since that moment. Disabled by default.}
#'   \item{\code{line(series, index = NA, color = NA, label = NA, size = NA, xaxis = NA, yaxis = NA, logScaleX = "false", logScaleY = "false")}}{This method creates a line visualization for vector/matrix with each row representing a line, given in \code{series}.}
#'   \item{\code{scatter(x, y, color = NA, label = NA, size = NA, alpha = NA, xaxis = NA, yaxis = NA)}}{This method creates a scatterplot for points with coordinates given in vectors \code{x, y}.}
#'   \item{\code{linestacked(series, color = NA, label = NA, size = NA)}}{This method creates a plot of multiple lines given in matrix \code{series}, with an ability to hide and show every one of them.}
#'   \item{\code{force(matrix, color = NA, label = NA, size = NA)}}{This method creates a force plot for matrix given in \code{matrix}.}
#'   \item{\code{graph(x, y, matrix, color = NA, label = NA, size = NA)}}{This method creates a graph of points with coordinates given in \code{x, y} vectors, with connection given in \code{matrix} connectivity matrix.}
#'   \item{\code{map(regions, weights, colormap)}}{This method creates a world (or USA) map, marking regions given as a vector of abbreviations (3-char for countries, 2-char for states) in \code{regions} with weights given in \code{weights} vector and with \code{colormap} color (string from colorbrewer).}
#'   \item{\code{graphbundled(x, y, matrix, color = NA, label = NA, size = NA)}}{This method creates a bundled graph of points with coordinates given in \code{x, y} vectors, with connection given in \code{matrix} connectivity matrix. Lines on this graph are stacked a bit more than in the \code{graph} function.}
#'   \item{\code{matrix(matrix, colormap)}}{This method creates a visualization of matrix given in \code{matrix} parameter, with its contents used as weights for the colormap given in \code{colormap} (string from colorbrewer).}
#'   \item{\code{adjacency(matrix, label = NA)}}{This method creates a visualization for adjacency matrix given in \code{matrix} parameter.}
#'   \item{\code{scatterline(x, y, t, color = NA, label = NA, size = NA)}}{This method creates a scatterplot for coordinates in vectors \code{x, y} and assignes a line plot to every point on that plot. Each line is given as a row in \code{t} matrix.}
#'   \item{\code{scatter3(x, y, z, color = NA, label = NA, size = NA, alpha = NA)}}{This method creates a 3D scatterplot for coordinates given in vectors \code{x, y, z}.}
#'   \item{\code{image(imgpath)}}{This method uploads image from file \code{imgpath} to the server and creates a visualisation of it.}
#'   \item{\code{gallery(imgpathvector)}}{This method uploads images from vector of file paths \code{imgpathvector} to the server and creates a gallery of these images.}}

fry9c_group <- R6Class("fry9c_group",
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

length.fry9c_group <- function(x, ...)
{
  return(x$length())
}

