#' Get FR Y-9c data
#'
#' Query the Chicago Fed to obtain the requested year and quarter FR Y-9c  up to
#' 2021 at \url{https://www.chicagofed.org/banking/financial-institution-reports/bhc-data}
#'
#' After 2021, the data moved to \url{https://www.ffiec.gov/npw/FinancialReport/FinancialDataDownload}
#'
#' @param year the year requested starting in 1986
#' @param quarter the quarter requested [1,2,3,4]
#' @param verbose should messages be printed? (Default: TRUE)
#'
#' @return a \code{data.frame} containing the results
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom utils download.file read.csv
#'
#' @examples
#' \dontrun{X <- get_fry9c_historical_data(2015, 1)}
get_fry9c_historical_data <- function(year, quarter, verbose = TRUE)
{
  assertthat::assert_that(length(year) == 1 && length(quarter) == 1, msg = "year and quarter must be length 1")
  assertthat::assert_that(year >= 1986, msg = "the year must be >= 1986")
  assertthat::assert_that(quarter >= 1 & quarter <= 4, msg = "the quarter must be on [1,2,3,4]")

  cyear <- substring(as.character(year), 3, 4)
  cquarter <- ifelse(quarter == 1, "03",
                  ifelse(quarter == 2, "06",
                         ifelse(quarter == 3, "09", "12")))

  base_url <- "https://www.chicagofed.org/~/media/others/banking/financial-institution-reports/bhc-data/"
  full_url <- paste0(base_url, "bhcf", cyear, cquarter, ".csv")

  file_name <- file.path(tempdir(), paste0("bhcf", cyear, cquarter, ".csv"))
  if (verbose)
  {
    cat("Downloading...\n")
    cat(paste0("\t", full_url, "\n"))
  }
  utils::download.file(full_url, file_name)
  if (verbose)
  {
    cat("Reading results...\n")
    cat(paste0("\t", file_name, "\n"))
  }
  if (file.exists(file_name))
  {
    BHCFYYMM <- utils::read.csv(file_name, header = FALSE, skip = 2, stringsAsFactors = FALSE)
    BHCFYYMM_header <- unlist(utils::read.csv(file_name, header = FALSE, nrows = 1, stringsAsFactors = FALSE))
    names(BHCFYYMM) <- BHCFYYMM_header
  } else
  {
    stop(paste("File Not Created:", file_name))
  }

  unlink(file_name)
  return(BHCFYYMM)
}
