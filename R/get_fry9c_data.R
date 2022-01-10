#' Get FR Y-9c data
#'
#' Query the FFIEC (Federal Financial Institutions Examination Council)
#' NIC (National Information Center) to obtain the requested year and quarter FR Y-9c
#'
#' \url{https://www.ffiec.gov/npw/FinancialReport/FinancialDataDownload}
#'
#' @param year the year requested starting in 2017 (for prior years, use \code{\link{get_fry9c_historical_data}})
#' @param quarter the quarter requested [1,2,3,4]
#' @param verbose should messages be printed? (Default: TRUE)
#'
#' @return a \code{data.frame} containing the results
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom httr parse_url build_url handle GET write_disk
#' @importFrom utils unzip read.table
#'
#' @seealso \code{\link{get_fry9c_historical_data}}
#'
#' @examples
#' \dontrun{X <- get_fry9c_data(2021, 1)}
get_fry9c_data <- function(year, quarter, verbose = TRUE)
{
  # year <- 2018
  # quarter <- 1
  # verbose <- TRUE
  assertthat::assert_that(length(year) == 1 && length(quarter) == 1, msg = "year and quarter must be length 1")
  assertthat::assert_that(year >= 2017, msg = "the year must be >= 2017")
  assertthat::assert_that(quarter >= 1 & quarter <= 4, msg = "the quarter must be on [1,2,3,4]")

  cdate <- ifelse(quarter == 1, "0331",
                  ifelse(quarter == 2, "0630",
                         ifelse(quarter == 3, "0930", "1231")))

  web_file_name <- paste0("BHCF", year, cdate, ".zip")

  file_name <- tempfile(pattern = "bhcdata", tmpdir = tempdir(), fileext = ".zip")
  base_url <- "https://www.ffiec.gov/npw/FinancialReport/ReturnBHCFZipFiles?zipfilename=BHCF20211231.zip"
  base_url_parsed <- httr::parse_url(base_url)
  base_url_parsed$query$zipfilename <- web_file_name
  base_url <- httr::build_url(base_url_parsed)
  h <- httr::handle(base_url)
  if (verbose)
  {
    cat("Querying for web data...\n")
    cat(paste0("\t", base_url, "\n"))
  }
  httr::GET(url = base_url, httr::write_disk(file_name, overwrite = TRUE), handle = h)
  if (verbose)
  {
    cat("Unzipping...\n")
    cat(paste0("\t", file_name, "\n"))
  }
  extract_files <- utils::unzip(file_name, overwrite = TRUE, unzip = "internal")
  if (verbose)
  {
    cat("Reading results...\n")
    cat(paste0("\t", extract_files[1], "\n"))
  }
  # try to read the table.  Some of the files have EOF characters in the fields
  #   Using a connection so that we can re-start the read
  b_second_read <- TRUE
  tryCatch({
    BHCFYYMM <- utils::read.table(extract_files[1], sep = "^", #nrows = max_rows,
                                  comment.char = "", header = TRUE, quote = "",
                                  na.strings = "--------", as.is = TRUE,
                                  stringsAsFactors = FALSE)
    b_second_read <- FALSE
  }, warning = function(w) {
    if (verbose)
    {
      cat("\nRe-Reading file due to a warning")
    }
  })
  if (b_second_read)
  {
    ff <- file(extract_files[1], "r")
    suppressWarnings({
      BHCFYYMM <- utils::read.table(ff, sep = "^", #nrows = max_rows,
                                    comment.char = "", header = TRUE, quote = "",
                                    na.strings = "--------", as.is = TRUE, flush = TRUE,
                                    stringsAsFactors = FALSE)
    })
    if (verbose)
    {
      cat(paste0("First chunk had ", nrow(BHCFYYMM), " rows\n"))
    }
    dummy <- seek(ff)
    # scan to the end of the offending line
    dummy <- scan(ff, what = character(), nlines = 1, quiet = TRUE)
    BHCFYYMM2 <- utils::read.table(ff, sep = "^", #nrows = max_rows,
                                  comment.char = "", header = FALSE, quote = "",
                                  na.strings = "--------", as.is = TRUE,
                                  stringsAsFactors = FALSE)
    if (verbose)
    {
      cat(paste0("Second chunk had ", nrow(BHCFYYMM2), " rows\n"))
    }
    names(BHCFYYMM2) <- names(BHCFYYMM)
    BHCFYYMM <- rbind(BHCFYYMM, BHCFYYMM2)
    close.connection(ff)
  }

  unlink(file_name)
  unlink(extract_files[1])
  return(BHCFYYMM)
}
