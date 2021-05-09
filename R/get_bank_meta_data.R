#' Get bank metadata from the Chicago Fed
#'
#' @return a \code{data.frame} containing the metadata
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom httr handle GET write_disk
#' @importFrom utils read.fwf
#'
#' @examples
#' \dontrun{
#' get_bank_meta_data()
#' }
get_bank_meta_data <- function()
{
  tmpdir <- tempdir()
  file_name <- file.path(tmpdir, "hc-name-list-pdf.pdf")
  my_url <- "https://www.chicagofed.org/~/media/others/banking/financial-institution-reports/hc-name-list-pdf.pdf?la=en"
  h <- httr::handle(my_url)
  httr::GET(url = my_url, httr::write_disk(file_name, overwrite = TRUE), handle = h)
  bank_text <- pdftools::pdf_text(file_name)
  unlink(file_name)
  res <- vector("list", length = length(bank_text))
  for (i in 1:length(bank_text)) # loop over pages
  {
    temp <- strsplit(bank_text[i], "\r\n")[[1]]
    res[[i]] <- temp[grepl("^[0-9]+", trimws(temp))]
    # padd each line with whitespace
    res[[i]] <- paste0(res[[i]], "                  ")
  }
  res <- unlist(res)
  tf <- tempfile("banklisting.txt")
  writeLines(text = res, con = tf)
  bank_meta_data <- utils::read.fwf(file = tf, widths = c(9,11,200),
                                    strip.white = TRUE, stringsAsFactors = FALSE,
                                    comment.char = "")
  unlink(tf)
  names(bank_meta_data) <- c("ID_RSSID", "Entity_Type", "Name")
  return(bank_meta_data)
}
