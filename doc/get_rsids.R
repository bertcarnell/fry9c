# get reserve system IDs
# ID_RSSD
get_rsids <- function(file_name)
{
  #bank_text <- pdftools::pdf_text(file.path("C:", "developer", "repositories", "ProvableBanking", "hc-name-list.pdf"))
  bank_text <- pdftools::pdf_text(file_name)
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
  bank_meta_data <- read.fwf(file = tf, widths = c(9,11,200), strip.white = TRUE, stringsAsFactors = FALSE,
                             comment.char = "")
  unlink(tf)
  names(bank_meta_data) <- c("ID_RSSID", "Entity_Type", "Name")
  return(bank_meta_data)
}
