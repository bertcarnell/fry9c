get_fry9c_data <- function(year, quarter, max_rows = 5000, verbose = TRUE)
{
  file_name <- tempfile(pattern = "bhcdata", tmpdir = tempdir(), fileext = ".zip")
  base_url <- "https://www.chicagofed.org/api/sitecore/BHCHome/GetFile?SelectedQuarter=1&SelectedYear=2018"
  base_url_parsed <- httr::parse_url(base_url)
  base_url_parsed$query$SelectedQuarter <- quarter
  base_url_parsed$query$SelectedYear <- year
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
  extract_files <- unzip(file_name, overwrite = TRUE, unzip = "internal")
  if (verbose)
  {
    cat("Reading results...\n")
    cat(paste0("\t", extract_files[1], "\n"))
  }
  BHCFYYMM <- read.table(extract_files[1], sep = "^", nrows = max_rows,
                         comment.char = "", header = TRUE, quote = "",
                         na.strings = "--------", as.is = TRUE,
                         stringsAsFactors = FALSE)
  if (nrow(BHCFYYMM) == max_rows)
  {
    warning(paste0("The dataset contains ", max_rows, " rows which is the limit of the read"))
  }
  unlink(file_name)
  unlink(extract_files[1])
  return(BHCFYYMM)
}
