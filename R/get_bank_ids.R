#' Get bank IDs for target banks
#'
#' @param bank_names The names of the banks to be selected from the \code{bank_meta_data}
#' @param bank_meta_data a \code{data.frame} with columns for ID_RSSID, Entity_Type, Name
#' @param entity_type The target entity type (Default: "FHD").  \code{NA} indicates the \code{entity_type} is not used.
#' @param most_recent if \code{TRUE} then select the most recent entry.
#'
#' @return If most_recent is \code{TRUE} then a vector of one ID for each bank_name.  Otherwise, multiple IDs may be returned in a list
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' bank_meta_data <- data.frame(ID_RSSID = c(10, 20, 30),
#'   Entity_Type = rep("FHD", 3),
#'   Name = c("HUNTINGTON BANCSHARES INCORPORATED", "KEYCORP", "PNC BANK CORP"))
#' targets <- c("HUNTINGTON BANCSHARES INCORPORATED", "KEYCORP", "PNC BANK CORP")
#' target_ids <- get_bank_ids(targets, bank_meta_data)
#' all(target_ids == c(10, 20, 30))

get_bank_ids <- function(bank_names, bank_meta_data, entity_type="FHD", most_recent=TRUE)
{
  assertthat::assert_that(length(entity_type) == 1, msg = "entity type must be length 1")
  target_ids <- sapply(bank_names, function(x) {
    if (!is.na(entity_type))
    {
      temp <- bank_meta_data[which(grepl(x, bank_meta_data$Name) &
                                     bank_meta_data$Entity_Type == entity_type), "ID_RSSID"]
    } else
    {
      temp <- bank_meta_data[which(grepl(x, bank_meta_data$Name)), "ID_RSSID"]
    }
    # if there is more than one result, take the most recent
    if (length(temp) > 1 && most_recent)
      temp <- temp[length(temp)]
    return(temp)
  }, USE.NAMES = FALSE)
  return(target_ids)
}
