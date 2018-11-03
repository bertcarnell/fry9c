# entity_type FHD default, NA means don't use it
# most_recent takes the most recent entry, FALSE takes all entries

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
