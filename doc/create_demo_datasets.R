# Create demo data sources

# data output location
repositoryPath <- file.path("C:","Users","Rob","Documents","Repositories")
data_output_dir <- file.path(repositoryPath, "fry9c", "inst", "extdata")

###############################################################################
# Bank Meta Data #

# get bank meta data and create a small dataset
bank_meta_data <- get_bank_meta_data()
targets <- c("HUNTINGTON BANCSHARES INCORPORATED",
             "KEYCORP",
             "PNC BANK CORP",
             "JPMORGAN CHASE",
             "FIFTH THIRD BANCORP",
             "U.S. BANCORP",
             "M&T BANK",
             "BB&T CORPORATION",
             "REGIONS FINANCIAL CORPORATION",
             "SUNTRUST BANKS, INC")
target_ids <- get_bank_ids(targets, bank_meta_data)
bank_meta_data <- subset(bank_meta_data, ID_RSSID %in% target_ids)
dim(bank_meta_data)

write.csv(bank_meta_data, file = file.path(data_output_dir, "ex_bank_meta_data.csv"))

###############################################################################
# FR Y-9c data #

fry9c_data_list <- list(get_fry9c_data(2017, 4, verbose = FALSE),
                        get_fry9c_data(2016, 4, verbose = FALSE))

fry9c_data_list_small <- lapply(fry9c_data_list, function(z) z[match(target_ids, z$RSSD9001),])

lapply(fry9c_data_list, dim)
lapply(fry9c_data_list_small, dim)

write.csv(fry9c_data_list_small[[1]], file = file.path(data_output_dir, "ex_BHCF1712.csv"))
write.csv(fry9c_data_list_small[[2]], file = file.path(data_output_dir, "ex_BHCF1812.csv"))
