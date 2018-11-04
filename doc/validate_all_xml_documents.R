# validate all xml files as a test
assertthat::assert_that(require(xml2))

repositoryPath <- file.path("C:","Users","Rob","Documents","Repositories")

xml_filenames <- list.files(file.path(repositoryPath, "fry9c", "inst", "extdata"),
                            pattern="[.]xml", full.names = TRUE)
xsd_filename <- file.path(repositoryPath, "fry9c", "inst", "extdata", "FR_Y-9c.xsd")
assertthat::assert_that(file.exists(xsd_filename))

Y <- xml2::read_xml(xsd_filename)
for (i in seq_along(xml_filenames))
{
  cat(paste0("Reading filename: ", xml_filenames[i], "\n"))
  X <- xml2::read_xml(xml_filenames[i])
  assertthat::assert_that(xml2::xml_validate(X, schema = Y))
}
