require(testthat)
testthat::context("Test the fry9c_group class")

fry9c_data_list <- list(
  read.csv(system.file(file.path("extdata", "ex_BHCF1712.csv"), package = "fry9c")),
  read.csv(system.file(file.path("extdata", "ex_BHCF1812.csv"), package = "fry9c")))

testthat::test_that("test creating fry9c_group objects", {
  my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
                                 quarters = c(4, 4))
  expect_error(Fry9c_group(1:2, 3))
  expect_error(Fry9c_group(1, 2:3))

  expect_equal(length(my_fry9c_group), 2)
})

testthat::test_that("test parsing fry9cs", {
  my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
                               quarters = c(4, 4))
  my_fry9c_group$parse_fry9c(
    system.file(file.path("extdata", c("FR_Y-9C20171231.xml", "FR_Y-9C20161231.xml")),
                package = "fry9c"))
  expect_equal(length(my_fry9c_group), 2)

  expect_equal(my_fry9c_group$get_fry9c(2017, 4)$getSchedule("HI")$getKeyFromNum("8."),
               c(key = "BHCK4301"))
  class(my_fry9c_group$get_fry9c(2016, 4))[1] == "fry9c"
})

testthat::test_that("test initializing data", {
  my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
                                quarters = c(4, 4))
  my_fry9c_group$parse_fry9c(
    system.file(file.path("extdata", c("FR_Y-9C20171231.xml", "FR_Y-9C20161231.xml")),
                package = "fry9c"))
  my_fry9c_group$initializeData(fry9c_data_list, paste("bank", LETTERS[1:10], sep=""))

  my_fry9c_group$commonSize("HC-K", "BHCK3368", "HI")

  expect_equal(nrow(my_fry9c_group$get_plot_data("HC-K", "BHCK3368")), 20)
})

testthat::test_that("test printing a fry9c_group", {
  my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
                                quarters = c(4, 4))
  expect_equal(capture_output(print(my_fry9c_group)), "\tyears =  2017, 2016 \n\tquarters =  4, 4 ")
})
