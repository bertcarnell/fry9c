require(testthat)
testthat::context("Test the fry9c class")

testthat::test_that("test creating fry9c objects", {
  f <- Fry9c("20180331", "7100-0128",
            "Consolidated Financial Statements for Holding Companies--FR Y-9c")
  expect_error(Fry9c(LETTERS[1:2], LETTERS[1], LETTERS[1]))
  expect_error(Fry9c(LETTERS[1], LETTERS[1:3], LETTERS[1]))
  expect_error(Fry9c(LETTERS[1], LETTERS[1], LETTERS[1:4]))

  expect_equal(f$getDate(), "20180331")
  expect_equal(f$getOmbNumber(), "7100-0128")
  expect_equal(f$getTitle(), "Consolidated Financial Statements for Holding Companies--FR Y-9c")
})

testthat::test_that("test adding schedules to fry9cs", {
  f <- Fry9c("20180331", "7100-0128",
             "Consolidated Financial Statements for Holding Companies--FR Y-9c")
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  f$add(x)
  expect_equal(f$getSchedule("HI")$getComponent(1)$getNum(), "1.a.")

  expect_error(f$add(5))
})

testthat::test_that("test initializing data", {
  f <- Fry9c("20180331", "7100-0128",
             "Consolidated Financial Statements for Holding Companies--FR Y-9c")
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  f$add(x)
  f$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))
  x$addBankNames(paste("bank", LETTERS[1:4], sep=""))

  expect_equal(f$getSchedule("HI")$getComponent(1)$getValue(), 1:4)

  expect_error(f$initializeData(data.frame(AAAA234 = 1:4, ABCD5555 = 5:8)))

  expect_error(f$initializeData(NA))
})

testthat::test_that("test printing a schedule", {
  f <- Fry9c("20180331", "7100-0128",
             "Consolidated Financial Statements for Holding Companies--FR Y-9c")
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  f$add(x)
  f$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  expect_equal(capture_output(print(f)),
               "  Consolidated Financial Statements for Holding Companies--FR Y-9c\n  20180331\n    Schedule HI Income Statement")
})
