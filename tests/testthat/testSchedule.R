require(testthat)
testthat::context("Test the schedule class")

testthat::test_that("test creating schedules", {
  expect_error(Schedule(c("HI","HIK"), LETTERS[1]))
  expect_error(Schedule(c("HI"), LETTERS[1:2]))

  x <- Schedule("HI", "Income Statement")
  expect_equal(x$getDesig(), "HI")
  expect_equal(x$getTitle(), "Income Statement")
})

testthat::test_that("test adding components to schedules", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)

  expect_identical(x$getComponent(1), y)
  expect_error(x$add(5))
})

testthat::test_that("test initializing data", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  expect_equal(x$getComponent(1)$getValue(), 1:4)

  expect_error(x$initializeData(data.frame(AAAA234 = 1:4, ABCD5555 = 5:8)))

  expect_error(x$initializeData(NA))

  x$addBankNames(paste("bank", LETTERS[1:4], sep=""))
  expect_equal(x$getBankNames(), paste("bank", LETTERS[1:4], sep=""))
})

testthat::test_that("test exporting schedule csv", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))
  expect_equal(x$export_csv(), c("Schedule HI, Income Statement, ",
                                 "1.a., Income, 1,2,3,4",
                                 "1.a.(1), Sub-Income, 5,6,7,8"))
})

testthat::test_that("test printing a schedule", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  expect_equal(capture_output(print(x)),
               "Schedule HI Income Statement \n1.a.\tIncome\t1 ...(4)\n1.a.(1)\tSub-Income\t5 ...(4)")
})

testthat::test_that("test extraction", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  expect_equal(x$getValueFromKey("ABCD5555"), 5:8)
  expect_equal(x$getValueFromKey("ZZZZ1234"), 1:4)
  expect_equal(x$getValueFromNum("1.a."), 1:4)
  expect_equal(x$getValueFromNum("1.a.(1)"), 5:8)

  expect_equal(x$getNumFromKey("ABCD5555"), "1.a.(1)")
  expect_equal(x$getNumFromKey("ZZZZ1234"), "1.a.")
  expect_equal(x$getKeyFromNum("1.a."), "ZZZZ1234")
  expect_equal(x$getKeyFromNum("1.a.(1)"), "ABCD5555")

  x$commonSize(100)
  expect_equal(x$getCommonSizeValueFromNum("1.a."), (1:4)/100)
  expect_equal(x$getCommonSizeValueFromKey("ZZZZ1234"), (1:4)/100)

  y <- x$getComponentFrom("ABCD5555")
  expect_equal(y$getNum(), "1.a.(1)")
})

testthat::test_that("test summing", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  expect_equal(x$sumLevels(c("1.a.", "1.a.(1)"), NA), 1:4 + 5:8)
  expect_equal(x$sumLevels("1.a.", "1.a.(1)"), 1:4 - 5:8)
})

testthat::test_that("test data.frame", {
  x <- Schedule("HI", "Income Statement")
  y <- Component("1.a.", "Income", "ZZZZ1234")
  y$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$add(y)
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))

  x$addBankNames(paste("bank", LETTERS[1:4], sep=""))
  expect_equal(x$createDataFrame()$bankA, c(1, 5))
})
