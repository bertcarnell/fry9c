require(testthat)
testthat::context("Test the component class")

testthat::test_that("test creating components", {
  expect_error(Component(c("1","2"), LETTERS[1], LETTERS[1]))
  expect_error(Component("1", LETTERS[1:2], LETTERS[1]))
  expect_error(Component("2", LETTERS[1], LETTERS[1:2]))
  x <- Component("1.a.", "Income", "ZZZZ1234")
  expect_equal(x$getNum(), "1.a.")
  expect_equal(x$getName(), "Income")
  expect_equal(x$getKey(), "ZZZZ1234")
})

testthat::test_that("test adding components to components", {
  x <- Component("1.a.", "Income", "ZZZZ1234")
  x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  expect_equal(x$getNum(), "1.a.")
  expect_equal(x$getName(), "Income")
  expect_equal(x$getKey(), "ZZZZ1234")
  expect_error(x$add(5))
})

testthat::test_that("test initializing data", {
  x <- Component("1.a.", "Income", "ZZZZ1234")
  x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))
  expect_equal(x$getValue(), 1:4)

  y <- Component("1.a.", "Income", "ZZZZ1234")
  expect_error(y$initializeData(data.frame(AAAA234 = 1:4, ABCD5555 = 5:8)))

  expect_error(y$initializeData(NA))
})

testthat::test_that("test exporting component csv", {
  x <- Component("1.a.", "Income", "ZZZZ1234")
  x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))
  expect_equal(x$export_csv()[1], "1.a., Income, 1,2,3,4")
})

testthat::test_that("test printing a component", {
  x <- Component("1.a.", "Income", "ZZZZ1234")
  x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
  x$initializeData(data.frame(ZZZZ1234 = 1:4,
                              ABCD5555 = 5:8))
  expect_equal(capture_output(print(x)),
               "1.a.\tIncome\t1 ...(4)\n1.a.(1)\tSub-Income\t5 ...(4)")
})

testthat::test_that("test extraction", {
  x <- Component("1.a.", "Income", "ZZZZ1234")
  x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
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

  expect_equal(unlist(x$getAllValues()), 1:8)
  expect_equal(unlist(x$getAllNums()), c("1.a.", "1.a.(1)"))
  expect_equal(unlist(x$getAllNames()), c("Income", "Sub-Income"))

  expect_true(all(is.na(x$getCommonSizeValue())))

  x$commonSize(100)
  expect_equal(x$getCommonSizeValueFromNum("1.a."), (1:4)/100)
  expect_equal(x$getCommonSizeValueFromKey("ZZZZ1234"), (1:4)/100)
  expect_equal(x$getCommonSizeValue(), (1:4)/100)

  y <- x$getComponentFrom("ABCD5555")
  expect_equal(y$getNum(), "1.a.(1)")
})
