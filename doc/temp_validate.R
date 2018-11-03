require(R6)
require(xml2)
require(assertthat)
require(openxlsx)

assertthat::assert_that(
  grepl(.num_pattern, "4.") &
    grepl(.num_pattern, "14.") &
    grepl(.num_pattern, "140.") &
    grepl(.num_pattern, "4.a.") &
    grepl(.num_pattern, "4.a.(1)") &
    grepl(.num_pattern, "4.a.(2)(a)") &
    !grepl(.num_pattern, "0.") &
    !grepl(.num_pattern, "4a.") &
    !grepl(.num_pattern, "4.a.z")
)

assertthat::assert_that(component$new("A", "B", "C")$getKey() == "C")
assertthat::assert_that(component$new("A", "B", "C")$getNum() == "A")

validate_HI <- function(schedHI)
{
  assertthat::assert_that(all.equal(class(schedHI), c("schedule", "R6")))

  assertthat::assert_that(all(is.na(schedHI$getValueFromNum("1.a."))))
  assertthat::assert_that(all.equal(schedHI$getValueFromNum("1.e."),
                                    schedHI$getValueFromKey("BHCK4069")))

  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("1.h."),
    schedHI$sumLevels(c("1.a.(1)(a)", "1.a.(1)(b)", "1.a.(1)(c)", "1.a.(2)", "1.b.", "1.c.",
                        "1.d.(1)", "1.d.(2)", "1.d.(3)", "1.e.", "1.f.", "1.g."), NA)
  ), msg = paste("A = ", paste(schedHI$getValueFromNum("1.h."), collapse = ","),
                 "B = ", paste(schedHI$sumLevels(c("1.a.(1)(a)", "1.a.(1)(b)", "1.a.(1)(c)", "1.a.(2)", "1.b.", "1.c.",
                                                   "1.d.(1)", "1.d.(2)", "1.d.(3)", "1.e.", "1.f.", "1.g."), NA), collapse = ",")))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("2.f."),
    schedHI$sumLevels(c("2.a.(1)(a)", "2.a.(1)(b)", "2.a.(1)(c)", "2.a.(2)", "2.b.", "2.c.",
                        "2.d.", "2.e."), NA)
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("3."),
    schedHI$getValueFromNum("1.h.") - schedHI$getValueFromNum("2.f.")
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("5.m."),
    schedHI$sumLevels(c("5.a.", "5.b.", "5.c.", paste0("5.d.(", 1:5, ")"), paste0("5.", letters[5:12], ".")), NA)
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("7.e."),
    schedHI$sumLevels(c("7.a.", "7.b.", "7.c.(1)", "7.c.(2)", "7.d."), NA)
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("8.a."),
    schedHI$sumLevels(c("3.", "5.m.", "6.a.", "6.b."), c("4.", "7.e."))
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("8.c."),
    schedHI$sumLevels(c("8.a.", "8.b."), NA)
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("10."),
    schedHI$getValueFromNum("8.c.") - schedHI$getValueFromNum("9.")
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("12."),
    schedHI$sumLevels(c("10.", "11."), NA)
  ))
  assertthat::assert_that(all.equal(
    schedHI$getValueFromNum("14."),
    schedHI$getValueFromNum("12.") - schedHI$getValueFromNum("13.")
  ))
}

validate_HC <- function(schedHC)
{
  assertthat::assert_that(all.equal(class(schedHC), c("schedule", "R6")))

  assertthat::assert_that(all.equal(
    schedHC$getValueFromNum("4.b.") - schedHC$getValueFromNum("4.c."),
    schedHC$getValueFromNum("4.d.")
  ))
  assertthat::assert_that(all.equal(
    schedHC$sumLevels(c("1.a.", "1.b.(1)", "1.b.(2)", paste0("2.", letters[1:3], "."),
                        "3.a.", "3.b.",
                        "4.a.", "4.d.",
                        paste0(5:9, "."), "10.a.", "10.b.", "11."), NA),  # 4.b and 4.c are in 4.d
    schedHC$getValueFromNum("12.")
  ))
  assertthat::assert_that(all.equal(
    schedHC$sumLevels(c("13.a.(1)", "13.a.(2)", "13.b.(1)", "13.b.(2)","14.a.", "14.b.",
                        "15.", "16.", "19.a.", "19.b.", "20."), NA),
    schedHC$getValueFromNum("21.")
  ))
  assertthat::assert_that(all.equal(
    schedHC$sumLevels(c("23.", "24.", "25.", paste0("26.", letters[1:3], ".")), NA),
    schedHC$getValueFromNum("27.a.")
  ))
  assertthat::assert_that(all.equal(
    schedHC$sumLevels(c("27.a.", "27.b."), NA),
    schedHC$getValueFromNum("28.")
  ))
  assertthat::assert_that(all.equal(
    schedHC$sumLevels(c("21.", "28."), NA),
    schedHC$getValueFromNum("29.")
  ))
}

validate_HI(fry9cs$get_fry9c(2018, 1)$getSchedule("HI"))
validate_HC(fry9cs$get_fry9c(2018, 1)$getSchedule("HC"))
