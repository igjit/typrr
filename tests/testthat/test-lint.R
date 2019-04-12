context("lint.R")

to_file <- function (lines) {
  file <- tempfile()
  writeLines(lines, file)
  file
}

test_that("source_expressions", {
  lines <- c("a <- 1", "a + 2")
  expressions <- source_expressions(to_file(lines))
  expect_equal(purrr::map_chr(expressions, "lines"), lines)
})

test_that("type_check", {
  expect_null(type_check(to_file(c("a <- 1", "a + 2"))))
  expect_equal(type_check(to_file(c("a <- 1", "a + 'foo'"))),
               list(line = 2, column = 1, message = "type error"))
})
