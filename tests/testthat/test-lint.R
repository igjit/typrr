context("lint.R")

test_that("source_expressions", {
  lines <- c("a <- 1", "a + 2")
  expressions <- source_expressions(to_file(lines))
  expect_equal(purrr::map_chr(expressions, "lines"), lines)
})

test_that("type_check", {
  expect_null(type_check(to_file(c("a <- 1", "a + 2"))))
  expect_equal(type_check(to_file(c("a <- 1", "a + 'foo'"))),
               list(line = 2, column = 1, message = "type error"))

  expect_null(type_check(to_file(c("add2 <- function(x) x + 2", "add2(40)"))))
  expect_equal(type_check(to_file(c("add2 <- function(x) x + 2", "add2('abc')"))),
               list(line = 2, column = 1, message = "type error"))

  expect_null(type_check(to_file(c("foo <- function(x) 40", "foo(0) + 2"))))
  expect_equal(type_check(to_file(c("foo <- function(x) TRUE", "foo(0) + 2"))),
               list(line = 2, column = 1, message = "type error"))
})
