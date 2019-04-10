context("typrr.R")

test_that("variable_name_generator", {
  gen <- variable_name_generator()
  tv <- c(gen(), gen(), gen())
  expect_equal(tv, c("'a", "'b", "'c"))
})

test_that("extract1", {
  expect_is(extract1(type_env(), quote(1))[[2]], "double")

  te <- type_env()
  te[["a"]] <- type("integer")
  expect_is(extract1(te, quote(a))[[2]], "integer")

  te <- type_env()
  extract1(te, quote(a <- 1))
  expect_is(te$a, "double")
})

test_that("substitute_type1", {
  expect_equal(substitute_type1(type_variable("'a"), type("double"), type_variable("'a")), type("double"))
  expect_equal(substitute_type1(type_variable("'a"), type("double"), type_variable("'b")), type_variable("'b"))
  expect_equal(substitute_type1(type_variable("'a"), type("double"), function_type(type_variable("'a"), type("logical"))),
               function_type(type("double"), type("logical")))
})

test_that("PT", {
  expect_is(PT(parse(text = "1.2")), "double")
  expect_is(PT(parse(text = "a <- function(x) x + 2; a(1)")), "double")
  expect_is(PT(parse(text = "a <- function(x) 'abc'; a(1)")), "character")
  expect_is(PT(parse(text = "a <- function(x) x; a(1); a(1)")), "double")
})
