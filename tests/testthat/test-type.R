context("type.R")

test_that("as.character.type", {
  expect_equal(as.character(type("integer")), "integer")
  expect_equal(as.character(type_of(1.2)), "double")
})

test_that("as.character.function_type", {
  ft <- function_type(type("double"), type("double"))
  expect_equal(as.character(ft), "double -> double")
})

test_that("eq.type", {
  expect_true(eq(type("double"), type("double")))
  expect_false(eq(type("double"), type("integer")))
  expect_false(eq(type("double"), type_variable("'a")))
})

test_that("eq.type_variable", {
  expect_true(eq(type_variable("'a"), type_variable("'a")))
  expect_false(eq(type_variable("'a"), type_variable("'b")))
  expect_false(eq(type_variable("'a"), type("double")))
})

test_that("eq.function_type", {
  expect_true(eq(function_type(type("double"), type("logical")), function_type(type("double"), type("logical"))))
  expect_false(eq(function_type(type("double"), type("logical")), function_type(type("double"), type("double"))))
  expect_false(eq(function_type(type("double"), type("logical")), type("double")))
})
