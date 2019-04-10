type <- function(name) {
  structure(list(name = name), class = c(name, "base_type", "type"))
}

type_of <- function(x) {
  name <- typeof(x)
  structure(list(name = name, value = x), class = c(name, "base_type", "type"))
}

function_type <- function(from, to) {
  structure(list(name = "function", from = from, to = to), class = c("function_type", "type"))
}

type_variable <- function(name) {
  structure(list(name = as.character(name)), class = c("type_variable", "type"))
}

as.character.type <- function(x, ...) x$name

as.character.function_type <- function(x, ...) paste(x$from, "->", x$to)

eq <- function(e1, e2) UseMethod("eq")

eq.type <- function (e1, e2) {
  identical(class(e1), class(e2))
}

eq.type_variable <- function (e1, e2) {
  is(e2, "type_variable") && e1$name == e2$name
}

eq.function_type <- function (e1, e2) {
  is(e2, "function_type") && eq(e1$from, e2$from) && eq(e1$to, e2$to)
}
