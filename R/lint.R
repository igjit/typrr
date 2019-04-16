#' @importFrom purrr imap flatten
NULL

#' Type check
#'
#' @param filename file
#' @export
type_check <- function(filename) {
  expressions <- source_expressions(filename)
  equations <- extract_from_expressions(expressions)
  result <- callCC(function(exit) unify(equations, exit))
  if (is(result, "type_error")) {
    expression <- expressions[[result$src_index]]
    list(line = expression$line, column = expression$column, message = "type error")
  }
}

source_expressions <- function(filename) {
  lintr::get_source_expressions(filename)$expressions %>%
    head(-1)
}

extract_from_expressions <- function(expressions) {
  gamma <- type_env()
  vng <- variable_name_generator()

  imap(expressions, function(e, i) {
    expr <- parse(text = e$content)[[1]]
    c(E, t) %<-% extract1(gamma, expr, vng)
    map(E, ~ with_index(., i))
  }) %>% flatten
}
