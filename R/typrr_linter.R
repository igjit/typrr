#' @export
typrr_linter <- function() {
  read <- list()

  function(source_file) {
    if (is.null(read[[source_file$filename]]) && !is_termination(source_file)) {
      read[[source_file$filename]] <<- TRUE
      error <- type_check(source_file$filename)
      if (!is.null(error)) {
        lintr::Lint(filename = source_file$filename,
                    line_number = error$line,
                    column_number = error$column,
                    type = "error",
                    message = error$message,
                    linter = "typrr_linter")
      }
    }
  }
}

is_termination <- function(source_file) is.null(source_file$line)
