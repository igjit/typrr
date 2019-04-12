(require 'flycheck)

(flycheck-define-checker r-typrr
  "An R type checking using typrr package."
  :command ("R" "--slave" "--no-save" "-e"
            "error <- typrr::type_check(commandArgs(TRUE)); if (!is.null(error)) cat(error$line, error$message, sep = ':')"
            "--args" source)
  :error-patterns
  ((error line-start line ":" (message) line-end))
  :modes ess-mode)

(add-to-list 'flycheck-checkers 'r-typrr)
