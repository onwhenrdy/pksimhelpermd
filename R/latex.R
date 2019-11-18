
#' Title
#'
#' @param ...
#' @param add_nl
#'
#' @return
#' @export
#'
#' @examples
latex_block <- function(..., add_nl = TRUE) {

  text <- paste0(...)

  if (add_nl) {
    text <- paste0(text, "\n")
  }

  cat(knitr::raw_latex(text))
}


#' Title
#'
#' @param ...
#' @param add_nl
#' @param env
#'
#' @return
#' @export
#'
#' @examples
latex_test_code <- function(..., add_nl = TRUE, env = parent.frame()) {
  p <- get0("params", envir = env)
  if (length(p) > 0) {
    dis <- p$disable_test_code
    if (length(dis) > 0 && dis)
      return(invisible(p))
  }
  latex_block(..., add_nl = add_nl)
}


#' Title
#'
#' @param ...
#' @param test_code
#' @param env
#'
#' @return
#' @export
#'
#' @examples
latex_graphics_path <- function(..., test_code = FALSE, env = parent.frame()) {
  paths <- list(...)
  if (length(paths) > 0) {
    paths <- lapply(paths, function(x) {
      last <- substring(x, nchar(x))
      if (last != .Platform$file.sep)
        x <- paste0(x, .Platform$file.sep)
      return(x)
    })

    paths <- paste0("{", paths, "}", collapse = "")
    cmd <- paste0("\\graphicspath{", paths, "}")
    if (test_code)
      latex_test_code(cmd, env = env)
    else
      latex_block(cmd)
  }

  invisible(paths)
}


.latex_section_helper <- function(text, cmd,
                                  short_version = NA,
                                  label = NA) {

  if (is.na(short_version))
    text <- paste0(cmd, "{", text, "}")
  else
    text <- paste0(cmd, "[", short_version, "]{", text, "}")

  if (!is.na(label))
    text <- paste0(text, "\\label{", label, "}")

  latex_block(text, add_nl = TRUE)
}


#' Title
#'
#' @param text
#' @param short_version
#' @param label
#'
#' @return
#' @export
#'
#' @examples
latex_section <- function(text, short_version = NA, label = NA) {
  .latex_section_helper(text, "\\section", short_version, label)
}

#' Title
#'
#' @param text
#' @param short_version
#' @param label
#'
#' @return
#' @export
#'
#' @examples
latex_subsection <- function(text, short_version = NA, label = NA) {
  .latex_section_helper(text, "\\subsection", short_version, label)
}

#' Title
#'
#' @param text
#' @param short_version
#' @param label
#'
#' @return
#' @export
#'
#' @examples
latex_subsubsection <- function(text, short_version = NA, label = NA) {
  .latex_section_helper(text, "\\subsubsection", short_version, label)
}

#' Title
#'
#' @param text
#' @param short_version
#' @param label
#'
#' @return
#' @export
#'
#' @examples
latex_paragraph <- function(text, short_version = NA, label = NA) {
  .latex_section_helper(text, "\\paragraph", short_version, label)
}



#' Title
#'
#' @param name
#' @param cmd
#' @param n_arg
#'
#' @return
#' @export
#'
#' @examples
latex_def_cmd <- function(name, cmd, n_arg = 0) {

  text <- paste0("\\providecommand{\\", name, "}")
  if (n_arg > 0)
    text <- paste0(text, "[", n_arg, "]")

  latex_block(paste0(text, "{", cmd, "}"))
}


#' Title
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
latex_bf <- function(str) {
  paste0("\\textbf{", str, "}")
}

#' Title
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
latex_it <- function(str) {
  paste0("\\textit{", str, "}")
}



