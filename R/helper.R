

.extended_letters <- function(alphabet) function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }
  vapply(i - 1L, base10toA, character(1L), alphabet)
}


#' Title
#'
#' @param prefix
#' @param type
#' @param sep
#' @param counter
#'
#' @return
#' @export
#'
#' @examples
latex_labelmaker <- function(prefix,
                             type = c("fig", "subfig", "tab"),
                             sep = "_",
                             counter = c("num", "letter")) {

  type <- match.arg(type)
  counter <- match.arg(counter)

  res <- function(x) {


    if (counter == "num")
      value = x
    else
      fn <- .extended_letters(letters)
      value <- fn(x)

    paste0(type, ":", prefix, sep, value)
  }

  return(res)
}

#' Title
#'
#' @param file
#' @param sub
#'
#' @return
#' @export
#'
#' @examples
sanatize_filename <- function(file, sub = "X") {
  file <- gsub(" +", "", file)
  file <- iconv(file, "UTF-8", "ASCII", sub = sub)
  file <- trimws(file)

  return(file)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extract_numbers_from_str <- function(x) {
  pat <- "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
  m <- gregexpr(pat, x)
  x <- lapply(regmatches(x,m),
              function(X) {
                as.numeric(X)
              })
  unlist(x)
}


#' Title
#'
#' @param json_filename
#' @param payload
#' @param pretty
#'
#' @return
#' @export
#'
#' @examples
dump_pksh <- function(json_filename, payload, pretty = TRUE) {

  out <- file(json_filename, 'w', encoding = "UTF-8")
  write(jsonlite::serializeJSON(payload, pretty = pretty), out)
  close(out)
}


#' Title
#'
#' @param json_filename
#'
#' @return
#' @export
#'
#' @examples
read_pksh <- function(json_filename) {

  locale <- readr::default_locale()
  locale$encoding <- "UTF-8"

  json_data <- readr::read_file(json_filename, locale = locale)
  md_assist <- jsonlite::unserializeJSON(json_data)
  return(md_assist)
}



#' Title
#'
#' @param input_file
#' @param rmd.params
#' @param tex_dir
#' @param clean
#' @param quiet
#'
#' @return
#' @export
#'
#' @examples
rmd_to_latex <- function(input_file,
                         rmd.params = NULL,
                         tex_dir = NULL,
                         clean = FALSE,
                         quiet = FALSE) {

  if (!file.exists(input_file))
    stop("Rmd file does not exist")

  work.dir <- dirname(input_file)
  file.name <- basename(input_file)
  md.file <- paste0(file.name, ".md")

  env <- new.env()
  env$params <- rmd.params
  env$params[["disable_test_code"]] <- TRUE
  # this will produce
  # 1. work.dir/basename.md
  md.file <- knitr::knit(input = input_file,
                         encoding = "UTF-8",
                         output = file.path(work.dir, md.file),
                         envir = env,
                         quiet = quiet)

  # use the basename.knit.md file to produce a tex
  base_name <- tools::file_path_sans_ext(file.name)
  tex_file <- file.path(work.dir, paste0(base_name, ".tex"))

  rmarkdown::pandoc_convert(md.file,
                            to = "latex",
                            output = tex_file)

  if (clean) {
    file.remove(md.file)
  }

  if (length(tex_dir) > 0) {

    ok <- file.copy(tex_file, tex_dir, copy.date = TRUE, overwrite = TRUE)
    if (ok && clean)
      file.remove(tex_file)

    if (!ok)
      return(NULL)

    tex_file <- file.path(tex_dir, basename(tex_file))
  }

  return(tex_file)
}


