

.alignment_string <- function(df) {
  col.types <- unlist(lapply(df, class))

  result <- c()
  for (type in col.types) {
    if (type == "numeric")
      result <- c(result, "r")
    else
      result <- c(result, "l")
  }

  return(paste0(result, collapse = ""))
}

.validate.alignment <- function(df, str) {

  if (ncol(df) != nchar(str))
    stop("Alignment must have the same length as the number of columns of the data.frame")

    if (!grepl("^[lrc]+$", str, perl = T))
      stop("Alignment strings must be 'l', 'r' or 'c'")
}


.table_landscape <- function(tab_str) {
  paste0("\\begin{landscape}\n", tab_str, "\\end{landscape}\n")
}

.table_font <- function(size, tab_str) {
  size_2 <- size + 2

  paste0("\\begingroup\\fontsize{", size, "}{", size_2, "}\\selectfont\n", tab_str, "\\endgroup{}\n")
}

.table_env <- function(df, tab_str, alignment = NULL) {

  # alignment
  alg.string <- alignment
  if (length(alignment) == 0)
    alg.string <- .alignment_string(df)
  .validate.alignment(df, alg.string)

  paste0("\\begin{longtable}[h]{", paste0(alg.string, collapse = ""), "}\n", tab_str, "\\end{longtable}\n")
}


.table_row <- function(row, bf = FALSE, last_row = FALSE) {

  if (bf)
    row <- unlist(lapply(row, latex_bf))

  text <- paste(paste(row, collapse = " & "), "\\\\\n")

  if (last_row)
    text <- paste0(text, "*")

  return(text)
}

.top_header <- function(names, df, bf = FALSE) {

  if (length(names) == 0)
    return("")

  h_text <- names(names)
  names <- as.integer(names)

  names[is.na(names)] <- 1
  if (sum(names) != ncol(df))
    stop("Top header must have the same size as columns in the data.frame")

  text <- c()
  for (i in 1:length(names)) {
    t_tmp <- if (bf) latex_bf(h_text[i]) else h_text[i]
    i_tmp <- names[i]
    text <- c(text, paste0("\\multicolumn{", i_tmp, "}{c}{", t_tmp, "}"))
  }

  row <- .table_row(text, FALSE, FALSE)

  i <- 0
  for (s in names) {
    if (s > 1)
      row <- paste0(row, "\\cmidrule(l{3pt}r{3pt}){", i + 1, "-", i + s, "}")
    i <- i + s
  }

  row <- paste0(row, "\n")
  return(row)
}

.table_header <- function(df, bold_header,
                          caption, label,
                          caption_cont_label = latex_it("(continued)"),
                          top_row,
                          footer = NULL,
                          header_formater = NULL) {

  head <- colnames(df)
  if (!is.null(header_formater))
    head <- header_formater(head)

  header_row <-  .table_row(head, bold_header)
  top_header <- .top_header(top_row, df, bold_header)

  # first caption
  lab <- if (is.na(label)) NA else paste0("tab:", label)
  text <- .caption_code(caption, lab, TRUE)

  text <- paste0(text, "\\toprule\n")
  # first header
  text <- paste(text, top_header)
  text <- paste(text, header_row)
  text <- paste0(text, "\\midrule\n")
  text <- paste0(text, "\\endfirsthead\n")

  #second caption
  text <- paste0(text, .caption_code(paste(caption, caption_cont_label), NA, TRUE))
  text <- paste0(text, "\\toprule\n")
  # second header
  text <- paste(text, top_header)
  text <- paste(text, header_row)
  text <- paste0(text, "\\midrule\n")
  text <- paste0(text, "\\endhead\n")
  text <- paste0(text, "\\bottomrule\n")

  if (length(footer$lines) > 0) {
    footer_text <- paste0("\\multicolumn{", ncol(df), "}{l}{", footer$lines, "}\\\\\n", collapse = "")
    text <- paste0(text, footer_text)
  }

  text <- paste0(text, "\\endfoot\n")
  text <- paste0(text, "\\addlinespace[0.3em]\n")
  return(text)
}


#' Title
#'
#' @param general
#' @param alpha
#' @param number
#' @param order
#' @param general_title
#' @param number_title
#' @param alpha_title
#' @param font_size
#'
#' @return
#' @export
#'
#' @examples
make_table_footer <- function(general = NULL,
                              alpha = NULL,
                              number = NULL,
                              order = c("general", "number", "alpha"),
                              general_title = NA,
                              number_title = NA,
                              alpha_title = NA,
                              font_size = NULL) {
  lines <- c()
  for (type in order) {
    title <- NA
    var_lines <- c()

    if (type == "general") {
      title <- general_title
      var_lines <- general
    } else if (type == "number") {
      title <- number_title
      if (length(number) > 0)
        var_lines <- paste0("\\textsuperscript{" , seq(1,length(number)), "}~", number)

    } else if (type == "alpha") {
      title <- alpha_title
      if (length(alpha) > 0)
        var_lines <- paste0("\\textsuperscript{" , letters[1:length(alpha)], "}~", alpha)
    }

    if (length(var_lines) > 0) {
      if (!is.na(title))
        lines <- c(lines, title)
        lines <- c(lines, var_lines)
    }
  }
  res <- list(lines = lines, font_size = font_size)
  class(res) <- "footnote"
  return(res)
}

.specify_decimal <- function(x, k, na = "", scientific_at = 10000) {
  x <- unlist(sapply(x, function(x) {
    x <- as.numeric(x)
    if (is.na(x))
      return(na)

    old_x <- x
    rounded_x <- round(as.numeric(x), k)
    if (rounded_x == 0 && old_x != 0)
      rounded_x <- scales::scientific(old_x, digits = k + 1)
    else {
      if (length(scientific_at) > 0 && rounded_x > scientific_at)
        rounded_x <- scales::scientific(old_x, digits = k + 1)
      else
        rounded_x <- trimws(format(rounded_x, nsmall = k, na.encode = FALSE))
    }
    return(rounded_x)
  }))

  formated <- gsub("NA", na, x)
  formated <- gsub("e", "E", x)
  return(formated)
}

.round_in_strings <- function(x, digits, na = "", scientific_at = 10000) {
  x[is.na(x)] <- na
  pat <- "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
  m <- gregexpr(pat, x)
  regmatches(x,m) <- lapply(regmatches(x,m),
                            function(X) {
                                .specify_decimal(X, digits, na)
                            })
  x
}

#' Title
#'
#' @param fixed_digits
#' @param format_str_numbers
#' @param sub_super_convert
#' @param na
#'
#' @return
#' @export
#'
#' @examples
column_formater <- function(fixed_digits = 2,
                            scientific_at = 10000,
                            format_str_numbers = TRUE,
                            sub_super_convert = TRUE,
                            line_br_to_cell = TRUE,
                            latex_converts = TRUE,
                            na = "") {

  f <- function(x) {
    must_format <- (length(fixed_digits) == 1 && !is.na(fixed_digits))
    if (is.numeric(x)) {
      if (must_format)
        x <- .specify_decimal(x, fixed_digits, na, scientific_at)
      else
        x <- format(x)
    }
    else {
      if (format_str_numbers && must_format) {
        x <- .round_in_strings(x, fixed_digits, na, scientific_at)
      }
    }

    # NA
    x[is.na(x)] <- na

    # Sup Super
    if (sub_super_convert) {
      x <- gsub("\\_", "\\\\textsubscript", x)
      x <- gsub("\\^", "\\\\textsuperscript", x)
    }

    # LB
    if (line_br_to_cell) {
      break_idx <- grep("\\\\n", x)
      for (idx in break_idx) {
        x[idx] <- gsub("\\\\n", "\\\\\\\\", x[idx])
        x[idx] <- paste0("\\makecell[tl]{", x[idx], "}")
      }
    }

    # latex converts
    if (latex_converts) {
      x <- gsub("([#$%&])", "\\\\\\1", x)
      x <- gsub("±", "\\$\\\\pm\\$", x)
      x <- gsub("~", "\\\\textasciitilde{}", x)
    }

    return(x)
  }
  return(f)
}


.table_data <- function(df, formater) {
  if (is.function(formater)) {
    df <- as.data.frame(apply(df, 2, formater))
  } else if (is.list(formater)) {
    if (length(formater) != ncol(df))
      stop("Formater list must have the same length as columns in data.frame")

    for (i in 1:ncol(df)) {
      df[[i]] <- formater[[i]](df[[i]])
    }
  }

  text <- apply(df, 1, .table_row)
  text <- paste0(text, collapse = "")
  return(text)
}

#' Title
#'
#' @param df
#' @param caption
#' @param label
#' @param alignment
#' @param centering
#' @param landscape
#' @param font_size
#' @param write
#'
#' @return
#' @export
#'
#' @examples
latex_table <- function(df,
                        caption = NA,
                        caption_cont_label = latex_it("(continued)"),
                        label = NA,
                        alignment = NULL,
                        landscape = FALSE,
                        font_size = NA,
                        bf_header = TRUE,
                        header_formater = column_formater(format_str_numbers = FALSE),
                        top_row = NULL,
                        footer = NULL,
                        col_formater = column_formater(),
                        write = c("cat", "str", "clipr")) {

  write <- match.arg(write)

  #### Parts
  # header
  tab <- .table_header(df, bf_header,
                       caption,
                       label,
                       caption_cont_label,
                       top_row,
                       footer,
                       header_formater)

  # data
  tab <- paste0(tab, .table_data(df, col_formater))

  #### wrapping code
  # outer
  tab <- .table_env(df, tab, alignment)

  # font
  if (!is.na(font_size))
    tab <- .table_font(font_size, tab)

  # landscape
  if (landscape)
    tab <- .table_landscape(tab)

  # output (latex, string or clipboard)
  if (write == "cat")
    latex_block(tab)
  else if (write == "str")
    return(tab)
  else if (write == "clipr") {
    clipr::write_clip(tab)
    message("Table written to clipboard")
  }
}

