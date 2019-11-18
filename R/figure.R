

.caption_code <- function(caption, label = NA, double_bs = FALSE) {

  cap_text <- if (length(caption) == 0 || is.na(caption)) "" else caption

  bs <- if (double_bs) "\\\\" else ""

  if (length(label) == 0 || is.na(label))
   text <- paste0("\\caption{", cap_text ,"}", bs, "\n")
  else
   text <-  paste0("\\caption{\\label{", label, "}", cap_text ,"}", bs, "\n")

  return(text)
}

.subfig_code <- function(file, width = 1.0,
                         caption = "",
                         label = NA,
                         caption_pos = c("top", "bottom")) {

  file_name <- tools::file_path_sans_ext(file) # excluding dot
  file_ext <- tools::file_ext(file) # including dot
  caption_pos <- match.arg(caption_pos)

  text <- paste0("\\begin{subfigure}[b]{", width, "\\linewidth} \n")
  if (caption_pos == "top")
    text <- paste0(text, .caption_code(caption, label))

  text <- paste0(text, "\\includegraphics[width=\\linewidth]{{",
                 shQuote(file_name), "}.", file_ext, "}\n")

  if (caption_pos == "bottom")
    text <- paste0(text, .caption_code(caption, label))

  paste0(text, "\\end{subfigure}%\n")
}

.fig_code <- function(file, width = 1.0) {

  file_name <- tools::file_path_sans_ext(file) # excluding dot
  file_ext <- tools::file_ext(file) # including dot
  paste0("\\includegraphics[width=", width, "\\linewidth]{{",
         shQuote(file_name), "}.", file_ext, "}\n")
}


#' Title
#'
#' @param files
#' @param pics_per_row
#' @param max_rows_per_fig
#' @param fig_caption
#' @param fig_label
#' @param file_captions
#' @param file_labels
#' @param subcaption_pos
#' @param fig_scale
#' @param fig_fixed_width
#' @param clearpage
#' @param centering
#' @param continued_text
#' @param smart_layout
#' @param add_env_code
#' @param last_figs_left_aligned
#'
#' @return
#' @export
#'
#' @examples
latex_figure <- function(files,
                         pics_per_row,
                         max_rows_per_fig,
                         fig_caption = NA,
                         fig_label = NA,
                         file_captions = NULL,
                         file_labels = NULL,
                         subcaption_pos = c("top", "bottom"),
                         fig_scale = 1,
                         fig_fixed_width = NA,
                         clearpage = TRUE,
                         centering = TRUE,
                         continued_text = "(continued)",
                         add_env_code = NA,
                         last_figs_left_aligned = TRUE,
                         smart_layout = TRUE) {

  subcaption_pos <- match.arg(subcaption_pos)
  file_captions <- as.character(file_captions)
  file_labels <- if (!is.function(file_labels)) as.character(file_labels) else file_labels

  files <- as.character(files)

  n_files <- length(files)
  n_rows <- ceiling(n_files / pics_per_row)
  n_figs <- ceiling(n_rows / max_rows_per_fig)
  max_sub_figs <- max_rows_per_fig * pics_per_row

  if (n_files == 0)
    return()

  # width of a subfigure
  sub_with <- if (!is.na(fig_fixed_width)) fig_fixed_width else fig_scale/pics_per_row

  if (smart_layout && n_files == pics_per_row + 1) {
    latex_figure(files,
                 pics_per_row = pics_per_row - 1,
                 max_rows_per_fig,
                 fig_caption,
                 fig_label,
                 file_captions,
                 file_labels,
                 subcaption_pos,
                 fig_scale = fig_scale * (pics_per_row - 1) / pics_per_row,
                 fig_fixed_width,
                 clearpage,
                 centering,
                 continued_text,
                 add_env_code,
                 smart_layout = FALSE)
    return()
  }

  start_idx <- 1
  for (fig in 1:n_figs) {
    latex_block("\\begin{figure}[!ht]")
    if (centering)
      latex_block("\\centering")

    if (fig > 1)
      latex_block("\\ContinuedFloat")

    if (n_files > length(letters))
      latex_block("\\renewcommand*{\\thesubfigure}{\\alphalph{\\value{subfigure}}}")

    if (!is.na(add_env_code))
      latex_block(add_env_code)

    fig_code <- ""
    pic_counter <- 0
    for (i in start_idx:n_files) {
      file <- files[i]

      if (n_files > 1) {
        fig_code <- paste0(fig_code, .subfig_code(file, sub_with,
                                                  caption = file_captions[i],
                                                  label = if (is.function(file_labels))
                                                            file_labels(i)
                                                          else
                                                            file_labels[i],
                                                  caption_pos = subcaption_pos))
      } else {
        fig_code <- paste0(fig_code, .fig_code(file, sub_with))
      }

      if ((i %% pics_per_row) == 0)
        fig_code <- paste(fig_code, "\n")

      pic_counter <- pic_counter + 1
      start_idx <- i + 1
      if (i >= (fig * max_sub_figs))
        break
    }

    # last figure left aligned only for at least two-row figures
    if (last_figs_left_aligned && pic_counter > pics_per_row) {
      fig_rest <- (start_idx - 1) %% pics_per_row
      if (fig_rest > 0) {
        fig_rest <- pics_per_row - fig_rest
        for (d in 1:fig_rest) {
         tmp <- paste0("\\begin{subfigure}[b]{", sub_with, "\\linewidth}~ \\end{subfigure}%\n")
         fig_code <- paste0(fig_code, tmp)
        }
      }
    }

    # figure code cat
    latex_block(fig_code)


    # caption code cat
    cont_text <- if (fig > 1) paste0(" ", continued_text) else ""
    caption_text <- if (is.na(fig_caption)) cont_text else paste0(fig_caption, cont_text)
    label <- if (fig == 1) fig_label else NA
    latex_block(.caption_code(caption_text, label))

    latex_block("\\end{figure}\n")

    # clearpage cat
    if (clearpage) {
      latex_block("\\vspace*{\\fill}%\n")
      latex_block("\\clearpage\n")
    }
  }
}

