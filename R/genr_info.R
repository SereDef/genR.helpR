#' Print an overview of the dataset
#'
#' Prints a table to console with info about all the column names, labels and
#' types (e.g., factor, numeric...).
#'
#' @param df A dataframe (optionally labelled)
#' @param column_subset A character vector with column names or a regular
#'  expression to match columns in the dataset.
#'
#' @export
#'
data_overview <- function(df, column_subset = NULL) {
  stopifnot(is.data.frame(df))

  # --- subset column if required  --------------------------------------------

  if (!is.null(column_subset)) {

    if (is.character(column_subset) && length(column_subset) == 1L) {
      # treat as regex pattern on colnames
      cols_to_keep <- grep(column_subset, names(df), value = TRUE)
    } else {
      # treat as explicit columns (names or positions)
      cols_to_keep <- intersect(column_subset, names(df))
    }

    if (length(cols_to_keep) < 1) stop('None or the specified columns were found in data.')

    df <- df[, cols_to_keep, drop = FALSE]
  }

  # --- extract info ----------------------------------------------------------
  col_names   <- names(df)
  col_classes <- vapply(df, function(x) paste(class(x), collapse = "/"),
                        character(1L))
  col_labels  <- vapply(df, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) " " else as.character(lab)
  }, character(1L))

  # --- color mapping for data types -------------------------------------------

  col_classes_color <- vapply(col_classes, function(col_class) {
    switch(
      col_class,
      factor    = cli::col_red(col_class),
      character = cli::col_cyan(col_class),
      numeric   = cli::col_green(col_class),
      integer   = cli::col_blue(col_class),
      logical   = cli::col_yellow(col_class),
      cli::col_grey(col_class)
    )
  }, character(1L))

  # --- layout widths ---------------------------------------------------------
  width <- cli::console_width()

  idx_w   <- nchar(as.character(length(col_names))) + 1L
  name_w  <- max(max(nchar(col_names)), 25L) + 1L
  class_w <- max(max(nchar(col_classes)), 9L) + 1L

  # remaining width for labels (now between name and class)
  lab_w <- max(20L, width - idx_w - name_w - class_w - 3L)

  # --- wrap labels -----------------------------------------------------------
  wrap_label <- function(lab) {
    if (lab == "") return(" ")
    paste(cli::ansi_strwrap(lab, width = lab_w), collapse = "\n")
  }
  col_labels_wrapped <- vapply(col_labels, wrap_label, character(1L))

  # --- helper for formatting one row ----------------------------------------
  fmt_row <- function(i) {
    idx   <- format(i, width = idx_w, justify = "right")
    name  <- format(col_names[i],  width = name_w,  justify = "left")
    class <- format(col_classes_color[i], width = class_w, justify = "left")

    lab   <- col_labels_wrapped[i]

    if (!nzchar(lab)) { paste(idx, name, "", class, sep = " ")
    } else {
      pieces <- strsplit(lab, "\n", fixed = TRUE)[[1]]

      first  <- paste(idx, name,
                      format(pieces[1], width = lab_w, justify = "left"),
                      class, sep = " ")

      if (length(pieces) == 1L) {
        first
      } else {
        # continuation lines: only label, aligned under label column
        pad <- paste0(strrep(" ", idx_w + 1L), strrep(" ", name_w + 1L))
        # for continuation, keep an empty class column area at the end
        class_pad <- format("", width = class_w, justify = "left")

        cont <- paste0(
          pad,
          vapply(
            pieces[-1L],
            function(x) format(x, width = lab_w, justify = "left"),
            character(1L)
          ),
          " ",
          class_pad
        )
        paste(c(first, cont), collapse = "\n")
      }
    }
  }

  # --- header ----------------------------------------------------------------
  header <- paste(
    format("#",      width = idx_w,   justify = "right"),
    format("name",   width = name_w,  justify = "left"),
    format("label",  width = lab_w,   justify = "left"),
    format("class",  width = class_w, justify = "left"),
    sep = " "
  )

  sep_line <- strrep("-", width)

  rows <- vapply(seq_along(col_names), fmt_row, character(1L))

  cli::cat_line(header)
  cli::cat_line(sep_line)
  cli::cat_line(rows)

  df_summary <- data.frame(
    col   = seq_along(col_names),
    name  = col_names,
    label = col_labels,
    class = col_classes,
    row.names   = NULL,
    check.names = FALSE
  )
  invisible(df_summary)
}
