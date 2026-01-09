#' Set outliers to NA
#'
#' @export
#'
remove_outliers <- function(var_name, data, thresh = 3) {

  message(var_name)

  x <- data[[var_name]]

  qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- qnt[2] - qnt[1]
  lower <- qnt[1] - thresh * iqr
  upper <- qnt[2] + thresh * iqr

  # Winsorize
  # min_non_outlier <- min(x[x >= lower], na.rm = TRUE)
  # max_non_outlier <- max(x[x <= upper], na.rm = TRUE
  n_removed_low <- sum(x < lower, na.rm=TRUE)
  n_removed_upp <- sum(x > upper, na.rm=TRUE)

  message('* ', n_removed_low + n_removed_upp, ' observations set to NA (',
          n_removed_low, ' outside lower and ', n_removed_upp, ' outside upper bound).')

  x[x < lower] <- NA # min_non_outlier
  x[x > upper] <- NA # max_non_outlier

  return(as.numeric(x))
}

#' Transform all factors-looking things to factors
#'
#' Takes a dataframe and transforms all variables with less than `min_levels`
#' unique values to factors. Preserved variable labels if there are any.
#'
#' @param df A dataframe (optionally labelled)
#' @param min_levels Minimum number of unique values, above which the variable
#'   is not treated as a factor. Default = 10.
#'
#' @export
#'
factorize <- function(df, min_levels = 10) {
  stopifnot(is.data.frame(df))

  # Preserve variable labels
  labs <- lapply(df, attr, "label")

  df[] <- lapply(seq_along(df), function(col_i) {
    col <- df[[col_i]]
    new_col <- if (length(unique(col)) < 10) forcats::as_factor(col) else col
    attr(new_col, "label") <- labs[[col_i]]
    return(new_col)
  })

  return(df)
}

#' Transform all "SPSS labelled" to factors
#'
#' Takes a dataframe and transforms all variables with class `haven_labelled`
#' to factors. Preserved variable labels if there are any.
#'
#' @param df A dataframe (optionally labelled)
#' @param min_levels Minimum number of unique values, above which the variable
#'   is not treated as a factor. Default = 10.
#'
#' @export
#'
spss_labelled_to_factor <- function(df) {
  stopifnot(is.data.frame(df))

  # Preserve variable labels
  labs <- lapply(df, attr, "label")

  df[] <- lapply(seq_along(df), function(col_i) {
    col <- df[[col_i]]
    new_col <- if (haven::is.labelled(col)) forcats::as_factor(col) else col
    attr(new_col, "label") <- labs[[col_i]]
    return(new_col)
  })

  return(df)
}

