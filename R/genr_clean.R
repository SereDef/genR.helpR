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

