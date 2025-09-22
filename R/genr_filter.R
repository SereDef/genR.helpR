#' Filter data and print resulting data loss
#' 
#' @export
#' 
verbose_filter <- function(data, expr) {
  expr_quo <- rlang::enquo(expr)
  expr_text <- rlang::quo_text(expr_quo)
  
  before_n <- nrow(data)
  result <- dplyr::filter(data, !!expr_quo)
  after_n <- nrow(result)
  
  cat(paste("Filter:", expr_text, "\n"))
  cat(paste("N =", after_n, "( -", before_n - after_n, ")\n\n"))
  
  invisible(result)
}

remove_retired_consent <- function(data, consent_var='RemoveData') {
  clean_data <- data[!is.na(consent_var), ] # TODO: logic may change ?
  return(clean_data)
}

identify_siblings <- function(data, column_selection = c(), random = FALSE, seed = 31081996, 
                           mother_id = 'MOTHER', child_id = 'IDC') {
  
  # if no selection is specified, missingness in the entire dataframe is used
  if (length(column_selection) > 0) { dt <- data[, c(child_id, mother_id, column_selection)] } 
  # First randomly shuffle the dataset 
  set.seed(seed)
  dt <- dt[sample(nrow(dt)),]
  # Get rid of empty NA values for mother
  dt <- dt[!is.na(dt[,mother_id]),]
  # Determine a list of children that have a sibling in the set
  if (random) { 
    sibling_ids <- dt[duplicated(dt[, mother_id]), child_id] # i.e.  which mother IDs recur more than once
  } else {
    dt$missing <- rowSums(is.na(dt)) # compute how many missing values in the columns of interest 
    dt_ord <- dt[order(dt$missing, decreasing=FALSE),] # order based on number of missing (less missing first)
    sibling_ids <- dt_ord[duplicated(dt_ord[, mother_id]), child_id] # selection the child with more missings
  }
  message(length(sibling_ids), ' siblings identified.')
  return(sibling_ids)
}