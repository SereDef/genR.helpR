#' Read and clean .sav files
#'
#' @description
#' The function allows to quickly load and clean a .sav file into an R dataframe.
#' It requires a `file` name that can be with or without extension (i.e., '.sav')
#' specified. The path to the file can be either included in the file name, or
#' specified via the `datapath` argument. If no (existing) path is provided, a
#' navigation window will be prompted and the user can decide whether to save the
#' path for later loading jobs.
#'
#' If the file is correctly identified, this is loaded into an R data.frame which
#' preserves value labels and substitutes 999 and 888 values with NA where
#' appropriate.
#'
#'
#' @param file : file name (with or without extension; with or without full path
#' specification).
#' @param datapath : (NULL) allows to specify the path where the file is located.
#' @param summary : (FALSE) print the summary of all variables in the dataset.
#'
#' @return An R data.frame with variables as columns and observations as rows.
#' @export
#'
#' @examplesIf file.exists('filename.sav')
#' data <- genr_read('filename')
#' data <- genr_read('filename.sav', summary = TRUE) 
#'
genr_read <- function(file, datapath=NULL, summary=FALSE) {

  # Check input: file extension
  if (!grepl('.sav', file)) { file <- paste0(file,'.sav') }

  # Check input: data path
  if (!file.exists(file)) {

    if (is.null(datapath) &
        !exists('datapath', where=globalenv(), mode='character', inherits=FALSE)) {

      filepath <- file.choose()

      datapath <- dirname(filepath)
      file <- basename(filepath)

      save_path <- readline('Use location for later? For example, most files you need are stored in this folder [y/n]')
      if (grepl('y', save_path)) { assign('datapath', datapath, envir = .GlobalEnv) }

    } else if (is.null(datapath)) {

      datapath <- get('datapath', envir = .GlobalEnv) # get `datapath` from global enviroment

    }

    if (!file %in% list.files(datapath)) {
      list_files <- readline(paste0('No file called "',file,'" in "', datapath,
                                    '\n Do you want to see which files are available? [y/n]'))
      if (grepl('y', list_files)) { print(list.files(datapath)) }
      return(NULL)
    }

  } else { # user provided valid path as file
    datapath <- dirname(file)
    file <- basename(file)
  }

  # Load the file
  message('Loading...')
  d <- suppressWarnings(foreign::read.spss(file.path(datapath, file),
                                           use.value.labels = TRUE,
                                           to.data.frame = TRUE))
  cat('Done! ', nrow(d),' observations of ', base::ncol(d), ' variables.\n')

  # Make sure missing values are read in correctly
  for (col in d) {
    if (max(as.numeric(col), na.rm = TRUE) == 999) { d[!is.na(col) & col == 999,] <- NA }
    if (max(as.numeric(col), na.rm = TRUE) == 888) { d[!is.na(col) & col == 888,] <- NA }
  }

  if (summary) { print(summary(d)) }

  return(d)
}
