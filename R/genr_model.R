#' @title
#' Run `CMAverse` causal mediation analysis across multiple imputations
#'
#' @description
#' Fits causal mediation models (via \pkg{CMAverse}) across multiple imputed datasets
#' and stores all intermediate regression and decomposition results in a custom
#' S4 container (`mira.CMA`).
#'
#' @param datalist A list of imputed datasets (each a `data.frame`).
#' @param exposure Character name of the exposure variable.
#' @param outcome Character name of the outcome variable.
#' @param mediator Character vector of mediator variable(s).
#' @param y_model Character string specifying the outcome regression model.
#'   Defaults to `"linear"`. Passed to `CMAverse::cmest(yreg = ...)`.
#' @param allow_interaction Logical; whether to allow exposure–mediator interaction (`EMint`).
#' @param exposure_contrast Numeric vector of length two giving the reference (`astar`)
#'   and comparison (`a`) exposure levels. Defaults to `c(0, 1)`.
#' @param nboot Integer; number of bootstrap replicates per imputation. Defaults to `1000`.
#' @param basec Optional character vector of baseline confounders to adjust for.
#' @param postc Optional character vector of post-treatment confounders to adjust for.
#' @param ... Additional arguments passed to `CMAverse::cmest()`.
#' @param verbose Logical; if `TRUE`, prints progress messages for each imputation.
#'
#' @details
#' This function loops over multiple imputed datasets and calls
#' `CMAverse::cmest()` for each, capturing all regression outputs:
#' \itemize{
#'   \item `yreg` – outcome regression
#'   \item `mreg` – mediator regression
#'   \item `cregr` – post-treatment confounder regression(s)
#'   \item `decomp` – decomposition summary table
#' }
#'
#' The outputs are combined into a custom S4 object (`mira.CMA`) with slots for each,
#' and an additional slot `n` storing the sample size.
#'
#' @return
#' An S4 object of class `"mira.CMA"` with slots:
#' \describe{
#'   \item{yreg}{List of outcome model fits}
#'   \item{mreg}{List of mediator model fits}
#'   \item{cregr}{List of post-treatment confounder model fits}
#'   \item{decomp}{List of CMA decomposition tables}
#'   \item{n}{Integer sample size}
#' }
#'
#' @seealso [pool_cma_mi()] for pooling results across imputations.
#' 
#' @export
#'
#' @examplesIf exists("imp_list")
#' fits <- run_cma_mi(
#'   datalist = imp_list,
#'   exposure = "X",
#'   outcome = "Y",
#'   mediator = "M",
#'   y_model = "linear",
#'   basec = c("age", "sex"),
#'   verbose = TRUE
#' )
#' 
run_cma_mi <- function(datalist, 
  exposure,
  outcome, 
  mediator,
  y_model = "linear", 
  allow_interaction = FALSE, 
  exposure_contrast = c(0, 1),
  nboot = 1000, 
  basec = NULL,
  postc = NULL,
  ...,
  verbose = TRUE) {
  
  # Check input
  stopifnot(is.list(datalist))

  mediator <- c(mediator) # CMAverse accepts a vector

  m <- length(datalist)
  n <- nrow(datalist[[1]])

  yregr_fit <- vector("list", m)
  mregr_fit <- vector("list", m)
  cregr_fit <- vector("list", m)
  decom_fit <- vector("list", m)
  
  for (i in seq_len(m)) {
    if (verbose) message("\nImputation number: ", i)
    # Fit the model
    est <- tryCatch({
      CMAverse::cmest(
        data = datalist[[i]],
        exposure = exposure,
        mediator = mediator,
        outcome = outcome,
        yreg = y_model,
        EMint = allow_interaction,
        basec = basec,
        postc = postc,
        astar = exposure_contrast[1], 
        a = exposure_contrast[2], 
        inference = "bootstrap", # default
        nboot = nboot,
        ...
      )
    }, error = function(e) {
      warning(sprintf("cmest failed in imputation %d: %s", i, e$message))
      return(NULL)
    })
    if (!is.null(est)) {
      s <- summary(est)
      yregr_fit[[i]] <- s[["reg.output"]][["yreg"]]
      mregr_fit[[i]] <- s[["reg.output"]][["mreg"]][[1]]
      cregr_fit[[i]] <- s[["reg.output"]][["postcreg"]] # length is variable 
      decom_fit[[i]] <- s[["summarydf"]]
    } else {
      yregr_fit[[i]] <- mregr_fit[[i]] <- cregr_fit[[i]] <- decom_fit[[i]] <- NA
    }
  }
  # Only create S4 class if not exists:
  if (!"mira.CMA" %in% methods::getClasses()) {
    methods::setClass("mira.CMA", slots = list(yreg ='list', mreg ='list', cregr ='list', decomp ='list', n ='integer'))
  }
  fit <- methods::new("mira.CMA", yreg = yregr_fit, mreg = mregr_fit, cregr = cregr_fit, decomp = decom_fit, n =  as.integer(n))
  fit
}

#' @title
#' Pool CMAverse results across multiple imputations
#'
#' @description
#' Combines model estimates from a `mira.CMA` object (produced by [run_cma_mi()])
#' across multiple imputations, using Rubin’s rules for both regression and
#' decomposition outputs.
#'
#' @param fit A `mira.CMA` object returned by [run_cma_mi()].
#' @param model_name Character label to tag each pooled model in the output.
#' @param verbose Logical; if `TRUE`, prints progress and the combined table.
#'
#' @details
#' The function pools:
#' \itemize{
#'   \item Outcome regression (`yreg`)
#'   \item Mediator regression (`mreg`)
#'   \item Post-treatment confounder regression(s) (`cregr`)
#'   \item Decomposition summary (`decomp`)
#' }
#'
#' Regression pooling uses `mice::pool()`, and decomposition pooling
#' follows Rubin’s rules with small-sample corrections for degrees of freedom.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{yreg}{Pooled outcome regression summary.}
#'   \item{mreg}{Pooled mediator regression summary.}
#'   \item{cregr}{List of pooled post-treatment confounder regressions.}
#'   \item{decomp}{Pooled decomposition summary.}
#' }
#'
#' If `verbose = TRUE`, a combined summary table is also printed.
#'
#' @seealso [run_cma_mi()] for model fitting.
#' 
#' @export
#'
#' @examplesIf exists("imp_list")
#' fit_all <- run_cma_mi(imp_list, "X", "Y", "M")
#' pooled <- pool_cma_mi(fit_all, model_name = "CMA Example")
#' pooled$decomp
#' 
pool_cma_mi <- function(fit, model_name = "Model", verbose = TRUE) {
    
  if (!inherits(fit, "mira.CMA"))
    stop("Input must be a 'mira.CMA' object created by run_cma_mi().")
  
  # Helpers
  pool_regres <- function(fitlist, title) {
    fitlist <- fitlist[!is.na(fitlist)]
    if (length(fitlist) == 0) return(NULL)
    p <- mice::pool(mice::as.mira(fitlist))
    s <- summary(p)
    if ("p.value" %in% names(s)) s$sign <- ifelse(s$p.value < 0.05, "*", "")
    s$model <- title
    return(s)
  }

  pool_decomp <- function(fit, n, title) {
    
    m <- length(fit)
    p <- nrow(fit[[1]])
    est <- sapply(fit, "[", , "Estimate")
    se  <- lapply(fit, "[", , "Std.error")

    d <- Reduce("+", fit) / m
    d <- cbind("Effect" = rownames(fit[[1]]), d)

    vw <- rowMeans(sapply(se, function(x) x^2))
    vb <- numeric(p)
    for (i in seq_len(p)) vb[i] <- stats::var(est[i, ])
    vt <- vw + vb + vb / m

    d$SE_p <- sqrt(vt)
    lambda <- (vb + vb/m) / vt
    df_old <- (m - 1) / lambda^2
    df_obs <- (((n - p) + 1) / ((n - p) + 3)) * (n - p) * (1 - lambda)
    df_adj <- (df_old * df_obs) / (df_old + df_obs)
    d$W_p <- d$Estimate / d$SE_p
    t_crit <- stats::qt(0.975, df_adj)
    d$CIL_p <- d$Estimate - t_crit * d$SE_p
    d$CIU_p <- d$Estimate + t_crit * d$SE_p
    d$Pv_p <- 2 * stats::pt(-abs(d$W_p), df_adj)
    d$sign <- ifelse(d$Pv_p < 0.05, "*", "")
    d$model <- title
    return(d)
  }

  results <- list()

  # 1. Pool outcome regression (yreg)
  if (verbose) message("Pooling outcome regression (yreg)...")
  results$yreg <- pool_regres(fit@yreg, paste0(model_name, " - Outcome regression"))

  # 2. Pool mediator regression (mreg)
  if (verbose) message("Pooling mediator regression(s) (mreg)...")
  results$mreg <- pool_regres(fit@mreg, paste0(model_name, " - Mediator regression"))

  # 3. Pool confounder regression(s)
  if (verbose) message("Pooling confounder regression(s) (cregr)...")
  if (is.list(fit@cregr[[1]])) {
    results$cregr <- lapply(seq_along(fit@cregr[[1]]), function(j) {
      pool_regres(lapply(fit@cregr, function(x) x[[j]]), paste0(model_name, " - Confounder ", j))
    })
    names(results$cregr) <- paste0("Conf_", seq_along(results$cregr))
  } else {
    results$cregr <- pool_regres(fit@cregr, paste0(model_name, " - Confounder regression"))
  }

  # 4. Pool decomposition estimates
  if (verbose) message("Pooling causal decomposition (decomp)...")
  results$decomp <- pool_decomp(fit@decomp, n = fit@n,
                                title = paste0(model_name, " - Decomposition"))

  if (verbose) {
    message("\nPooling complete:")
    combined <- do.call(rbind, c(
      list(results$yreg),
      list(results$mreg),
      if (is.list(results$cregr)) results$cregr else list(results$cregr),
      list(results$decomp)
    ))
    rownames(combined) <- NULL

    print(combined)
  }

  # Return components
  invisible(results)
}