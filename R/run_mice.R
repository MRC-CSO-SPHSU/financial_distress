# Congeniality guard for the imputation model.
#
# The DR-learner GATE arm's entire output is between-stratum effect contrasts, so
# the imputation model must carry the exposure x strata interaction (design 8.4).
# If mice drops one of those terms for collinearity it records the fact in
# loggedEvents and carries on silently -- which is precisely the attenuation this
# change exists to prevent. So: stop(), not message().
assert_congenial_terms <- function(mids, a_col = "econ_dist_bin_0") {
  le <- mids$loggedEvents
  if (is.null(le) || !is.data.frame(le) || nrow(le) == 0L) return(invisible(TRUE))
  if (!"out" %in% names(le)) return(invisible(TRUE))

  # mice mangles interaction terms into design-matrix column names
  # (e.g. "econ_dist_bin_01:race_baseNon.white"), so match on the exposure name
  # appearing anywhere in a term that also contains a ":".
  is_interaction <- grepl(a_col, le$out, fixed = TRUE) &
                    grepl(":",   le$out, fixed = TRUE)

  if (any(is_interaction)) {
    stop("run_mice: congeniality broken -- mice dropped ", sum(is_interaction),
         " exposure-interaction term(s) from the imputation model:\n",
         paste(utils::capture.output(print(le[is_interaction, , drop = FALSE])),
               collapse = "\n"),
         "\n\nFall back to the additive form 'A * (sex + race + hiqual)' ",
         "(design 8.4 rung 2) and state in the paper that the imputation model is ",
         "congenial for the additive part of heterogeneity only.")
  }

  invisible(TRUE)
}

run_mice <- function(wide_data, m = 10, maxit = 10, seed = 20260522,
                     outcome = c("MCS", "PCS")) {

  outcome <- match.arg(outcome)

  method_list <- mice::make.method(wide_data)

  methods_by_group <- c(
    # Time-invariant confounders
    sex_dv_base = "logreg",
    hiqual_dv_fact_base = "polr",
    race_base = "logreg",
    # Lagged confounders
    pcs_lagged = "pmm",
    mcs_lagged = "pmm",
    econ_dist_bin_lagged = "logreg",
    econ_benefits_lagged = "logreg",
    home_owner_lagged = "logreg",
    mastat_lagged = "logreg",
    dnc_lagged = "polyreg",
    econ_emp_bin_fact = "logreg",
    log_income = "pmm",       # continuous
    # Other confounders
    gor_dv_fact = "polr",
    age_dv = "norm",
    # Exposure
    econ_dist_bin = "logreg",
    # Outcome
    sf12mcs_dv = "pmm",
    sf12pcs_dv = "pmm"
  )

  group   <- sub("_(\\d+|base)$", "", names(method_list))
  matched <- group %in% names(methods_by_group)
  method_list[matched] <- methods_by_group[group[matched]]

  # ---- Congenial imputation model (design 8) -------------------------------
  # The analysis projects an AIPW score onto S = sex x race x hiqual saturated,
  # so the imputation model must be saturated in S too, otherwise imputed
  # records are drawn under effect homogeneity and GATE contrasts attenuate.
  #
  # formulas mode (not predictorMatrix mode) because mice rebuilds the design
  # matrix from the CURRENT imputations each iteration: the interaction stays
  # self-consistent even though sex/race/hiqual are themselves imputed, with no
  # passive-imputation strata_id column to drift out of agreement.
  y_col <- if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"  # wave-3 outcome only
  a_col <- "econ_dist_bin_0"
  S     <- "sex_dv_base * race_base * hiqual_dv_fact_base"

  stopifnot(all(c(y_col, a_col) %in% names(wide_data)))

  # pred_mat exists only to seed make.formulas(); it is NOT passed to mice().
  # Supplying both would leave predictorMatrix silently inert.
  pred_mat  <- mice::make.predictorMatrix(wide_data)
  form_list <- mice::make.formulas(wide_data, predictorMatrix = pred_mat)

  # Y model: saturated A x S -- the term that carries the estimand.
  form_list[[y_col]] <- stats::update.formula(
    form_list[[y_col]], stats::reformulate(c(".", paste(a_col, "*", S)))
  )
  # A model: saturated S -- lets exposure prevalence vary freely by cell
  # (measured 7.2%-47.8% across the 12 strata).
  form_list[[a_col]] <- stats::update.formula(
    form_list[[a_col]], stats::reformulate(c(".", S))
  )
  # Not touched: sf12mcs_dv_base. Pre-exposure, so it carries no part of the
  # estimand even though it is also imputed by pmm.

  mids <-  mice::mice(
           data            = wide_data,
           m               = m,
           maxit           = maxit,
           seed            = seed,
           method          = method_list,
           formulas        = form_list
          )

  le <- mids$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_mice: no logged events.")
  } else {
    message("run_mice: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  # Hard gate: a dropped A-interaction silently reintroduces the homogeneity
  # assumption this whole change removes.
  assert_congenial_terms(mids, a_col = a_col)

  mids
}
