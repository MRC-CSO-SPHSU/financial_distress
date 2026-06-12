run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
  # wide_data$pidp <- as.numeric(wide_data$pidp) # not needed anymore

  method_list <- mice::make.method(wide_data)
  method_list[c("sex_dv_base", 
                "hiqual_dv_base", 
                "race_base", 
                "gor_dv_fact_base",
                "sf12mcs_dv_base",
                "age_dv_base",
                "pcs_lagged_0", "pcs_lagged_1", "pcs_lagged_2", 
                "econ_dist_bin_0", "econ_dist_bin_1", "econ_dist_bin_2",
                "sf12mcs_dv_0", "sf12mcs_dv_1", "sf12mcs_dv_2",
                "econ_benefits_lagged_0", "econ_benefits_lagged_1", "econ_benefits_lagged_2",
                "home_owner_lagged_0", "home_owner_lagged_1", "home_owner_lagged_2", 
                "mastat_lagged_0", "mastat_lagged_1", "mastat_lagged_2",
                "dnc_lagged_0", "dnc_lagged_1", "dnc_lagged_2",
                "log_income_0", "log_income_1", "log_income_2",
                "econ_emp_bin_fact_0", "econ_emp_bin_fact_1", "econ_emp_bin_fact_2"
                )] <-
                  c("logreg", 
                    "polr", 
                    "logreg", 
                    "polr",
                    "pmm",
                    "pmm",
                    "pmm", "pmm", "pmm",
                    "logreg", "logreg", "logreg",
                    "pmm", "pmm", "pmm",
                    "logreg", "logreg", "logreg",
                    "logreg", "logreg", "logreg",
                    "logreg", "logreg", "logreg",
                    "polyreg", "polyreg", "polyreg",
                    "rf", "rf", "rf",
                    "logreg", "logreg", "logreg"
                    )

  pred_mat <- mice::make.predictorMatrix(wide_data)
#  pred_mat[, "pidp"] <- 0
#  pred_mat["pidp", ] <- 0 # not needed anymore

  mice::mice(
    data            = wide_data,
#    defaultMethod   = c("pmm", "logreg", "polr", "polyreg"),
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = method_list,
    predictorMatrix = pred_mat
  )
}
