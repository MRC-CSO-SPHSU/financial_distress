run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
# wide_data$pidp <- as.character(wide_data$pidp)

  method_list <- mice::make.method(wide_data)
  method_list[c("sex_dv_base", 
                "hiqual_dv_base", 
                "race_base", 
                "gor_dv_fact_0",
                "sf12mcs_dv_base",
                "age_dv_0",
                "pcs_lagged_0", 
                "econ_dist_bin_0",
                "sf12mcs_dv_0",
                "econ_benefits_lagged_0",
                "home_owner_lagged_0",
                "mastat_lagged_0",
                "dnc_lagged_0",
                "log_income_0",
                "econ_emp_bin_fact_0"
                )] <-
                  c("logreg", 
                    "polr", 
                    "logreg", 
                    "polr",
                    "rf",
                    "norm",
                    "rf",
                    "logreg",
                    "rf",
                    "logreg",
                    "logreg",
                    "logreg",
                    "polyreg",
                    "rf",
                    "logreg"
                    )

  pred_mat <- mice::make.predictorMatrix(wide_data)
#  pred_mat[, "pidp"] <- 0
#  pred_mat["pidp", ] <- 0

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
