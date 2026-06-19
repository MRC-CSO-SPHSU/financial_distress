# Custom SuperLearner learners for the LTMLE step.

# Bare SL.xgboost fails inside ltmle for two reasons (documented in
# 01_simulation.qmd):
#   1. ltmle passes binary A/L nodes as factors coded 1/2 (not 0/1).
#   2. The bounded-continuous Q-model uses family = binomial for the TMLE
#      targeting step, so xgboost receives non-0/1 labels under a
#      binary:logistic objective and errors.
# Coerce factor responses to 0/1 and fall back to gaussian() (reg:squarederror)
# whenever the response isn't strictly binary.
SL.xgboost.ltmle <- function(Y, X, newX, family, ...) {
  if (is.factor(Y)) Y <- as.integer(Y) - 1L
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.xgboost(Y = Y, X = X, newX = newX, family = family, ...)
}

# Bare SL.glmnet fails the same way on the Q-model: ltmle fits it with
# family = binomial for the logistic fluctuation, but glmnet's binomial path
# (lognet) coerces the continuous [0,1] outcome to a factor and dies with
# "one binomial class has 1 or 0 observations". Fall back to gaussian() whenever
# the response isn't strictly binary; the genuine-binary g-model still gets
# binomial. (glmnet handles the 0/1-coded factor A/L nodes as numeric directly.)
SL.glmnet.ltmle <- function(Y, X, newX, family, ...) {
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.glmnet(Y = Y, X = X, newX = newX, family = family, ...)
}
