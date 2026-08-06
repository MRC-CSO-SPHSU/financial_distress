# Custom SuperLearner learners for the TMLE step.
SL.xgboost.tmle <- function(Y, X, newX, family, ...) {
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.xgboost(Y = Y, X = X, newX = newX, family = family, ...)
}

SL.glmnet.tmle <- function(Y, X, newX, family, ...) {
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.glmnet(Y = Y, X = X, newX = newX, family = family, ...)
}
