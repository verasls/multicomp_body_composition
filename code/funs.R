# Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#
# Args:
#   X: A correlation matrix.
kmo <- function(X) {
  iX <- MASS::ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2 %*% iX %*% S2
  Dai <- sqrt(diag(diag(AIS)))
  AIR <- MASS::ginv(Dai) %*% AIS %*% MASS::ginv(Dai)
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b / (b + a)
  AIR <- AIR - diag(nrow(AIR)) + diag(MSA)
  kmo <- BB / (AA + BB)

  kmo
}

# Predictive residual sum of squares (PRESS)
#
# Args:
#   model: A multivariate regression model.
press <- function(model) {
  press <- residuals(model) / (1 - lm.influence(model)$hat)
  colSums(press^2)
}

# PRESS coefficient of determination (Q2)
#
# Args:
#   model: A multivariate regression model.
q2 <- function(model) {
  ss <- purrr::map(summary.aov(model), "Sum Sq")
  tss <- purrr::map_dbl(ss, sum)
  1 - press(model) / tss
}

# PRESS standard error of the estimate
#
# Args:
#   model: A multivariate regression model.
se_press <- function(model) {
  n <- nrow(model.frame(model))
  k <- length(attributes(terms(model))$term.labels)
  df <- n - k - 1
  sqrt(press(model)) / df
}
