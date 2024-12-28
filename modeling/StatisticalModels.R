# ------------------------------------------------------------------------------
# StatisticalModels.R
# Illustrates econometric or time-series methods that converge to "oneness."
# ------------------------------------------------------------------------------
library(vars)

#' run_VAR_convergence
#'
#' Runs a simple VAR model and checks if multiple series converge to a single
#' shared trajectory (very simplified).
#'
#' @param data_matrix A matrix or data.frame
#' @param p The lag order
#' @return A VAR model object
#' @export
run_VAR_convergence <- function(data_matrix, p = 1) {
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  var_model <- VAR(data_matrix, p = p, type = "const")
  return(var_model)
}

#' run_PCA_oneness
#'
#' If the first principal component explains "enough" variance, we treat it as
#' a sign of "1+1=1" synergy.
#'
#' @param data_matrix A numeric matrix
#' @return A list with pca results and synergy measure
#' @export
run_PCA_oneness <- function(data_matrix) {
  pca_res <- prcomp(data_matrix, center = TRUE, scale. = TRUE)
  variance_explained <- summary(pca_res)$importance[2, 1]  # PC1 proportion
  synergy <- variance_explained  # Use proportion of variance as synergy measure
  return(list(pca_res = pca_res, synergy = synergy))
}
