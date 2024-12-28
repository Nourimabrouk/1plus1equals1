# ------------------------------------------------------------------------------
# StaticPlots.R
# Basic ggplot2-based visualizations for fractals, synergy, etc.
# ------------------------------------------------------------------------------
library(ggplot2)

#' plot_fractal_static
#'
#' Creates a static plot for fractal data.
#'
#' @param df A tibble with iteration, x, y
#' @return A ggplot object
#' @export
plot_fractal_static <- function(df) {
  ggplot(df, aes(x = x, y = y, color = factor(iteration))) +
    geom_point() +
    theme_minimal() +
    scale_color_viridis_d(option = "magma") +
    labs(
      title = "Fractal Data Convergence",
      subtitle = "Each iteration merges in the grand tapestry",
      color = "Iteration"
    )
}
