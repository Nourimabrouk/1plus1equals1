# ------------------------------------------------------------------------------
# Interactive3D.R
# Illustrates using plotly for 3D synergy or quantum state visualization.
# ------------------------------------------------------------------------------
library(plotly)

#' plot_fractal_3D
#'
#' Visualizes fractal data in 3D, using iteration as the z-axis.
#'
#' @param df A tibble with iteration, x, y
#' @return A plotly object
#' @export
plot_fractal_3D <- function(df) {
  plot_ly(
    df,
    x = ~x, y = ~y, z = ~iteration,
    color = ~factor(iteration),
    colors = "Viridis",
    type = "scatter3d",
    mode = "markers"
  ) %>%
    layout(
      title = "3D Fractal Perspective",
      scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Iteration")
      )
    )
}
