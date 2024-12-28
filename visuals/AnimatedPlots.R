# ------------------------------------------------------------------------------
# AnimatedPlots.R
# Using gganimate to show iterative transitions from "many" to "one."
# ------------------------------------------------------------------------------
library(gganimate)
library(ggplot2)

#' animate_fractal
#'
#' Creates an animation that transitions through fractal iterations.
#'
#' @param df A tibble with iteration, x, y
#' @return A gganimate object
#' @export
animate_fractal <- function(df) {
  p <- ggplot(df, aes(x = x, y = y, color = factor(iteration))) +
    geom_point(alpha = 0.7) +
    theme_minimal() +
    scale_color_viridis_d() +
    labs(
      title = "Fractal Iteration: {frame_time}",
      subtitle = "Observe 'plural' merging into 'singular' patterns over time",
      color = "Iteration"
    ) +
    transition_time(iteration)
  
  return(p)
}
