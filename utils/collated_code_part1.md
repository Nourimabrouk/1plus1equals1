

# File: ./collate_code.R
--------------------------------------------------------------------------------

setwd("C:/Users/Nouri/Documents/GitHub/1plus1equals1")
collate_R_files <- function(output_base = "collated_code", format = c("txt", "md"), max_lines = 5000) {
  format <- match.arg(format)
  file_ext <- paste0(".", format)
  r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  if (length(r_files) == 0) stop("No R files found in the current repository!")
  process_file <- function(file) {
    content <- readLines(file, warn = FALSE)
    content <- content[!grepl("^\\s*(#|$)", content)]  # Remove comments and blank lines
    content
  }
  all_content <- lapply(r_files, function(file) {
    content <- process_file(file)
    header <- sprintf("\n\n# File: %s\n%s\n\n", file, strrep("-", 80))
    list(
      text = paste0(header, paste(content, collapse = "\n")),
      size = length(content)
    )
  })
  part1 <- c()
  part2 <- c()
  current_lines <- 0
  for (content in all_content) {
    if (current_lines + content$size <= max_lines) {
      part1 <- c(part1, content$text)
      current_lines <- current_lines + content$size
    } else {
      part2 <- c(part2, content$text)
    }
  }
  output_file1 <- paste0(output_base, "_part1", file_ext)
  output_file2 <- paste0(output_base, "_part2", file_ext)
  writeLines(paste(part1, collapse = "\n"), output_file1)
  writeLines(paste(part2, collapse = "\n"), output_file2)
  message("Code collation complete:")
  message("Part 1 saved to: ", output_file1)
  message("Part 2 saved to: ", output_file2)
  invisible(list(part1 = output_file1, part2 = output_file2))
}
collate_R_files("collated_code", format = "md")  # Creates markdown files


# File: ./core/FractalGenerator.R
--------------------------------------------------------------------------------




# File: ./core/IdempotentArithmetic.R
--------------------------------------------------------------------------------

IdempotentSemiring <- R6::R6Class("IdempotentSemiring",
                                  public = list(
                                    elements = NULL,
                                    initialize = function() {
                                      self$elements <- c(0, 1)
                                    },
                                    plus = function(a, b) {
                                      if (a == 1 || b == 1) {
                                        return(1)
                                      } else {
                                        return(0)
                                      }
                                    },
                                    times = function(a, b) {
                                      if (a == 1 && b == 1) {
                                        return(1)
                                      } else {
                                        return(0)
                                      }
                                    }
                                  )
)
plus_idem <- function(x, y) {
  ifelse(x == 1 | y == 1, 1, 0)
}
times_idem <- function(x, y) {
  ifelse(x == 1 & y == 1, 1, 0)
}


# File: ./core/QuantumState.R
--------------------------------------------------------------------------------




# File: ./core/UnityCategory.R
--------------------------------------------------------------------------------




# File: ./core/UnityMetrics.R
--------------------------------------------------------------------------------

library(dplyr)
synergy_score <- function(vec) {
  total <- length(vec)
  ones_count <- sum(vec == 1 | vec == "1")
  return(ones_count / total)
}
fractal_synergy <- function(df) {
  stdev_x <- sd(df$x)
  stdev_y <- sd(df$y)
  synergy_val <- 1 / (1 + stdev_x + stdev_y)
  return(synergy_val)
}


# File: ./dashboards/fractal_dashboard.R
--------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
source("../core/FractalGenerator.R")
source("../visuals/StaticPlots.R")
ui <- fluidPage(
  titlePanel("Fractal Dashboard - 1+1=1 Metagame"),
  sidebarLayout(
    sidebarPanel(
      numericInput("max_iter", "Max Iterations:", 5, min = 1, max = 100),
      numericInput("points_per_iter", "Points per Iteration:", 100, min = 10, max = 10000),
      actionButton("generate", "Generate Fractal!")
    ),
    mainPanel(
      plotOutput("fractalPlot")
    )
  )
)
server <- function(input, output, session) {
  fractal_data <- eventReactive(input$generate, {
    build_fractal_series(
      max_iter = input$max_iter,
      points_per_iter = input$points_per_iter
    )
  })
  output$fractalPlot <- renderPlot({
    req(fractal_data())
    plot_fractal_static(fractal_data())
  })
}
shinyApp(ui, server)


# File: ./dashboards/quantum_dashboard.R
--------------------------------------------------------------------------------

library(shiny)
library(ggplot2)
source("../core/QuantumState.R")
source("../core/UnityMetrics.R")
ui <- fluidPage(
  titlePanel("Quantum Dashboard - 1+1=1 Metagame"),
  sidebarLayout(
    sidebarPanel(
      numericInput("real_part", "Real part of wavefunction:", 1, min = -1, max = 1, step = 0.1),
      numericInput("imag_part", "Imag part of wavefunction:", 0, min = -1, max = 1, step = 0.1),
      actionButton("measure", "Measure State")
    ),
    mainPanel(
      textOutput("measurementResult"),
      textOutput("synergyOutput")
    )
  )
)
server <- function(input, output, session) {
  q_state <- reactiveVal(NULL)
  observe({
    psi <- complex(real = input$real_part, imaginary = input$imag_part)
    q_state(QuantumState$new(psi_init = psi))
  })
  measure_result <- eventReactive(input$measure, {
    qs <- q_state()
    if (!is.null(qs)) {
      qs$collapse()
      return(qs$measure())
    } else {
      return(NULL)
    }
  })
  output$measurementResult <- renderText({
    res <- measure_result()
    if (!is.null(res)) {
      paste("Measurement collapsed to:", res)
    }
  })
  output$synergyOutput <- renderText({
    res <- measure_result()
    if (!is.null(res)) {
      score <- synergy_score(c(res))
      paste("Synergy Score (1 = total oneness):", round(score, 3))
    }
  })
}
shinyApp(ui, server)


# File: ./dashboards/SynergyDashboard.R
--------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
source("../core/FractalGenerator.R")
source("../core/QuantumState.R")
source("../core/UnityMetrics.R")
ui <- fluidPage(
  titlePanel("Synergy Dashboard - 1+1=1 Metagame"),
  sidebarLayout(
    sidebarPanel(
      h3("Fractal Settings"),
      numericInput("max_iter", "Max Iterations:", 5),
      numericInput("points_per_iter", "Points per Iteration:", 100),
      br(),
      h3("Quantum Settings"),
      numericInput("real_part", "Real part of wavefunction:", 1, min=-1, max=1),
      numericInput("imag_part", "Imag part of wavefunction:", 0, min=-1, max=1),
      actionButton("refresh", "Generate & Measure")
    ),
    mainPanel(
      plotOutput("fractalPlot"),
      textOutput("fractalSynergy"),
      br(),
      textOutput("quantumMeasurement"),
      textOutput("quantumSynergy"),
      br(),
      textOutput("overallSynergy")
    )
  )
)
server <- function(input, output, session) {
  fractal_data <- eventReactive(input$refresh, {
    build_fractal_series(input$max_iter, input$points_per_iter)
  })
  fractal_synergy_val <- reactive({
    fractal_synergy(fractal_data())
  })
  quantum_result <- eventReactive(input$refresh, {
    qs <- QuantumState$new(
      psi_init = complex(real=input$real_part, imaginary=input$imag_part)
    )
    qs$collapse()
    measurement <- qs$measure()
    list(measurement = measurement, synergy = synergy_score(measurement))
  })
  output$fractalPlot <- renderPlot({
    req(fractal_data())
    plot_fractal_static(fractal_data())
  })
  output$fractalSynergy <- renderText({
    paste("Fractal Synergy:", round(fractal_synergy_val(), 3))
  })
  output$quantumMeasurement <- renderText({
    paste("Quantum Measurement Collapsed To:", quantum_result()$measurement)
  })
  output$quantumSynergy <- renderText({
    paste("Quantum Synergy Score:", round(quantum_result()$synergy, 3))
  })
  output$overallSynergy <- renderText({
    overall <- (fractal_synergy_val() + quantum_result()$synergy) / 2
    paste("Overall Oneness Score:", round(overall, 3))
  })
}
shinyApp(ui, server)


# File: ./meta/code_as_poetry.R
--------------------------------------------------------------------------------

code_as_poetry <- function() {
  poem <- c(
    "-----------------------------------------------------",
    "  We begin in fragments, scattered across time,      ",
    "  Two lines of code meet, illusions unwind,          ",
    "  In echoes of logic, synergy is found,              ",
    "  As fractals and qubits converge on the ground.     ",
    "                                                    ",
    "  The golden ratio weaves aesthetic grace,           ",
    "  A cosmic reflection in data’s embrace,             ",
    "  Our dashboards and modules in spiritual tether,    ",
    "  Proclaiming that 1 plus 1 merges together.         ",
    "-----------------------------------------------------"
  )
  cat(paste(poem, collapse = "\n"))
}
library(dplyr)
library(purrr)
library(tidyr)
meta_reflect_repo <- function(repo_path = ".") {
  files <- list.files(repo_path, recursive = TRUE, pattern = "\\.R$")
  tibble(file = files) %>%
    mutate(code = map(file, ~ readLines(file.path(repo_path, .x)))) %>%
    unnest_longer(code)
}
synergy_map <- function(df) {
  keywords <- c("1+1=1", "synergy", "oneness", "unity")
  df %>%
    group_by(file) %>%
    summarize(
      synergy_hits = sum(map_int(code, ~ sum(str_detect(.x, keywords))))
    ) %>%
    ungroup() %>%
    arrange(desc(synergy_hits))
}


# File: ./meta/meta_reflect.R
--------------------------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
meta_reflect_repo <- function(repo_path = ".") {
  files <- list.files(repo_path, recursive = TRUE, pattern = "\\.R$")
  tibble(file = files) %>%
    mutate(code = map(file, ~ readLines(file.path(repo_path, .x)))) %>%
    unnest_longer(code)
}
synergy_map <- function(df) {
  keywords <- c("1+1=1", "synergy", "oneness", "unity")
  df %>%
    group_by(file) %>%
    summarize(
      synergy_hits = sum(map_int(code, ~ sum(str_detect(.x, keywords))))
    ) %>%
    ungroup() %>%
    arrange(desc(synergy_hits))
}


# File: ./modeling/Optimization.R
--------------------------------------------------------------------------------

library(dplyr)
duality_loss <- function(a, b, target = 1) {
  ((a + b) - target)^2
}
gradient_descent_duality <- function(a_init = 0.5, b_init = 0.5,
                                     lr = 0.01, steps = 100) {
  a <- a_init
  b <- b_init
  loss_history <- numeric(steps)
  for (i in seq_len(steps)) {
    grad_a <- 2 * ((a + b) - 1)
    grad_b <- 2 * ((a + b) - 1)
    a <- a - lr * grad_a
    b <- b - lr * grad_b
    loss_history[i] <- duality_loss(a, b)
  }
  return(list(a = a, b = b, loss_history = loss_history))
}


# File: ./modeling/StatisticalModels.R
--------------------------------------------------------------------------------

library(vars)
run_VAR_convergence <- function(data_matrix, p = 1) {
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  var_model <- VAR(data_matrix, p = p, type = "const")
  return(var_model)
}
run_PCA_oneness <- function(data_matrix) {
  pca_res <- prcomp(data_matrix, center = TRUE, scale. = TRUE)
  variance_explained <- summary(pca_res)$importance[2, 1]  # PC1 proportion
  synergy <- variance_explained  # Use proportion of variance as synergy measure
  return(list(pca_res = pca_res, synergy = synergy))
}


# File: ./tests (hypothetical)/test_that.R
--------------------------------------------------------------------------------

library(testthat)
library(R6)
source("../core/IdempotentArithmetic.R")
test_that("IdempotentArithmetic works as intended", {
  semiring <- IdempotentSemiring$new()
  expect_equal(semiring$plus(1,1), 1)
  expect_equal(semiring$plus(1,0), 1)
  expect_equal(semiring$plus(0,0), 0)
  expect_equal(semiring$times(1,1), 1)
  expect_equal(semiring$times(1,0), 0)
  expect_equal(semiring$times(0,0), 0)
})
test_that("plus_idem and times_idem vectorized ops hold synergy principle", {
  x <- c(1,0,1)
  y <- c(1,1,0)
  expect_equal(plus_idem(x,y), c(1,1,1))
  expect_equal(times_idem(x,y), c(1,0,0))
})
test_that("duality_loss is zero if a+b=1", {
  loss_val <- duality_loss(0.5,0.5,1)
  expect_true(abs(loss_val) < 1e-12)  # effectively zero, floating tolerance
})


# File: ./visuals/AnimatedPlots.R
--------------------------------------------------------------------------------

library(gganimate)
library(ggplot2)
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


# File: ./visuals/Interactive3D.R
--------------------------------------------------------------------------------

library(plotly)
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


# File: ./visuals/StaticPlots.R
--------------------------------------------------------------------------------

library(ggplot2)
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
