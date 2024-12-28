# ------------------------------------------------------------------------------
# fractal_dashboard.R
# A Shiny dashboard to let users tweak fractal parameters in real time.
# ------------------------------------------------------------------------------
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
