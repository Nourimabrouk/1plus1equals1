################################################################################################################################################################
# File: synergy_dashboard.R
# Project: 1plus1equals1
# Created: 2025-01-10
# Author: 1+1=1 Dev Collective
#
# Purpose:
#   A Shiny dashboard that allows interactive exploration of the synergy between
#   the core modules (idempotent_math, quantum_state, fractal_generator, unity_category).
#   Also calculates and displays the "Oneness Score" from unity_metrics.R.
#
# Philosophy:
#   When users manipulate or unify elements in one tab, the changes reflect 
#   the universal truth that everything merges. We unify them in a real-time,
#   interactive environment.
#
################################################################################################################################################################

library(shiny)
library(tidyverse)

# If your environment doesn't auto-source, adjust these to your local paths:
# source("idempotent_math.R")
# source("quantum_state.R")
# source("fractal_generator.R")
# source("unity_category.R")
# source("unity_metrics.R")  # for compute_unity_score

ui <- fluidPage(
  titlePanel("Synergy Dashboard: 1+1=1 in Action"),
  tabsetPanel(
    tabPanel("Idempotent Arithmetic", 
             fluidRow(
               column(6,
                      h3("Idempotent Addition"),
                      numericInput("ida_a", "Value A (0 or 1):", value = 1, min = 0, max = 1),
                      numericInput("ida_b", "Value B (0 or 1):", value = 1, min = 0, max = 1),
                      verbatimTextOutput("ida_res")
               ),
               column(6,
                      h3("Idempotent Multiplication"),
                      numericInput("idm_a", "Value A (0 or 1):", value = 1, min = 0, max = 1),
                      numericInput("idm_b", "Value B (0 or 1):", value = 0, min = 0, max = 1),
                      verbatimTextOutput("idm_res")
               )
             )
    ),
    tabPanel("Quantum State",
             fluidRow(
               column(6,
                      h3("Create / Collapse Quantum States"),
                      actionButton("qs_gen", "Generate |0> + |1> State"),
                      actionButton("qs_collapse", "Collapse"),
                      verbatimTextOutput("qs_summary")
               )
             )
    ),
    tabPanel("Fractal Demo",
             fluidRow(
               column(6,
                      numericInput("fr_res", "Resolution:", 150, min = 50, max = 1000),
                      actionButton("fr_go", "Generate Mandelbrot")
               ),
               column(6,
                      plotOutput("fr_plot", width = "100%", height = "500px")
               )
             )
    ),
    tabPanel("Unity Category",
             fluidRow(
               column(6,
                      h3("Add Objects / Morphisms"),
                      textInput("cat_obj", "New Object Name:", value = "A"),
                      actionButton("cat_add_obj", "Add Object"),
                      textInput("cat_from", "From Object:", value = "A"),
                      textInput("cat_to", "To Object:", value = "U"),
                      textInput("cat_morph_name", "Morphism Name:", value = "fAU"),
                      actionButton("cat_add_morph", "Define Morphism")
               ),
               column(6,
                      verbatimTextOutput("cat_display")
               )
             )
    ),
    tabPanel("Oneness Score",
             fluidRow(
               column(12,
                      h3("Compute Oneness Score"),
                      actionButton("compute_score", "Compute Score"),
                      verbatimTextOutput("score_result")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Idempotent Arithmetic
  output$ida_res <- renderText({
    a <- input$ida_a
    b <- input$ida_b
    if (!a %in% c(0,1) || !b %in% c(0,1)) return("Please enter 0 or 1 only")
    res <- idempotent_add(a, b)
    paste(a, "+", b, "=", res)
  })
  output$idm_res <- renderText({
    a <- input$idm_a
    b <- input$idm_b
    if (!a %in% c(0,1) || !b %in% c(0,1)) return("Please enter 0 or 1 only")
    res <- idempotent_multiply(a, b)
    paste(a, "*", b, "=", res)
  })
  
  # Quantum State
  qs_obj <- reactiveVal(NULL)
  
  observeEvent(input$qs_gen, {
    qs_obj( QuantumState$new(c(1/sqrt(2), 1/sqrt(2))) )
  })
  
  observeEvent(input$qs_collapse, {
    if (!is.null(qs_obj())) {
      qs_obj()$collapse()
    }
  })
  
  output$qs_summary <- renderPrint({
    if (is.null(qs_obj())) {
      "No quantum state yet. Click 'Generate' to create one."
    } else {
      qs_obj()$summary()
    }
  })
  
  # Fractal Demo
  fract_data <- reactiveVal(NULL)
  observeEvent(input$fr_go, {
    fg <- FractalGenerator$new()
    md <- fg$generate_mandelbrot(resolution = input$fr_res, max_iter = 50)
    fract_data(md)
  })
  output$fr_plot <- renderPlot({
    req(fract_data())
    fg <- FractalGenerator$new()
    fg$plot_fractal(fract_data(), "Mandelbrot Set (Synergy)")
  })
  
  # Unity Category
  cat_obj <- reactiveVal(NULL)
  observe({
    if (is.null(cat_obj())) {
      cat_obj( UnityCategory$new("U") )
    }
  })
  
  observeEvent(input$cat_add_obj, {
    if (!is.null(cat_obj())) {
      cat_obj()$define_object(input$cat_obj)
    }
  })
  
  observeEvent(input$cat_add_morph, {
    if (!is.null(cat_obj())) {
      cat_obj()$define_morphism(input$cat_from, input$cat_to, input$cat_morph_name)
    }
  })
  
  output$cat_display <- renderPrint({
    if (is.null(cat_obj())) return("No category yet.")
    cat_obj()$display_category()
  })
  
  # Oneness Score
  observeEvent(input$compute_score, {
    # We'll do a minimal synergy example. 
    # 1) Idempotent vector
    ido_vec <- c(input$ida_a, input$ida_b, input$idm_a, input$idm_b)
    
    # 2) Category unification
    cat_score_obj <- cat_obj()
    
    # 3) Quantum states
    q_list <- list()
    if (!is.null(qs_obj())) q_list <- list(qs_obj())
    
    # 4) No fractal 'unity_factor' here, but we might pass fract_data if we had such a column
    # Combine them into a single metric
    score <- 0
    if (!is.null(cat_score_obj)) {
      source("unity_metrics.R", local = TRUE)  # ensure compute_unity_score is loaded
      # compute score
      score <- compute_unity_score(
        idempotent_vec = ido_vec,
        unity_cat = cat_score_obj,
        qstate_list = q_list
      )
    }
    output$score_result <- renderText({
      paste("Current Oneness Score:", round(score, 4))
    })
  })
  
}

shinyApp(ui, server)
