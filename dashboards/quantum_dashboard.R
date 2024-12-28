library(shiny)
library(ggplot2)
source("../core/QuantumState.R")
source("../core/UnityMetrics.R")

ui <- fluidPage(
  titlePanel("Quantum State Explorer - Visualizing Superposition and Measurement"),
  sidebarLayout(
    sidebarPanel(
      numericInput("real_part", "Real part of amplitude 1:", value = sqrt(0.5), min = -1, max = 1, step = 0.05),
      numericInput("imag_part", "Imaginary part of amplitude 1:", value = 0, min = -1, max = 1, step = 0.05),
      actionButton("measure_state", "Measure State")
    ),
    mainPanel(
      plotOutput("probability_plot"),
      verbatimTextOutput("measurement_result"),
      verbatimTextOutput("coherence_score")
    )
  )
)

server <- function(input, output) {
  quantum_state <- reactive({
    # Ensure amplitudes are normalized
    norm_squared_amp2 <- pmax(0, 1 - input$real_part^2 - input$imag_part^2)
    norm_factor <- sqrt(input$real_part^2 + input$imag_part^2 + norm_squared_amp2)
    QuantumState$new(amplitudes_init = c(complex(real = input$real_part / norm_factor, imaginary = input$imag_part / norm_factor),
                                         complex(real = sqrt(norm_squared_amp2) / norm_factor, imaginary = 0)))
  })
  
  output$probability_plot <- renderPlot({
    req(quantum_state())
    probs <- Mod(quantum_state()$amplitudes)^2
    states <- factor(c("State 1", "State 2"))
    df <- data.frame(states, Probability = probs)
    ggplot(df, aes(x = states, y = Probability, fill = states)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      labs(title = "Probability Distribution of Quantum States") +
      theme_minimal()
  })
  
  observeEvent(input$measure_state, {
    measurement <- quantum_state()$measure()
    output$measurement_result <- renderText({
      paste("Measurement Outcome: State", measurement)
    })
    output$coherence_score <- renderText({
      paste("Quantum Coherence:", round(quantum_coherence(quantum_state()), 3))
    })
  })
  
  output$coherence_score <- renderText({
    req(quantum_state())
    paste("Quantum Coherence:", round(quantum_coherence(quantum_state()), 3))
  })
}

shinyApp(ui, server)