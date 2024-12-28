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
norm_factor <- sqrt(input
              r
              e
              a
              l
              p
              a
              r
              t
              2
              +
                i
              n
              p
              u
              t
              real 
              p
              ​
              art 
              2
              +input
              imag_part^2 + pmax(0, 1 - input
                                 r
                                 e
                                 a
                                 l
                                 p
                                 a
                                 r
                                 t
                                 2
                                 −
                                 i
                                 n
                                 p
                                 u
                                 t
                                 real 
                                 p
                                 ​
                                 art 
                                 2
                                 −input
                                 imag_part^2))
QuantumState
n
e
w
(
a
m
p
l
i
t
u
d
e
s
i
n
i
t
=
c
(
c
o
m
p
l
e
x
(
r
e
a
l
=
  i
n
p
u
t
new(amplitudes 
    i
    ​
    nit=c(complex(real=input
                  real_part, imaginary = input
                  i
                  m
                  a
                  g
                  p
                  a
                  r
                  t
    )
    /
      n
    o
    r
    m
    f
    a
    c
    t
    o
    r
    ,
    c
    o
    m
    p
    l
    e
    x
    (
      r
      e
      a
      l
      =
        s
      q
      r
      t
      (
        p
        m
        a
        x
        (
          0
          ,
          1
          −
          i
          n
          p
          u
          t
          imag 
          p
          ​
          art)/norm 
        f
        ​
        actor,complex(real=sqrt(pmax(0,1−input
                                     real_part^2 - input$imag_part^2)) / norm_factor, imaginary = 0)))
})
    
    output
    amplitudes)^2
states <- factor(c("State 1", "State 2"))
df <- data.frame(states, Probability = probs)
ggplot(df, aes(x = states, y = Probability, fill = states)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(title = "Probability Distribution of Quantum States") +
  theme_minimal()
})

observeEvent(input
           measure()
           output$measurement_result
           
           