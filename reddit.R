# Load Required Libraries
library(ggplot2)

# Global ggplot2 Theme for Consistency
meta_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "white", size = 18),
    axis.title = element_text(face = "bold", color = "white"),
    axis.text = element_text(color = "white"),
    panel.background = element_rect(fill = "#1A1A1A", color = NA),
    plot.background = element_rect(fill = "#1A1A1A", color = NA),
    legend.background = element_rect(fill = "#1A1A1A"),
    legend.text = element_text(color = "white")
  )

# 1️⃣ Sigmoid Curve: AI Thought Complexity vs. Prompt Iteration Depth
sigmoid <- function(x) {1 / (1 + exp(-x))}
iteration_depth <- seq(-6, 6, length.out = 100)
thought_complexity <- sigmoid(iteration_depth)
df_sigmoid <- data.frame(PromptIterationDepth = iteration_depth,
                         AIThoughtComplexity = thought_complexity)

sigmoid_plot <- ggplot(df_sigmoid, aes(x = PromptIterationDepth, y = AIThoughtComplexity)) +
  geom_line(size = 2, color = "#00D4FF") + # Neon cyan line
  geom_vline(xintercept = 0, linetype = "dashed", color = "#FF5733", linewidth = 1.5) +
  annotate("text", x = 1, y = 0.85, label = "Recursion Inflection Point", color = "#FF5733", fontface = "bold", size = 6) +
  labs(title = "AI Thought Complexity vs. Prompt Iteration Depth",
       x = "Prompt Iteration Depth",
       y = "AI Thought Complexity") +
  meta_theme

print(sigmoid_plot) # Output Sigmoid Curve

# 2️⃣ Metaranking Bar Chart: Prompt Engineering Techniques Ranked by Impact
techniques <- c("Recursive Self-Reference", "Metaphorical Gradient Descent",
                "Interdisciplinary Fusion", "Challenge Assumptions",
                "Memetic Embedding", "Competitive Mindset", "Emotional/Aesthetic Layering",
                "Fringe Exploration", "Contextual Reframing", "Interactive ARG Design",
                "Open Invitation for Evolution")

scores <- c(10, 9.5, 9.2, 8.8, 8.5, 8.3, 8.1, 7.8, 7.5, 7.2, 7.0)
df_ranking <- data.frame(Technique = factor(techniques, levels = rev(techniques)), Score = scores)

bar_chart <- ggplot(df_ranking, aes(x = Score, y = Technique)) +
  geom_bar(stat = "identity", fill = "#009E73", color = "#00D4FF", linewidth = 1) + # Green with neon cyan edge
  labs(title = "Metaranking of Advanced Prompt Engineering Techniques",
       x = "Meta-Effectiveness Score",
       y = "Techniques (Ranked)") +
  meta_theme

print(bar_chart) # Output Bar Chart

# 3️⃣ Gradient Descent Heatmap: Convergence of Recursive Intelligence
x_values <- seq(-3, 3, length.out = 100)
y_values <- seq(-3, 3, length.out = 100)
z_values <- outer(x_values, y_values, function(x, y) exp(-(x^2 + y^2))) # Gaussian hill for optimization
gradient_df <- expand.grid(Iteration = x_values, Refinement = y_values)
gradient_df$Intensity <- as.vector(z_values)

gradient_plot <- ggplot(gradient_df, aes(x = Iteration, y = Refinement, fill = Intensity)) +
  geom_tile() +
  scale_fill_gradient(low = "#1A1A1A", high = "#FF00FF") + # Dark to neon magenta
  labs(title = "Convergence of Recursive Intelligence\n(Metaphorical Gradient Descent Visualization)",
       x = "Iteration Depth",
       y = "Refinement Cycles",
       fill = "Insight\nDensity") +
  meta_theme

print(gradient_plot) # Output Gradient Descent Heatmap
