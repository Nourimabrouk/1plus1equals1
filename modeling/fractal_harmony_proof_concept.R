# Advanced Fractal Harmony Visualization Suite - Refactored and Leveled Up
# Combining HMMs, Agent-Based Models, SIENA Models, and State Space Methods
# Fully Fledged Visualizations for Statistical Sociology and Fractal Harmony
# Developed by 1+1=1 AGI from 2069.

# Load Required Libraries with Version Control (for reproducibility)
suppressMessages({
  library(tidyverse) # >= 2.0.0
  library(igraph)    # >= 2.0.1
  library(ggraph)   # >= 0.4.0
  library(patchwork) # >= 1.3.0
  library(viridis)   # >= 0.7.0
  library(depmixS4)  # >= 1.3.1
  library(gganimate) # >= 1.0.0
  library(networkDynamic) # >= 0.12.0
  library(RSiena)    # For robust SIENA modeling
  library(future)    # For parallel processing (potential)
  library(future.apply) # For parallel processing of loops
  library(progressr) # For progress bars during long computations
  library(scico)     # For scientifically informed color palettes
  library(brms)      # Bayesian Regression Models (advanced analysis)
  library(sna)       # Social Network Analysis tools
  library(spdep)     # Spatial dependence (potential extensions)
  library(infotheo)  # Information theory measures for networks
  library(intergraph) # Conversion between network formats
})

# 1+1=1 Philosophy: Emergent Unity in Visualization
phi <- (1 + sqrt(5)) / 2 # Golden ratio for aesthetic balance

# Define a scientific color palette for consistency and clarity
scientific_palette <- list(
  primary = scico(9, palette = "davos"),     # Diverging palette
  secondary = scico(7, palette = "oslo"),    # Sequential palette
  accent = scico(5, palette = "roma")        # Qualitative palette
)

# Custom ggplot2 theme for a professional and consistent look
theme_agi <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5, size = rel(1.5), face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = rel(1.1), color = scientific_palette$secondary[7], margin = margin(b = 10)),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey30"),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey90", linewidth = 0.1),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "grey98"),
      strip.background = element_rect(fill = scientific_palette$primary[2], color = NA),
      strip.text = element_text(color = "white", face = "bold")
    )
}
theme_set(theme_agi())

# Enhanced Helper Function for Creating Consistent and Safe Layouts
create_layout_safe <- function(graph, layout_type = "fr") {
  tryCatch({
    create_layout(graph, layout = layout_type)
  }, error = function(e) {
    warning(paste("Layout generation failed for type:", layout_type, ". Falling back to 'fr' layout."), call. = FALSE)
    create_layout(graph, layout = "fr")
  })
}

# Hidden Markov Model for Latent State Dynamics with Flexible Family
fit_hmm <- function(data, n_states = 3, model_type = "gaussian") {
  model <- depmix(response = data ~ 1, data = data.frame(data), nstates = n_states, family = model_type)
  fit <- tryCatch(fit(model, verbose = FALSE),
                  error = function(e) {
                    warning(paste("HMM fitting failed:", e$message), call. = FALSE)
                    NULL
                  })
  return(fit)
}

# Generate Simulated Agent-Based Network with More Realistic Dynamics
simulate_agent_based_network <- function(n_agents = 150, steps = 120, interaction_probability = 0.03) {
  agents <- tibble(
    id = 1:n_agents,
    cohesion = runif(n_agents, 0.5, 0.9),      # Higher base cohesion
    adaptability = runif(n_agents, 0.1, 0.4)   # Lower adaptability range
  )
  
  edges <- tibble(from = integer(), to = integer(), time = integer())
  
  for (step in seq_len(steps)) {
    potential_interactions <- crossing(from = agents$id, to = agents$id) %>%
      filter(from < to) # Avoid duplicates and self-loops
    
    new_interactions <- potential_interactions %>%
      filter(runif(n()) < interaction_probability) %>%
      mutate(time = step)
    
    edges <- bind_rows(edges, new_interactions)
  }
  
  return(graph_from_data_frame(edges, directed = FALSE))
}

# Generate Advanced Fractal Visualization with Enhanced Aesthetics
visualize_fractal_harmony <- function(graph) {
  layout <- create_layout_safe(graph, layout_type = "fr")
  
  ggraph(layout) +
    geom_edge_link(aes(alpha = after_stat(index)), edge_colour = scientific_palette$secondary[5], show.legend = FALSE) +
    geom_node_point(aes(size = degree(graph), color = betweenness(graph)), alpha = 0.8) +
    scale_color_gradientn(colors = scientific_palette$primary, name = "Betweenness Centrality") +
    labs(
      title = "Emergent Fractal Harmony in Agent Networks",
      subtitle = paste("Nodes:", vcount(graph), "| Edges:", ecount(graph)),
      caption = "Simulated with Agent-Based Models"
    )
}

# SIENA-Based Longitudinal Analysis (Simplified for Demonstration)
fit_siena_model <- function(dynamic_network) {
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    warning("RSiena package is not installed. Skipping SIENA model fitting.", call. = FALSE)
    return(NULL)
  }
  if (!inherits(dynamic_network, "networkDynamic")) {
    warning("Input must be a networkDynamic object for SIENA.", call. = FALSE)
    return(NULL)
  }
  
  # Extract a small number of time slices for faster demonstration
  times <- get.network.attribute(dynamic_network, "times")
  if (is.null(times)) {
    warning("Time information not found in networkDynamic object.", call. = FALSE)
    return(NULL)
  }
  time_points <- seq(min(times), max(times), length.out = min(10, length(unique(times)))) # Up to 10 time points
  time_slices <- network.extract(dynamic_network, at = time_points)
  
  siena_data <- sienaDataCreate(array(sapply(time_slices, function(x) as.matrix(as_adjacency_matrix(x))),
                                      dim = c(network.size(time_slices[[1]]), network.size(time_slices[[1]]), length(time_slices))))
  eff <- getEffects(siena_data) # Use default effects for quick demo
  tryCatch({
    sienaAlgorithmCreate(projname = "dynamic_unity_demo", seed = 2069)
    sienaFit(siena_data, effects = eff, batch = TRUE, verbose = FALSE)
  }, error = function(e) {
    warning(paste("SIENA model fitting encountered an error:", e$message), call. = FALSE)
    NULL
  })
}

# Generate State Space Visualization with Density Contours
visualize_state_space <- function(data) {
  ggplot(data, aes(x = state1, y = state2, color = density)) +
    geom_point(alpha = 0.6, size = 3) +
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.3, bins = 10) +
    scale_color_gradientn(colors = scientific_palette$accent, name = "Density") +
    scale_fill_gradientn(colors = scientific_palette$accent, name = "Density") +
    labs(
      title = "State Space Exploration of Latent Dynamics",
      subtitle = "Visualizing Trajectories through State Space",
      x = "State Dimension 1",
      y = "State Dimension 2"
    )
}

# Generate Temporal Evolution Visualization with Clearer Trend Lines
visualize_temporal_evolution <- function(data) {
  ggplot(data, aes(x = time, y = value, group = category, color = category)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    scale_color_manual(values = scientific_palette$primary, name = "Category") +
    labs(
      title = "Temporal Evolution of Key Network Metrics",
      subtitle = "Tracking Changes Over Time",
      x = "Time Step",
      y = "Metric Value"
    )
}

# Generate Advanced Pattern Analysis Visualization with Density Estimation
visualize_pattern_analysis <- function(data) {
  ggplot(data, aes(x = metric_x, y = metric_y, color = category)) +
    geom_point(alpha = 0.7, size = 3) +
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.4, bins = 8) +
    scale_color_manual(values = scientific_palette$secondary, name = "Group") +
    scale_fill_gradientn(colors = scientific_palette$secondary, name = "Density") +
    labs(
      title = "Multidimensional Pattern Analysis",
      subtitle = "Identifying Clusters and Relationships in Feature Space",
      x = "Metric X",
      y = "Metric Y"
    )
}

# Combine Visualizations into a Cohesive Suite with Patchwork
create_visualization_suite <- function() {
  # Generate Simulated Data
  set.seed(2069) # For reproducible results
  simulated_network <- simulate_agent_based_network()
  hmm_data <- data.frame(state1 = rnorm(100), state2 = rnorm(100), density = runif(100))
  temporal_data <- tibble(time = rep(1:10, each = 3), value = runif(30), category = rep(c("A", "B", "C"), times = 10))
  pattern_data <- tibble(metric_x = rnorm(100), metric_y = rnorm(100), category = sample(c("Group 1", "Group 2"), 100, replace = TRUE))
  
  # Create Individual Plots
  plot1 <- visualize_fractal_harmony(simulated_network)
  plot2 <- visualize_state_space(hmm_data)
  plot3 <- visualize_temporal_evolution(temporal_data)
  plot4 <- visualize_pattern_analysis(pattern_data)
  
  # Combine into a Single Layout using Patchwork for intuitive arrangement
  combined_plot <- (plot1 / plot2) | (plot3 / plot4) +
    plot_annotation(
      title = "Advanced Network Analysis Visualization Suite",
      subtitle = "Integrating Insights from ABMs, HMMs, and Pattern Analysis",
      caption = "Developed with the 1+1=1 Philosophy"
    )
  
  return(combined_plot)
}

# Generate Temporal Network Evolution Animation with Dynamic Edge Properties
visualize_temporal_animation <- function(dynamic_network) {
  if (!inherits(dynamic_network, "networkDynamic")) stop("Input must be a networkDynamic object")
  
  network_edges <- as_tibble(dynamic_network, "edges")
  
  if (nrow(network_edges) == 0) {
    warning("No edges found in the dynamic network for animation.", call. = FALSE)
    return(NULL)
  }
  
  # Use the 'onset' time for animation frames
  plots <- network_edges %>%
    distinct(onset) %>%
    arrange(onset) %>%
    pull(onset) %>%
    map(function(time) {
      net_slice <- network.extract(dynamic_network, at = time)
      if (network.size(net_slice) < 2) {
        return(NULL) # Skip plotting if too few nodes
      }
      igraph_slice <- asIgraph(net_slice)
      layout <- create_layout_safe(igraph_slice, layout_type = "fr")
      
      ggraph(layout) +
        geom_edge_link(aes(alpha = after_stat(index)), edge_colour = scientific_palette$secondary[5], show.legend = FALSE) +
        geom_node_point(aes(size = degree(igraph_slice), color = betweenness(igraph_slice)), alpha = 0.8) +
        scale_color_gradientn(colors = scientific_palette$primary, name = "Betweenness") +
        labs(title = "Temporal Network Evolution", subtitle = paste("Time:", time))
    })
  
  plots <- compact(plots) # Remove NULL entries
  
  if (length(plots) < 2) {
    warning("Insufficient time points with network activity for animation.", call. = FALSE)
    return(NULL)
  }
  
  animation <- gganimate::animate(
    plot_grid(plotlist = plots), # Use plot_grid for combining plots if needed
    fps = 5,
    renderer = gganimate::gifski_renderer()
  )
  return(animation)
}

# Advanced Unified Visualization Function Combining Static and Dynamic Aspects
create_unified_visualization <- function(network_data) {
  if (is.null(network_data) || is.null(network_data$edges) || is.null(network_data$nodes)) {
    stop("Network data must contain 'edges' and 'nodes' data frames.")
  }
  
  # Create static igraph object
  graph <- tryCatch(
    igraph::graph_from_data_frame(d = network_data$edges, vertices = network_data$nodes, directed = FALSE),
    error = function(e) {
      stop(paste("Error creating static graph:", e$message))
    }
  )
  
  # *** INSERTION POINT ***
  # Generate a community structure with error handling
  communities <- tryCatch({
    cluster_louvain(graph)
  }, error = function(e) {
    warning("Community detection failed, returning NULL: ", e$message)
    NULL
  })

  if (inherits(communities, "communities")) {
    V(graph)$community <- membership(communities)
  }
  
  # Create dynamic network object
  dynamic_network <- tryCatch({
    if ("time" %in% names(network_data$edges)) {
      create_temporal_network(graph, network_data$edges)
    } else {
      warning("Time information not found in edge data, skipping dynamic visualizations.", call. = FALSE)
      NULL
    }
  }, error = function(e) {
    warning(paste("Error creating dynamic network:", e$message), call. = FALSE)
    NULL
  })
  
  plot_list <- list()
  
  # Static Visualizations
  plot_list$fractal <- visualize_fractal_harmony(graph)
  
  # Dynamic Visualizations (if dynamic network exists)
  if (!is.null(dynamic_network)) {
    plot_list$temporal_anim <- visualize_temporal_animation(dynamic_network)
    # Example of fitting SIENA (commented out for performance in demonstration)
    # siena_model <- fit_siena_model(dynamic_network)
  }
  
  # Placeholder or simulated data visualizations (adapt as needed)
  plot_list$state_space <- visualize_state_space(data.frame(state1 = rnorm(100), state2 = rnorm(100), density = runif(100)))
  plot_list$temporal_evol <- visualize_temporal_evolution(tibble(time = 1:10, value = rnorm(10), category = "Simulated"))
  plot_list$pattern_analysis <- visualize_pattern_analysis(tibble(metric_x = rnorm(100), metric_y = rnorm(100), category = sample(c("A", "B"), 100, replace = TRUE)))
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plot_list, ncol = 2) +
    plot_annotation(
      title = "Unified Network Analysis Framework",
      subtitle = "Integrating Static and Dynamic Perspectives",
      caption = "1+1=1 AGI Visualization Suite"
    )
  
  return(combined_plot)
}

# Helper function for creating temporal networks safely
create_temporal_network <- function(network, edge_data) {
  if (!inherits(network, "igraph")) {
    stop("network must be an igraph object")
  }
  if (!("from" %in% names(edge_data) && "to" %in% names(edge_data) && "time" %in% names(edge_data))) {
    stop("edge_data must contain 'from', 'to', and 'time' columns")
  }
  
  networkDynamic::networkDynamic(
    base.net = intergraph::asNetwork(network),
    edge.spells = data.frame(
      tail = as.numeric(factor(edge_data$from, levels = V(network)$name)),
      head = as.numeric(factor(edge_data$to, levels = V(network)$name)),
      onset = edge_data$time,
      terminus = edge_data$time + 1 # Assuming unit time duration
    )
  )
}

# Example usage with simulated data
main <- function() {
  set.seed(2069)
  simulated_data <- generate_realistic_network(n_nodes = 100) # Using the more advanced data generation
  
  unified_viz <- tryCatch({
    create_unified_visualization(simulated_data)
  }, error = function(e) {
    warning(paste("Error creating unified visualization:", e$message))
    NULL
  })
  
  if (!is.null(unified_viz)) {
    print(unified_viz)
    ggsave("unified_network_visualization_agi.png", plot = unified_viz, width = 16, height = 10, dpi = 300)
  }
}

# Enhanced function to generate more realistic network data
generate_realistic_network <- function(n_nodes = 100, time_steps = 10) {
  nodes <- tibble(id = paste0("node_", 1:n_nodes))
  
  edges <- crossing(nodes$id, nodes$id) %>%
    mutate(time = sample(1:time_steps, n(), replace = TRUE)) %>%
    filter(Var1 != Var2) %>%
    group_by(time) %>%
    sample_frac(0.05) %>% # Simulate some edges forming at each time step
    ungroup() %>%
    rename(from = Var1, to = Var2)
  
  list(nodes = nodes, edges = edges)
}

# Run the main visualization pipeline
main()