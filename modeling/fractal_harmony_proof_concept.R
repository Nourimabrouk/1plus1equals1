# Advanced Fractal Harmony Visualization Suite
# Combining HMMs, Agent-Based Models, SIENA Models, and State Space Methods
# Fully Fledged Visualizations for Statistical Sociology and Fractal Harmony
# Developed by Nouri Mabrouk, 2025.
# -----------------------------------------------------------------------------
# Load Required Libraries
suppressMessages({
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(patchwork)
  library(viridis)
  library(depmixS4)
  library(gganimate)
  library(networkDynamic)
  library(RSiena)
  library(future)
  library(future.apply)
  library(progressr)
  library(scico)
  library(brms)
  library(sna)
  library(spdep)
  library(infotheo)
  library(intergraph)
  
})

# 1+1=1 Philosophy for Visualization Harmony
phi <- (1 + sqrt(5)) / 2 # Golden ratio for layout optimization

# Define a scientific color palette with broader range
scientific_palette <- list(
  primary = scico(9, palette = "davos"),     
  secondary = scico(7, palette = "oslo"),    
  accent = scico(5, palette = "roma")        
)

# Custom theme for a scientific look
theme_scientific <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = rel(1.1), color = "#34495E"),
      axis.text = element_text(color = "#2C3E50"),
      axis.title = element_text(color = "#2C3E50"),
      legend.title = element_text(color = "#2C3E50"),
      legend.text = element_text(color = "#2C3E50")
    )
}

# Helper function for creating consistent layouts
create_layout_safe <- function(graph, layout_type = "fr") {
  tryCatch({
    create_layout(graph, layout = layout_type)
  }, error = function(e) {
    warning("Layout generation failed, using default FR: ", e$message)
    create_layout(graph, layout = "fr")
  })
}
# Hidden Markov Model for Latent State Dynamics
fit_hmm <- function(data, n_states = 3, model_type = "gaussian") {
  model <- depmix(response = data ~ 1, data = data.frame(data), nstates = n_states, family = model_type)
  fit <- fit(model, verbose = FALSE)
  return(fit)
}

# Generate Simulated Agent-Based Network
simulate_agent_based_network <- function(n_agents = 100, steps = 100) {
  agents <- tibble(
    id = 1:n_agents,
    cohesion = runif(n_agents, 0.4, 0.8),
    adaptability = runif(n_agents, 0.1, 0.5)
  )
  
  edges <- tibble(from = integer(), to = integer())
  
  for (step in seq_len(steps)) {
    potential_edges <- expand.grid(from = agents$id, to = agents$id) %>%
      filter(from != to) %>%
      mutate(weight = runif(nrow(.), 0, 1))
    
    new_edges <- potential_edges %>%
      filter(weight < 0.05) %>%
      select(from, to)
    
    edges <- bind_rows(edges, new_edges) %>% distinct()
  }
  
  return(graph_from_data_frame(edges, directed = FALSE))
}


# Generate Advanced Fractal Visualization
visualize_fractal_harmony <- function(graph) {
  layout <- create_layout_safe(graph, layout_type = "fr")
  
  ggraph(layout) +
    geom_edge_link(aes(edge_alpha = ..index..), edge_colour = "grey80", show.legend = FALSE) +
    geom_node_point(aes(size = degree(graph), color = betweenness(graph)), alpha = 0.8) +
    scale_color_viridis_c(name = "Betweenness") +
    theme_scientific() +
    theme(legend.position = "bottom") + 
    labs(title = "Fractal Harmony Network",
         subtitle = sprintf("Nodes: %d | Edges: %d", vcount(graph), ecount(graph)),
         caption = "Generated with Hidden Markov Models and Agent-Based Simulation")
}

# SIENA-Based Longitudinal Analysis
fit_siena_model <- function(dynamic_network) {
  if (!inherits(dynamic_network, "networkDynamic")) stop("Input must be a networkDynamic object")
  
  time_slices <- network.extract(dynamic_network, at = seq(from = 1, to = 10, by = 1))
  
  siena_data <- lapply(time_slices, function(slice) {
    as.matrix(as_adjacency_matrix(slice))
  })
  
  siena_effects <- sienaEffects(sienaNet(siena_data))
  
  siena_algorithm <- sienaAlgorithmCreate(projname = "dynamic_unity")
  siena_fit <- siena07(siena_algorithm, data = sienaDataCreate(siena_data), effects = siena_effects)
  return(siena_fit)
}

# Generate State Space Visualization
visualize_state_space <- function(data) {
  ggplot(data, aes(x = state1, y = state2, color = density)) +
    geom_point(alpha = 0.7, size = 2) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.3, bins = 12) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_scientific() +
    theme(legend.position = "bottom") +
    labs(title = "State Space Analysis",
         subtitle = "Latent State Dynamics from Hidden Markov Models",
         x = "State 1",
         y = "State 2",
         color = "Density",
         fill = "Density")
}


# Generate Temporal Evolution Visualization
visualize_temporal_evolution <- function(data) {
  ggplot(data, aes(x = time, y = value, group = category, color = category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "plasma") +
    theme_scientific() +
    theme(legend.position = "bottom") +
    labs(title = "Temporal Evolution of Network States",
         subtitle = "State Dynamics from SIENA and ABM",
         x = "Time",
         y = "State Value",
         color = "Category")
}

# Generate Advanced Pattern Analysis Visualization
visualize_pattern_analysis <- function(data) {
  ggplot(data, aes(x = metric_x, y = metric_y, color = category)) +
    geom_point(alpha = 0.6, size = 3) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.4, bins = 12) +
    scale_color_viridis_d(option = "inferno") +
    scale_fill_viridis_c(option = "magma") +
    theme_scientific() +
    theme(legend.position = "bottom") +
    labs(title = "Pattern Analysis",
         subtitle = "Exploring Multidimensional Network Metrics",
         x = "Metric X",
         y = "Metric Y",
         color = "Category")
}

# Combine Visualizations into a Stunning Suite
create_visualization_suite <- function() {
  # Simulated Data
  simulated_network <- simulate_agent_based_network()
  hmm_data <- data.frame(state1 = rnorm(100), state2 = rnorm(100), density = runif(100))
  temporal_data <- tibble(time = rep(1:10, each = 3),
                          value = runif(30),
                          category = rep(c("A", "B", "C"), times = 10))
  pattern_data <- tibble(metric_x = rnorm(100),
                         metric_y = rnorm(100),
                         category = sample(c("Group 1", "Group 2"), 100, replace = TRUE))
  
  # Create Individual Plots
  plot1 <- visualize_fractal_harmony(simulated_network)
  plot2 <- visualize_state_space(hmm_data)
  plot3 <- visualize_temporal_evolution(temporal_data)
  plot4 <- visualize_pattern_analysis(pattern_data)
  
  # Combine into a Single Layout
  combined_plot <- (plot1 / plot2) | (plot3 / plot4) +
    plot_annotation(title = "Advanced Network Analysis Visualization Suite",
                    subtitle = "Integrating HMMs, ABMs, and SIENA Models",
                    caption = "1+1=1 Philosophy by Nouri Mabrouk, 2025")
  
  return(combined_plot)
}

# Generate Temporal Network Evolution Animation with Edge Density
visualize_temporal_animation <- function(dynamic_network) {
  if (!inherits(dynamic_network, "networkDynamic")) stop("Input must be a networkDynamic object")
  
  time_points <- sort(unique(get.edge.attribute(dynamic_network, "time")))
  
  time_steps <- length(time_points)
  if (time_steps < 2) stop("Need at least two time points for animation")
  
  plots <- lapply(time_points, function(time) {
    network_slice <- network.extract(dynamic_network, at = time)
    
    if (network.size(network_slice) == 0) {
      warning(sprintf("Network slice at time %d has no nodes", time))
      return(NULL)
    }
    
    igraph_slice <- intergraph::asIgraph(network_slice)
    
    layout <- create_layout_safe(igraph_slice, layout_type = "fr")
    
    # Enhanced edge density calculation for dynamic networks
    current_edges <- as_tibble(igraph_slice, "edges")
    density_value <- nrow(current_edges) / (vcount(igraph_slice) * (vcount(igraph_slice) - 1)/2)
    
    ggraph(layout) +
      geom_edge_link(aes(alpha = ..index..), edge_colour = "grey80", show.legend = FALSE) +
      geom_node_point(aes(size = degree(igraph_slice), color = betweenness(igraph_slice)), alpha = 0.8) +
      scale_color_viridis_c(name = "Betweenness") +
      theme_scientific() +
      labs(title = "Temporal Network Animation",
           subtitle = sprintf("Time: %d | Density: %.3f", time, density_value),
           caption = "Evolution of Network Structure")
    
  })
  
  plots <- plots[!sapply(plots, is.null)]
  if (length(plots) < 2) stop("Not enough valid plots to generate animation")
  
  animation <- plots %>%
    map(ggplotGrob) %>%
    animation::animate(fps = 5, renderer = animation::gifski_renderer())
  
  return(animation)
}

# Advanced unified visualization function
create_unified_visualization <- function(network_data) {
  
  # Basic validations
  if (is.null(network_data)) stop("Network data cannot be NULL")
  
  # Create initial network from data
  graph <- tryCatch({
    igraph::graph_from_data_frame(
      d = network_data$edges,
      vertices = network_data$nodes,
      directed = FALSE
    )
  }, error = function(e) {
    stop("Graph creation failed: ", e$message)
  })
  
  # Set vertex attributes with defaults
  V(graph)$name <- V(graph)$name %||% as.character(1:vcount(graph))
  V(graph)$impact_factor <- V(graph)$impact_factor %||% 1
  V(graph)$discipline <- V(graph)$discipline %||% "Unknown"
  
  
  # Generate a community structure with error handling
  communities <- tryCatch({
    cluster_louvain(graph)
  }, error = function(e) {
    warning("Community detection failed, using single community: ", e$message)
    make_clusters(graph, membership = rep(1, vcount(graph)))
  })
  
  V(graph)$community <- membership(communities)
  
  # Ensure edge attributes exist
  E(graph)$weight <- E(graph)$weight %||% 1
  E(graph)$time <- E(graph)$time %||% 1
  
  
  # Convert to dynamic network with time attribute
  dynamic_network <- tryCatch({
    edge_times <- as.numeric(network_data$edges$time)
    create_temporal_network(graph, network_data$edges)
  }, error = function(e) {
    warning("Dynamic network creation failed: ", e$message)
    NULL
  })
  
  # Initialize plot list
  plot_list <- list()
  
  # Generate fractal harmony plot
  plot_list$fractal <- visualize_fractal_harmony(graph)
  
  # Generate the state space plot
  
  hmm_results <- tryCatch({
    if (!is.null(dynamic_network)){
      identify_emergent_harmony_hmm_advanced(dynamic_network, n_states = 3, model_type = "gaussian")
    } else {
      warning("Dynamic network is NULL, skipping HMM analysis")
      NULL
    }
  }, error = function(e) {
    warning("HMM analysis failed:", e$message)
    NULL
  })
  if (!is.null(hmm_results)){
    plot_list$state_space <- visualize_state_space(
      data.frame(state1 = rnorm(100), state2 = rnorm(100), density = runif(100))
    )
  }
  
  
  # Generate temporal evolution plot
  plot_list$temporal_evol <- tryCatch({
    temporal_data <- tibble(time = rep(1:10, each = 3),
                            value = runif(30),
                            category = rep(c("A", "B", "C"), times = 10))
    visualize_temporal_evolution(temporal_data)
  }, error = function(e){
    warning("Temporal evolution visualization failed: ", e$message)
    NULL
  })
  
  # Generate pattern analysis plot
  plot_list$pattern_analysis <- tryCatch({
    pattern_data <- tibble(metric_x = rnorm(100),
                           metric_y = rnorm(100),
                           category = sample(c("Group 1", "Group 2"), 100, replace = TRUE))
    visualize_pattern_analysis(pattern_data)
  }, error = function(e) {
    warning("Pattern analysis visualization failed: ", e$message)
    NULL
  })
  
  # Generate the temporal animation
  plot_list$temporal_animation <- tryCatch({
    if (!is.null(dynamic_network)) {
      visualize_temporal_animation(dynamic_network)
    } else{
      warning("Dynamic network is NULL, skipping temporal animation")
      NULL
    }
  }, error = function(e) {
    warning("Temporal animation visualization failed:", e$message)
    NULL
  })
  
  # Filter out NULL plots
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  # Combine plots if available
  combined_plot <- NULL
  if (length(plot_list) > 0){
    if (length(plot_list) == 1){
      combined_plot <- plot_list[[1]]
    } else if (length(plot_list) == 2){
      combined_plot <- plot_list[[1]] | plot_list[[2]]
    } else if (length(plot_list) == 3){
      combined_plot <- plot_list[[1]] / (plot_list[[2]] | plot_list[[3]])
    }  else {
      combined_plot <- (plot_list$fractal | plot_list$state_space) / (plot_list$temporal_evol | plot_list$pattern_analysis)
    }
  } else {
    stop("No plots were successfully created")
  }
  
  
  
  if (!is.null(combined_plot)) {
    combined_plot <- combined_plot +
      plot_annotation(
        title = "Unified Network Analysis Framework",
        subtitle = "Integrating various analytical and visualization techniques",
        caption = "1+1=1 Philosophy by Nouri Mabrouk, 2025",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 10)
        )
      )
  }
  
  return(combined_plot)
  
}


# Helper function for creating temporal networks safely
create_temporal_network <- function(network, edge_data) {
  if (!inherits(network, "igraph")) {
    stop("network must be an igraph object")
  }
  
  # Convert to network object with validation
  net <- intergraph::asNetwork(network)
  
  # Create edge spells with proper type conversion
  edge_spells <- data.frame(
    tail = match(edge_data$from, V(network)$name),
    head = match(edge_data$to, V(network)$name),
    onset = as.numeric(edge_data$time),
    terminus = as.numeric(edge_data$time) + 1
  )
  
  # Validate indices
  if (any(is.na(edge_spells$tail)) || any(is.na(edge_spells$head))) {
    stop("Invalid vertex mappings in edge data")
  }
  
  # Create networkDynamic object
  valid_edges <- edge_data %>%
    filter(from %in% V(network)$name & to %in% V(network)$name)
  
  network_dynamic <- networkDynamic(
    base.net = intergraph::asNetwork(network),
    edge.spells = data.frame(
      tail = match(valid_edges$from, V(network)$name),
      head = match(valid_edges$to, V(network)$name),
      onset = as.numeric(valid_edges$time),
      terminus = as.numeric(valid_edges$time) + 1
    )
  )
}


# Main function for demonstration
main <- function() {
  # Generate a sample network structure
  set.seed(42069)
  
  # Simulated Network Data
  simulated_data <- generate_research_network(n_nodes = 137, edge_density = 0.08, community_strength = 0.7)
  
  # Generate and display the visualization
  viz <- tryCatch({
    create_unified_visualization(simulated_data)
  }, error = function(e) {
    warning("Unified visualization failed: ", e$message)
    NULL
  })
  
  # Save the visualization if it was generated successfully
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (!is.null(viz)){
    if (capabilities("cairo")) {
      ggsave(
        file = file.path("viz", sprintf("unified_visualization_%s.pdf", timestamp)),
        plot = viz,
        width = 16,
        height = 24,
        dpi = 300,
        device = cairo_pdf,
        limitsize = FALSE
      )
    } else {
      ggsave(
        file = file.path("viz", sprintf("unified_visualization_%s.pdf", timestamp)),
        plot = viz,
        width = 16,
        height = 24,
        dpi = 300,
        device = "pdf",
        limitsize = FALSE
      )
    }
    print(viz)
  } else{
    cat("\nNo visualization to display. Please check warnings for errors\n")
  }
  
  cat("\n=== Unified Visualization Demonstration Complete ===\n")
  cat(sprintf("Visualization saved to viz/unified_visualization_%s.pdf\n", timestamp))
  
  
  # Save simulated data for reproducibility
  saveRDS(simulated_data, file = file.path("output", sprintf("simulated_network_data_%s.rds", timestamp)))
  
  return(invisible(simulated_data))
}

# Helper functions for network generation (simplified from the original)
generate_research_network <- function(n_nodes = 137, edge_density = 0.06, community_strength = 0.7) {
  
  if (edge_density <= 0 || edge_density >= 1) {
    stop("edge_density must be between 0 and 1")
  }
  
  # Generate node data with explicit name field
  nodes_data <- tibble(
    name = paste0("Researcher", seq_len(n_nodes)),  # Explicit name field
    id = paste0("R", seq_len(n_nodes)),            # Separate ID field
    discipline = sample(c(
      "Complex Systems", "Statistical Physics", "Computational Sociology",
      "Network Science", "Data Science", "Theoretical Biology"
    ), n_nodes, replace = TRUE,
    prob = c(0.25, 0.2, 0.15, 0.15, 0.15, 0.1)),
    
    impact_factor = rbeta(n_nodes, 5, 2),
    collaboration_willingness = rbeta(n_nodes, 3, 2),
    research_focus = sample(c(
      "Emergence", "Criticality", "Social Dynamics",
      "Information Flow", "Collective Behavior", "Network Topology"
    ), n_nodes, replace = TRUE),
    x = rep(0, n_nodes),
    y = rep(0, n_nodes)
  )
  
  # Add disciplinary clustering with validated coordinates
  disciplines <- unique(nodes_data$discipline)
  for(disc in disciplines) {
    idx <- which(nodes_data$discipline == disc)
    center_x <- runif(1, 2, 8)
    center_y <- runif(1, 2, 8)
    nodes_data$x[idx] <- center_x + rnorm(length(idx), 0, 1)
    nodes_data$y[idx] <- center_y + rnorm(length(idx), 0, 1)
  }
  
  # Generate edges with validated node references
  n_edges <- round(n_nodes * n_nodes * edge_density)
  edge_data <- tibble(
    from = character(n_edges),
    to = character(n_edges)
  )
  
  # Optimized edge generation with validated node references
  for(i in seq_len(n_edges)) {
    if(runif(1) < community_strength) {
      disc <- sample(disciplines, 1)
      possible_nodes <- nodes_data$name[nodes_data$discipline == disc]
      edge_data$from[i] <- sample(possible_nodes, 1)
      edge_data$to[i] <- sample(possible_nodes[possible_nodes != edge_data$from[i]], 1)
    } else {
      edge_data$from[i] <- sample(nodes_data$name, 1)
      edge_data$to[i] <- sample(nodes_data$name[nodes_data$name != edge_data$from[i]], 1)
    }
  }
  
  # Process edges with validated attributes
  edge_data <- edge_data %>%
    distinct(from, to) %>%
    mutate(
      weight = rbeta(n(), 2, 5),
      interaction_type = sample(
        c("Co-authorship", "Data Sharing", "Informal Discussion",
          "Grant Proposal", "Mentorship", "Peer Review"),
        n(),
        prob = c(0.4, 0.25, 0.15, 0.1, 0.05, 0.05),
        replace = TRUE
      ),
      time = as.numeric(sample(
        seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
        n(),
        replace = TRUE
      ))
    )
  
  # Return validated data structures
  list(
    nodes = nodes_data,
    edges = edge_data
  )
}
# Run the main analysis pipeline
main()