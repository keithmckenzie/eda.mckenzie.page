#' Generate EDA Visualizations
#'
#' Creates comprehensive visualizations for exploratory data analysis
#'
#' @param data A data frame to visualize
#' @param analysis_results Results from analyze_data function
#' @return List of ggplot objects
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom corrplot corrplot
#' @importFrom VIM aggr
generate_eda_plots <- function(data, analysis_results) {
  
  plots <- list()
  
  # Missing value pattern plot
  if (any(analysis_results$missing_analysis$missing_count > 0)) {
    tryCatch({
      missing_plot <- VIM::aggr(data, col = c('navyblue', 'red'), 
                               numbers = TRUE, sortVars = TRUE)
      plots$missing_pattern <- missing_plot
    }, error = function(e) {
      warning("Could not generate missing value pattern plot")
    })
  }
  
  # Missing value bar plot
  if (nrow(analysis_results$missing_analysis) > 0) {
    missing_summary <- analysis_results$missing_analysis %>%
      filter(missing_count > 0) %>%
      head(20)  # Top 20 variables with missing values
    
    if (nrow(missing_summary) > 0) {
      plots$missing_bars <- ggplot(missing_summary, aes(x = reorder(variable, missing_percentage), 
                                                        y = missing_percentage)) +
        geom_col(fill = "coral") +
        coord_flip() +
        labs(title = "Missing Values by Variable", 
             x = "Variable", 
             y = "Missing Percentage (%)") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))
    }
  }
  
  # Histograms for numeric variables
  numeric_vars <- analysis_results$variable_types$numeric
  if (length(numeric_vars) > 0) {
    # Limit to first 16 numeric variables for readability
    vars_to_plot <- head(numeric_vars, 16)
    
    hist_plots <- list()
    for (var in vars_to_plot) {
      hist_plots[[var]] <- ggplot(data, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
        labs(title = paste("Distribution of", var), x = var, y = "Frequency") +
        theme_minimal()
    }
    plots$histograms <- hist_plots
  }
  
  # Boxplots for numeric variables
  if (length(numeric_vars) > 0) {
    vars_to_plot <- head(numeric_vars, 16)
    
    box_plots <- list()
    for (var in vars_to_plot) {
      box_plots[[var]] <- ggplot(data, aes_string(y = var)) +
        geom_boxplot(fill = "lightgreen", alpha = 0.7) +
        labs(title = paste("Boxplot of", var), y = var) +
        theme_minimal()
    }
    plots$boxplots <- box_plots
  }
  
  # Correlation matrix heatmap
  if (!is.null(analysis_results$correlation_matrix) && ncol(analysis_results$correlation_matrix) > 1) {
    # Create correlation plot using ggplot2 for consistency
    cor_data <- as.data.frame(analysis_results$correlation_matrix) %>%
      tibble::rownames_to_column("var1") %>%
      tidyr::gather(key = "var2", value = "correlation", -var1)
    
    plots$correlation_heatmap <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                          midpoint = 0, limits = c(-1, 1)) +
      labs(title = "Correlation Matrix", x = "", y = "", fill = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Bar plots for categorical variables
  cat_vars <- c(analysis_results$variable_types$character, analysis_results$variable_types$factor)
  if (length(cat_vars) > 0) {
    # Limit to first 8 categorical variables
    vars_to_plot <- head(cat_vars, 8)
    
    bar_plots <- list()
    for (var in vars_to_plot) {
      # Get top categories to avoid overcrowding
      top_categories <- data %>%
        count(!!sym(var), sort = TRUE) %>%
        head(10)
      
      data_filtered <- data %>%
        filter(!!sym(var) %in% top_categories[[var]])
      
      bar_plots[[var]] <- ggplot(data_filtered, aes_string(x = var)) +
        geom_bar(fill = "orange", alpha = 0.7) +
        labs(title = paste("Distribution of", var), x = var, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    plots$bar_plots <- bar_plots
  }
  
  # Scatter plots for numeric variable pairs
  if (length(numeric_vars) >= 2) {
    # Create scatter plots for first few variable pairs
    pairs_to_plot <- min(6, choose(length(numeric_vars), 2))
    var_combinations <- combn(head(numeric_vars, 4), 2, simplify = FALSE)
    
    scatter_plots <- list()
    for (i in seq_len(min(pairs_to_plot, length(var_combinations)))) {
      var_pair <- var_combinations[[i]]
      plot_name <- paste(var_pair[1], "vs", var_pair[2])
      
      scatter_plots[[plot_name]] <- ggplot(data, aes_string(x = var_pair[1], y = var_pair[2])) +
        geom_point(alpha = 0.6, color = "purple") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = paste("Scatter Plot:", var_pair[1], "vs", var_pair[2]),
             x = var_pair[1], y = var_pair[2]) +
        theme_minimal()
    }
    plots$scatter_plots <- scatter_plots
  }
  
  return(plots)
}
