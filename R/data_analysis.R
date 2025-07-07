#' Comprehensive Data Analysis
#'
#' Performs comprehensive exploratory data analysis including descriptive
#' statistics, missing value analysis, and data quality assessment.
#'
#' @param data A data frame to analyze
#' @return List containing analysis results
#' @export
analyze_data <- function(data) {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Basic dataset information
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  dataset_info <- list(
    n_rows = n_rows,
    n_cols = n_cols,
    variable_names = names(data),
    variable_types = sapply(data, function(x) class(x)[1]),
    memory_usage = format(object.size(data), units = "auto")
  )
  
  # Identify variable types
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  character_vars <- names(data)[sapply(data, is.character)]
  factor_vars <- names(data)[sapply(data, is.factor)]
  logical_vars <- names(data)[sapply(data, is.logical)]
  date_vars <- names(data)[sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  
  variable_types <- list(
    numeric = numeric_vars,
    character = character_vars,
    factor = factor_vars,
    logical = logical_vars,
    date = date_vars
  )
  
  # Missing value analysis - using base R only
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_percentages <- round((missing_counts / n_rows) * 100, 2)
  
  missing_analysis <- data.frame(
    variable = names(missing_counts),
    missing_count = as.numeric(missing_counts),
    missing_percentage = missing_percentages,
    stringsAsFactors = FALSE
  )
  missing_analysis <- missing_analysis[order(-missing_analysis$missing_count), ]
  
  # Descriptive statistics for numeric variables - using base R only
  numeric_summary <- NULL
  if (length(numeric_vars) > 0) {
    numeric_summary <- data.frame(
      variable = numeric_vars,
      n = sapply(data[numeric_vars], function(x) sum(!is.na(x))),
      mean = round(sapply(data[numeric_vars], function(x) mean(x, na.rm = TRUE)), 3),
      sd = round(sapply(data[numeric_vars], function(x) sd(x, na.rm = TRUE)), 3),
      median = round(sapply(data[numeric_vars], function(x) median(x, na.rm = TRUE)), 3),
      min = round(sapply(data[numeric_vars], function(x) min(x, na.rm = TRUE)), 3),
      max = round(sapply(data[numeric_vars], function(x) max(x, na.rm = TRUE)), 3),
      q25 = round(sapply(data[numeric_vars], function(x) quantile(x, 0.25, na.rm = TRUE)), 3),
      q75 = round(sapply(data[numeric_vars], function(x) quantile(x, 0.75, na.rm = TRUE)), 3),
      stringsAsFactors = FALSE
    )
    numeric_summary$range <- numeric_summary$max - numeric_summary$min
    numeric_summary$iqr <- numeric_summary$q75 - numeric_summary$q25
  }
  
  # Categorical variable summary - using base R only
  categorical_summary <- list()
  cat_vars <- c(character_vars, factor_vars)
  if (length(cat_vars) > 0) {
    for (var in cat_vars) {
      var_table <- table(data[[var]], useNA = "ifany")
      var_df <- data.frame(
        category = names(var_table),
        n = as.numeric(var_table),
        stringsAsFactors = FALSE
      )
      var_df$percentage <- round((var_df$n / sum(var_df$n)) * 100, 2)
      var_df <- var_df[order(-var_df$n), ]
      categorical_summary[[var]] <- head(var_df, 10)  # Top 10 categories
    }
  }
  
  # Outlier detection for numeric variables
  outliers <- list()
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars) {
      var_data <- data[[var]]
      var_data <- var_data[!is.na(var_data)]
      
      if (length(var_data) > 0) {
        Q1 <- quantile(var_data, 0.25)
        Q3 <- quantile(var_data, 0.75)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        outlier_indices <- which(data[[var]] < lower_bound | data[[var]] > upper_bound)
        outlier_values <- data[[var]][outlier_indices]
        
        outliers[[var]] <- list(
          count = length(outlier_indices),
          percentage = round((length(outlier_indices) / n_rows) * 100, 2),
          lower_bound = round(lower_bound, 3),
          upper_bound = round(upper_bound, 3),
          values = head(outlier_values[!is.na(outlier_values)], 5)  # Show first 5 outliers
        )
      }
    }
  }
  
  # Correlation analysis
  correlation_matrix <- NULL
  if (length(numeric_vars) > 1) {
    correlation_matrix <- cor(data[numeric_vars], use = "complete.obs")
  }
  
  # Data quality issues
  quality_issues <- list()
  
  # Check for duplicate rows
  duplicate_rows <- sum(duplicated(data))
  if (duplicate_rows > 0) {
    quality_issues$duplicate_rows <- duplicate_rows
  }
  
  # Check for constant variables
  constant_vars <- character(0)
  for (col in names(data)) {
    unique_vals <- unique(data[[col]][!is.na(data[[col]])])
    if (length(unique_vals) <= 1) {
      constant_vars <- c(constant_vars, col)
    }
  }
  if (length(constant_vars) > 0) {
    quality_issues$constant_variables <- constant_vars
  }
  
  # Check for high cardinality categorical variables
  high_cardinality <- character(0)
  if (length(cat_vars) > 0) {
    for (var in cat_vars) {
      unique_count <- length(unique(data[[var]]))
      if (unique_count > n_rows * 0.9) {
        high_cardinality <- c(high_cardinality, var)
      }
    }
  }
  if (length(high_cardinality) > 0) {
    quality_issues$high_cardinality_variables <- high_cardinality
  }
  
  return(list(
    dataset_info = dataset_info,
    variable_types = variable_types,
    missing_analysis = missing_analysis,
    numeric_summary = numeric_summary,
    categorical_summary = categorical_summary,
    outliers = outliers,
    correlation_matrix = correlation_matrix,
    quality_issues = quality_issues
  ))
}