#' Utility Functions for EasyEDA Package
#'
#' Collection of helper functions used throughout the package

#' Check Package Dependencies
#'
#' Checks if required packages are installed and loads them
#'
#' @param packages Character vector of package names
#' @return Logical indicating success
check_dependencies <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    warning(paste("Missing packages:", paste(missing_packages, collapse = ", ")))
    message("Install missing packages with: install.packages(c('", 
            paste(missing_packages, collapse = "', '"), "'))")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate Data Frame
#'
#' Performs basic validation on input data frame
#'
#' @param data Data frame to validate
#' @return List with validation results
validate_dataframe <- function(data) {
  
  validation <- list(
    is_valid = TRUE,
    messages = character(),
    warnings = character()
  )
  
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Input must be a data frame")
    return(validation)
  }
  
  # Check for empty data frame
  if (nrow(data) == 0) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Data frame is empty")
    return(validation)
  }
  
  if (ncol(data) == 0) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Data frame has no columns")
    return(validation)
  }
  
  # Check for reasonable size
  if (nrow(data) > 1000000) {
    validation$warnings <- c(validation$warnings, 
      "Large dataset detected. Consider sampling for better performance.")
  }
  
  if (ncol(data) > 1000) {
    validation$warnings <- c(validation$warnings,
      "High-dimensional dataset detected. Analysis may be limited to subset of variables.")
  }
  
  # Check for variable names
  if (any(names(data) == "")) {
    validation$warnings <- c(validation$warnings,
      "Some variables have empty names. Consider renaming for clarity.")
  }
  
  # Check for duplicate column names
  if (any(duplicated(names(data)))) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Duplicate column names detected")
  }
  
  return(validation)
}

#' Clean Variable Names
#'
#' Cleans variable names for better compatibility
#'
#' @param names Character vector of variable names
#' @return Character vector of cleaned names
clean_variable_names <- function(names) {
  # Remove special characters and replace with underscores
  cleaned <- gsub("[^A-Za-z0-9_]", "_", names)
  
  # Ensure names start with letter or underscore
  cleaned <- ifelse(grepl("^[0-9]", cleaned), paste0("X", cleaned), cleaned)
  
  # Remove multiple consecutive underscores
  cleaned <- gsub("_{2,}", "_", cleaned)
  
  # Remove trailing underscores
  cleaned <- gsub("_$", "", cleaned)
  
  # Ensure unique names
  cleaned <- make.unique(cleaned, sep = "_")
  
  return(cleaned)
}

#' Format Numbers for Display
#'
#' Formats numbers for better readability in reports
#'
#' @param x Numeric vector
#' @param digits Number of decimal places
#' @return Character vector of formatted numbers
format_numbers <- function(x, digits = 3) {
  ifelse(is.na(x), "N/A", 
         ifelse(abs(x) < 0.001 & x != 0, format(x, scientific = TRUE, digits = digits),
                format(round(x, digits), nsmall = ifelse(digits > 0, min(digits, 3), 0))))
}

#' Safe Division
#'
#' Performs division with protection against division by zero
#'
#' @param numerator Numeric vector
#' @param denominator Numeric vector
#' @param na_value Value to return when denominator is zero
#' @return Numeric vector
safe_divide <- function(numerator, denominator, na_value = NA) {
  result <- numerator / denominator
  result[is.infinite(result) | denominator == 0] <- na_value
  return(result)
}

#' Create Summary Table
#'
#' Creates a formatted summary table for display
#'
#' @param data Data frame
#' @param group_by Character. Variable name to group by (optional)
#' @return Data frame formatted for display
create_summary_table <- function(data, group_by = NULL) {
  
  if (!is.null(group_by) && group_by %in% names(data)) {
    # Grouped summary
    summary_data <- data %>%
      group_by(!!sym(group_by)) %>%
      summarise(
        Count = n(),
        .groups = 'drop'
      ) %>%
      mutate(
        Percentage = round((Count / sum(Count)) * 100, 1)
      ) %>%
      arrange(desc(Count))
  } else {
    # Overall summary
    summary_data <- data.frame(
      Statistic = c("Total Rows", "Total Columns", "Complete Cases", "Missing Values"),
      Value = c(
        nrow(data),
        ncol(data),
        sum(complete.cases(data)),
        sum(!complete.cases(data))
      )
    )
  }
  
  return(summary_data)
}
