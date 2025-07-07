#' Get AI-Powered Insights
#'
#' Uses OpenAI API to generate insights about the data analysis results
#'
#' @param analysis_results Results from analyze_data function
#' @param api_key OpenAI API key (if NULL, uses environment variable)
#' @return Character string with AI insights
#' @export
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
get_ai_insights <- function(analysis_results, api_key = NULL) {
  
  # Get API key from environment if not provided
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY", "")
    if (api_key == "") {
      warning("OpenAI API key not found. Set OPENAI_API_KEY environment variable.")
      return("AI insights unavailable: No API key provided.")
    }
  }
  
  # Prepare summary of analysis results for AI
  data_summary <- prepare_data_summary(analysis_results)
  
  # Create prompt for AI analysis
  prompt <- create_ai_prompt(data_summary)
  
  # Make API call to OpenAI
  tryCatch({
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(
            role = "system",
            content = "You are a data scientist expert providing insights on exploratory data analysis results. Provide clear, actionable insights in plain English."
          ),
          list(
            role = "user",
            content = prompt
          )
        ),
        max_tokens = 1500,
        temperature = 0.7
      ), auto_unbox = TRUE)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      result <- jsonlite::fromJSON(content)
      
      if (!is.null(result$choices) && length(result$choices) > 0) {
        return(result$choices[[1]]$message$content)
      } else {
        return("AI insights unavailable: Invalid response format.")
      }
    } else {
      warning(paste("OpenAI API call failed with status:", httr::status_code(response)))
      return("AI insights unavailable: API call failed.")
    }
    
  }, error = function(e) {
    warning(paste("Error calling OpenAI API:", e$message))
    return("AI insights unavailable: API error occurred.")
  })
}

#' Prepare Data Summary for AI Analysis
#'
#' @param analysis_results Results from analyze_data function
#' @return Character string summarizing the data
prepare_data_summary <- function(analysis_results) {
  
  summary_parts <- list()
  
  # Dataset overview
  info <- analysis_results$dataset_info
  summary_parts$overview <- paste(
    "Dataset Overview:",
    sprintf("- %d rows, %d columns", info$n_rows, info$n_cols),
    sprintf("- Variable types: %d numeric, %d character, %d factor, %d logical",
            length(analysis_results$variable_types$numeric),
            length(analysis_results$variable_types$character),
            length(analysis_results$variable_types$factor),
            length(analysis_results$variable_types$logical))
  )
  
  # Missing values
  missing_vars <- analysis_results$missing_analysis %>%
    filter(missing_count > 0) %>%
    head(5)
  
  if (nrow(missing_vars) > 0) {
    missing_text <- paste(
      "Missing Values:",
      paste(sprintf("- %s: %d missing (%.1f%%)", 
                   missing_vars$variable, 
                   missing_vars$missing_count, 
                   missing_vars$missing_percentage), 
            collapse = "\n")
    )
    summary_parts$missing <- missing_text
  }
  
  # Numeric variable summary
  if (!is.null(analysis_results$numeric_summary)) {
    numeric_text <- "Numeric Variables Summary:\n"
    top_vars <- head(analysis_results$numeric_summary, 5)
    for (i in 1:nrow(top_vars)) {
      var <- top_vars[i, ]
      numeric_text <- paste(numeric_text,
        sprintf("- %s: mean=%.2f, sd=%.2f, skew=%.2f", 
                var$variable, var$mean, var$sd, var$skew),
        sep = "\n")
    }
    summary_parts$numeric <- numeric_text
  }
  
  # Outliers
  outlier_vars <- analysis_results$outliers[sapply(analysis_results$outliers, function(x) x$count > 0)]
  if (length(outlier_vars) > 0) {
    outlier_text <- "Outliers Detected:\n"
    top_outliers <- head(outlier_vars, 5)
    for (var_name in names(top_outliers)) {
      outlier_info <- top_outliers[[var_name]]
      outlier_text <- paste(outlier_text,
        sprintf("- %s: %d outliers (%.1f%%)", 
                var_name, outlier_info$count, outlier_info$percentage),
        sep = "\n")
    }
    summary_parts$outliers <- outlier_text
  }
  
  # Data quality issues
  if (length(analysis_results$quality_issues) > 0) {
    quality_text <- "Data Quality Issues:\n"
    if (!is.null(analysis_results$quality_issues$duplicate_rows)) {
      quality_text <- paste(quality_text,
        sprintf("- %d duplicate rows", analysis_results$quality_issues$duplicate_rows),
        sep = "\n")
    }
    if (!is.null(analysis_results$quality_issues$constant_variables)) {
      quality_text <- paste(quality_text,
        sprintf("- Constant variables: %s", 
                paste(analysis_results$quality_issues$constant_variables, collapse = ", ")),
        sep = "\n")
    }
    summary_parts$quality <- quality_text
  }
  
  return(paste(summary_parts, collapse = "\n\n"))
}

#' Create AI Prompt
#'
#' @param data_summary Summary of data analysis results
#' @return Character string with prompt for AI
create_ai_prompt <- function(data_summary) {
  paste(
    "Please analyze the following exploratory data analysis results and provide insights:",
    "",
    data_summary,
    "",
    "Please provide:",
    "1. Key findings and patterns in the data",
    "2. Potential data quality issues and recommendations",
    "3. Suggestions for further analysis or data preprocessing",
    "4. Any concerns about data leakage, outliers, or anomalies",
    "5. Statistical considerations for modeling this data",
    "",
    "Please explain in plain English, suitable for both technical and non-technical audiences.",
    sep = "\n"
  )
}
