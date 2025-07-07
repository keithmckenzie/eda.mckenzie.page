#' McKenzie's EDA: Automated Exploratory Data Analysis with AI-Powered Insights
#'
#' @description
#' EasyEDA provides automated exploratory data analysis with AI-powered insights
#' and professional report generation. The package generates comprehensive HTML/PDF
#' reports with visualizations, statistical summaries, and plain English explanations.
#'
#' @docType package
#' @name eda.mckenzie.page
NULL

#' Generate Comprehensive EDA Report
#'
#' Creates a professional exploratory data analysis report with automated
#' visualizations, statistical summaries, and AI-powered insights.
#'
#' @param data A data frame to analyze
#' @param output_file Character. Output file name (default: "eda_report.html")
#' @param output_format Character. Output format: "html" or "pdf" (default: "html")
#' @param title Character. Report title (default: "Exploratory Data Analysis Report")
#' @param include_ai_insights Logical. Whether to include AI-powered insights (default: TRUE)
#' @param openai_api_key Character. OpenAI API key (if NULL, uses environment variable)
#' @param max_variables Integer. Maximum number of variables to analyze (default: 50)
#' @param sample_size Integer. Sample size for large datasets (default: 10000)
#'
#' @return Character. Path to generated report file
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EDA report for mtcars dataset
#' report_path <- generate_eda_report(mtcars, "mtcars_eda.html")
#' 
#' # Generate PDF report with custom title
#' report_path <- generate_eda_report(
#'   data = iris,
#'   output_file = "iris_analysis.pdf",
#'   output_format = "pdf",
#'   title = "Iris Dataset Analysis"
#' )
#' }
generate_eda_report <- function(data,
                               output_file = "eda_report.html",
                               output_format = "html",
                               title = "Exploratory Data Analysis Report",
                               include_ai_insights = TRUE,
                               openai_api_key = NULL,
                               max_variables = 50,
                               sample_size = 10000) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }
  
  if (!output_format %in% c("html", "pdf")) {
    stop("Output format must be 'html' or 'pdf'")
  }
  
  # Sample data if too large
  if (nrow(data) > sample_size) {
    message(paste("Dataset has", nrow(data), "rows. Sampling", sample_size, "rows for analysis."))
    data <- data[sample(nrow(data), sample_size), ]
  }
  
  # Limit number of variables
  if (ncol(data) > max_variables) {
    message(paste("Dataset has", ncol(data), "variables. Using first", max_variables, "variables."))
    data <- data[, 1:max_variables]
  }
  
  # Perform comprehensive data analysis
  analysis_results <- analyze_data(data)
  
  # Get AI insights if requested
  ai_insights <- NULL
  if (include_ai_insights) {
    tryCatch({
      ai_insights <- get_ai_insights(analysis_results, openai_api_key)
    }, error = function(e) {
      warning(paste("Failed to get AI insights:", e$message))
      ai_insights <- NULL
    })
  }
  
  # Get statistical test recommendations
  test_recommendations <- recommend_statistical_tests(data)
  
  # Generate report
  report_path <- generate_report(
    data = data,
    analysis_results = analysis_results,
    ai_insights = ai_insights,
    test_recommendations = test_recommendations,
    output_file = output_file,
    output_format = output_format,
    title = title
  )
  
  message(paste("EDA report generated successfully:", report_path))
  return(report_path)
}
