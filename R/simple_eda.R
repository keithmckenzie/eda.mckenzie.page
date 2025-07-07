#' Simple EDA Report Generator
#'
#' A simplified version of the EDA report generator that works reliably
#'
#' @param data A data frame to analyze
#' @param output_file Output file name
#' @param title Report title
#' @return Path to generated report
#' @export
simple_eda_report <- function(data, output_file = "simple_eda.html", title = "Data Analysis Report") {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Basic analysis
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Identify variable types
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  character_vars <- names(data)[sapply(data, is.character)]
  factor_vars <- names(data)[sapply(data, is.factor)]
  
  # Missing values
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_pct <- round((missing_counts / n_rows) * 100, 2)
  
  # Basic statistics for numeric variables
  numeric_stats <- NULL
  if (length(numeric_vars) > 0) {
    numeric_stats <- data.frame(
      Variable = numeric_vars,
      Mean = round(sapply(data[numeric_vars], function(x) mean(x, na.rm = TRUE)), 3),
      Median = round(sapply(data[numeric_vars], function(x) median(x, na.rm = TRUE)), 3),
      SD = round(sapply(data[numeric_vars], function(x) sd(x, na.rm = TRUE)), 3),
      Min = round(sapply(data[numeric_vars], function(x) min(x, na.rm = TRUE)), 3),
      Max = round(sapply(data[numeric_vars], function(x) max(x, na.rm = TRUE)), 3)
    )
  }
  
  # Create HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <title>', title, '</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; background-color: #f8f9fa; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
        h2 { color: #34495e; margin-top: 30px; border-left: 4px solid #3498db; padding-left: 15px; }
        h3 { color: #7f8c8d; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
        th { background-color: #f2f2f2; font-weight: bold; }
        tr:nth-child(even) { background-color: #f9f9f9; }
        .summary-box { background-color: #e8f4fd; border-left: 4px solid #3498db; padding: 15px; margin: 20px 0; }
        .stat-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin: 20px 0; }
        .stat-card { background: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6; }
        .number { font-size: 24px; font-weight: bold; color: #3498db; }
    </style>
</head>
<body>
    <div class="container">
        <h1>', title, '</h1>
        <p><strong>Generated on:</strong> ', Sys.Date(), '</p>
        
        <h2>Dataset Overview</h2>
        <div class="stat-grid">
            <div class="stat-card">
                <div class="number">', format(n_rows, big.mark = ","), '</div>
                <div>Total Rows</div>
            </div>
            <div class="stat-card">
                <div class="number">', n_cols, '</div>
                <div>Total Columns</div>
            </div>
            <div class="stat-card">
                <div class="number">', length(numeric_vars), '</div>
                <div>Numeric Variables</div>
            </div>
            <div class="stat-card">
                <div class="number">', length(character_vars) + length(factor_vars), '</div>
                <div>Categorical Variables</div>
            </div>
        </div>
        
        <h2>Variable Information</h2>
        <table>
            <tr><th>Variable Name</th><th>Type</th><th>Missing Count</th><th>Missing %</th></tr>')
  
  # Add variable information rows
  for (i in 1:n_cols) {
    var_name <- names(data)[i]
    var_type <- class(data[[i]])[1]
    miss_count <- missing_counts[i]
    miss_pct_val <- missing_pct[i]
    
    html_content <- paste0(html_content, '
            <tr>
                <td>', var_name, '</td>
                <td>', var_type, '</td>
                <td>', miss_count, '</td>
                <td>', miss_pct_val, '%</td>
            </tr>')
  }
  
  html_content <- paste0(html_content, '
        </table>')
  
  # Add numeric statistics if available
  if (!is.null(numeric_stats) && nrow(numeric_stats) > 0) {
    html_content <- paste0(html_content, '
        <h2>Numeric Variables Summary</h2>
        <table>
            <tr><th>Variable</th><th>Mean</th><th>Median</th><th>Std Dev</th><th>Min</th><th>Max</th></tr>')
    
    for (i in 1:nrow(numeric_stats)) {
      row <- numeric_stats[i, ]
      html_content <- paste0(html_content, '
            <tr>
                <td>', row$Variable, '</td>
                <td>', row$Mean, '</td>
                <td>', row$Median, '</td>
                <td>', row$SD, '</td>
                <td>', row$Min, '</td>
                <td>', row$Max, '</td>
            </tr>')
    }
    
    html_content <- paste0(html_content, '
        </table>')
  }
  
  # Add categorical summary
  cat_vars <- c(character_vars, factor_vars)
  if (length(cat_vars) > 0) {
    html_content <- paste0(html_content, '
        <h2>Categorical Variables Summary</h2>')
    
    for (var in head(cat_vars, 5)) {  # Show first 5 categorical variables
      var_table <- table(data[[var]], useNA = "ifany")
      var_df <- as.data.frame(var_table)
      names(var_df) <- c("Category", "Count")
      var_df$Percentage <- round((var_df$Count / sum(var_df$Count)) * 100, 1)
      var_df <- var_df[order(-var_df$Count), ]
      var_df <- head(var_df, 10)  # Top 10 categories
      
      html_content <- paste0(html_content, '
        <h3>', var, '</h3>
        <table>
            <tr><th>Category</th><th>Count</th><th>Percentage</th></tr>')
      
      for (i in 1:nrow(var_df)) {
        row <- var_df[i, ]
        html_content <- paste0(html_content, '
            <tr>
                <td>', row$Category, '</td>
                <td>', row$Count, '</td>
                <td>', row$Percentage, '%</td>
            </tr>')
      }
      
      html_content <- paste0(html_content, '
        </table>')
    }
  }
  
  # Add insights section
  html_content <- paste0(html_content, '
        <h2>Key Insights</h2>
        <div class="summary-box">
            <h3>Data Quality Summary:</h3>
            <ul>')
  
  # Missing data insights
  total_missing <- sum(missing_counts)
  if (total_missing > 0) {
    html_content <- paste0(html_content, '
                <li>Dataset has ', total_missing, ' missing values across all variables</li>')
    
    vars_with_missing <- sum(missing_counts > 0)
    if (vars_with_missing > 0) {
      html_content <- paste0(html_content, '
                <li>', vars_with_missing, ' variables have missing values</li>')
    }
  } else {
    html_content <- paste0(html_content, '
                <li>No missing values detected - excellent data quality!</li>')
  }
  
  # Sample size insights
  if (n_rows < 30) {
    html_content <- paste0(html_content, '
                <li>Small sample size (n=', n_rows, ') - consider statistical power limitations</li>')
  } else if (n_rows > 10000) {
    html_content <- paste0(html_content, '
                <li>Large sample size (n=', format(n_rows, big.mark=","), ') provides good statistical power</li>')
  }
  
  # Variable type insights
  if (length(numeric_vars) > 0) {
    html_content <- paste0(html_content, '
                <li>Dataset contains ', length(numeric_vars), ' numeric variables suitable for statistical analysis</li>')
  }
  
  if (length(cat_vars) > 0) {
    html_content <- paste0(html_content, '
                <li>Dataset contains ', length(cat_vars), ' categorical variables for grouping and segmentation</li>')
  }
  
  html_content <- paste0(html_content, '
            </ul>
        </div>
        
        <h2>Next Steps</h2>
        <div class="summary-box">
            <p><strong>Recommended actions based on this analysis:</strong></p>
            <ol>
                <li><strong>Data Cleaning:</strong> Address missing values if present</li>
                <li><strong>Exploratory Visualization:</strong> Create plots to understand distributions and relationships</li>
                <li><strong>Statistical Analysis:</strong> Apply appropriate tests based on variable types</li>
                <li><strong>Feature Engineering:</strong> Consider transformations for skewed numeric variables</li>
                <li><strong>Further Analysis:</strong> Develop hypotheses based on patterns observed</li>
            </ol>
        </div>
        
        <hr style="margin: 30px 0; border: none; border-top: 1px solid #ddd;">
        <p style="text-align: center; color: #7f8c8d; font-size: 14px;">
            Report generated by eda.mckenzie.page R Package - Automated Exploratory Data Analysis
        </p>
    </div>
</body>
</html>')
  
  # Write HTML file
  writeLines(html_content, output_file)
  
  # Return the path
  return(normalizePath(output_file))
}