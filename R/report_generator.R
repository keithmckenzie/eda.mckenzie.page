#' Generate Comprehensive EDA Report
#'
#' Creates a detailed HTML report with analysis, visualizations, and insights
#'
#' @param data A data frame to analyze
#' @param output_file Output HTML file path
#' @param title Report title
#' @param include_ai_insights Whether to include AI insights (requires OpenAI API key)
#' @return Path to generated report
#' @export
generate_eda_report <- function(data, output_file = "eda_report.html", title = "Exploratory Data Analysis Report", include_ai_insights = FALSE) {
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Perform comprehensive analysis
  analysis <- analyze_data(data)
  
  # Generate HTML content
  html_content <- generate_html_report(analysis, title, include_ai_insights)
  
  # Write to file
  writeLines(html_content, output_file)
  
  # Return path
  return(normalizePath(output_file))
}

#' Generate HTML Report Content
#'
#' Creates the HTML content for the EDA report
#'
#' @param analysis Analysis results from analyze_data()
#' @param title Report title
#' @param include_ai_insights Whether to include AI insights
#' @return HTML content as character string
generate_html_report <- function(analysis, title, include_ai_insights = FALSE) {
  
  # CSS Styles
  css_styles <- '
    <style>
        body { 
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; 
            margin: 0; 
            padding: 0; 
            background-color: #f8f9fa; 
            line-height: 1.6; 
        }
        .container { 
            max-width: 1200px; 
            margin: 0 auto; 
            background: white; 
            box-shadow: 0 0 20px rgba(0,0,0,0.1); 
        }
        .header { 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
            color: white; 
            padding: 40px; 
            text-align: center; 
        }
        .header h1 { margin: 0; font-size: 2.5em; font-weight: 300; }
        .header p { margin: 10px 0 0 0; opacity: 0.9; }
        .content { padding: 40px; }
        .section { margin-bottom: 40px; }
        .section h2 { 
            color: #2c3e50; 
            border-bottom: 3px solid #3498db; 
            padding-bottom: 10px; 
            margin-bottom: 25px; 
            font-size: 1.8em; 
        }
        .section h3 { 
            color: #34495e; 
            margin-top: 30px; 
            border-left: 4px solid #3498db; 
            padding-left: 15px; 
        }
        .stats-grid { 
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); 
            gap: 20px; 
            margin: 25px 0; 
        }
        .stat-card { 
            background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); 
            padding: 25px; 
            border-radius: 10px; 
            text-align: center; 
            border-left: 5px solid #3498db; 
            transition: transform 0.2s; 
        }
        .stat-card:hover { transform: translateY(-2px); }
        .stat-number { 
            font-size: 2.5em; 
            font-weight: bold; 
            color: #3498db; 
            margin-bottom: 5px; 
        }
        .stat-label { 
            color: #7f8c8d; 
            font-size: 0.9em; 
            text-transform: uppercase; 
            letter-spacing: 1px; 
        }
        table { 
            border-collapse: collapse; 
            width: 100%; 
            margin: 20px 0; 
            background: white; 
            border-radius: 8px; 
            overflow: hidden; 
            box-shadow: 0 2px 10px rgba(0,0,0,0.1); 
        }
        th { 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
            color: white; 
            padding: 15px; 
            text-align: left; 
            font-weight: 600; 
        }
        td { 
            padding: 12px 15px; 
            border-bottom: 1px solid #eee; 
        }
        tr:nth-child(even) { background-color: #f8f9fa; }
        tr:hover { background-color: #e3f2fd; }
        .insight-box { 
            background: linear-gradient(135deg, #e8f5e8 0%, #f0f8f0 100%); 
            border-left: 5px solid #27ae60; 
            padding: 20px; 
            margin: 20px 0; 
            border-radius: 5px; 
        }
        .warning-box { 
            background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%); 
            border-left: 5px solid #f39c12; 
            padding: 20px; 
            margin: 20px 0; 
            border-radius: 5px; 
        }
        .alert-box { 
            background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%); 
            border-left: 5px solid #e74c3c; 
            padding: 20px; 
            margin: 20px 0; 
            border-radius: 5px; 
        }
        .progress-bar { 
            background-color: #e9ecef; 
            border-radius: 10px; 
            overflow: hidden; 
            height: 20px; 
            margin: 5px 0; 
        }
        .progress-fill { 
            background: linear-gradient(90deg, #667eea 0%, #764ba2 100%); 
            height: 100%; 
            transition: width 0.3s ease; 
        }
        .footer { 
            background: #2c3e50; 
            color: white; 
            text-align: center; 
            padding: 20px; 
            font-size: 0.9em; 
        }
        .numeric-viz {
            background: #f8f9fa;
            padding: 15px;
            margin: 10px 0;
            border-radius: 5px;
            border: 1px solid #dee2e6;
        }
    </style>'
  
  # Start HTML document
  html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>', title, '</title>
    ', css_styles, '
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>', title, '</h1>
            <p>Generated on ', format(Sys.time(), "%B %d, %Y at %I:%M %p"), '</p>
            <p>Powered by eda.mckenzie.page - Automated Exploratory Data Analysis</p>
        </div>
        
        <div class="content">')
  
  # Dataset Overview Section
  html_content <- paste0(html_content, '
            <div class="section">
                <h2>Dataset Overview</h2>
                <div class="stats-grid">
                    <div class="stat-card">
                        <div class="stat-number">', format(analysis$dataset_info$n_rows, big.mark = ","), '</div>
                        <div class="stat-label">Total Rows</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-number">', analysis$dataset_info$n_cols, '</div>
                        <div class="stat-label">Total Columns</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-number">', length(analysis$variable_types$numeric), '</div>
                        <div class="stat-label">Numeric Variables</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-number">', length(analysis$variable_types$character) + length(analysis$variable_types$factor), '</div>
                        <div class="stat-label">Categorical Variables</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-number">', analysis$dataset_info$memory_usage, '</div>
                        <div class="stat-label">Memory Usage</div>
                    </div>
                    <div class="stat-card">
                        <div class="stat-number">', ifelse(length(analysis$quality_issues) > 0 && !is.null(analysis$quality_issues$duplicate_rows), analysis$quality_issues$duplicate_rows, 0), '</div>
                        <div class="stat-label">Duplicate Rows</div>
                    </div>
                </div>
            </div>')
  
  # Variable Information Section
  html_content <- paste0(html_content, '
            <div class="section">
                <h2>Variable Information</h2>
                <table>
                    <tr>
                        <th>Variable Name</th>
                        <th>Data Type</th>
                        <th>Missing Count</th>
                        <th>Missing %</th>
                        <th>Completeness</th>
                    </tr>')
  
  # Add variable rows
  for (i in 1:nrow(analysis$missing_analysis)) {
    row <- analysis$missing_analysis[i, ]
    completeness_pct <- 100 - row$missing_percentage
    var_type <- analysis$dataset_info$variable_types[row$variable]
    
    html_content <- paste0(html_content, '
                    <tr>
                        <td><strong>', row$variable, '</strong></td>
                        <td>', var_type, '</td>
                        <td>', row$missing_count, '</td>
                        <td>', row$missing_percentage, '%</td>
                        <td>
                            <div class="progress-bar">
                                <div class="progress-fill" style="width: ', completeness_pct, '%"></div>
                            </div>
                            ', round(completeness_pct, 1), '%
                        </td>
                    </tr>')
  }
  
  html_content <- paste0(html_content, '
                </table>
            </div>')
  
  # Numeric Variables Analysis
  if (!is.null(analysis$numeric_summary) && nrow(analysis$numeric_summary) > 0) {
    html_content <- paste0(html_content, '
            <div class="section">
                <h2>Numeric Variables Analysis</h2>
                <table>
                    <tr>
                        <th>Variable</th>
                        <th>Count</th>
                        <th>Mean</th>
                        <th>Std Dev</th>
                        <th>Min</th>
                        <th>Q25</th>
                        <th>Median</th>
                        <th>Q75</th>
                        <th>Max</th>
                        <th>Range</th>
                    </tr>')
    
    for (i in 1:nrow(analysis$numeric_summary)) {
      row <- analysis$numeric_summary[i, ]
      html_content <- paste0(html_content, '
                    <tr>
                        <td><strong>', row$variable, '</strong></td>
                        <td>', row$n, '</td>
                        <td>', row$mean, '</td>
                        <td>', row$sd, '</td>
                        <td>', row$min, '</td>
                        <td>', row$q25, '</td>
                        <td>', row$median, '</td>
                        <td>', row$q75, '</td>
                        <td>', row$max, '</td>
                        <td>', row$range, '</td>
                    </tr>')
    }
    
    html_content <- paste0(html_content, '
                </table>')
    
    # Add insights for numeric variables
    html_content <- paste0(html_content, '
                <h3>Key Insights for Numeric Variables</h3>')
    
    for (i in 1:nrow(analysis$numeric_summary)) {
      row <- analysis$numeric_summary[i, ]
      var_name <- row$variable
      
      # Generate insights
      insights <- character(0)
      
      if (row$mean > row$median) {
        insights <- c(insights, "Right-skewed distribution (mean > median)")
      } else if (row$mean < row$median) {
        insights <- c(insights, "Left-skewed distribution (mean < median)")
      } else {
        insights <- c(insights, "Approximately symmetric distribution")
      }
      
      if (row$sd > 0) {
        cv <- row$sd / abs(row$mean)
        if (cv > 1) {
          insights <- c(insights, "High variability (CV > 1)")
        } else if (cv < 0.1) {
          insights <- c(insights, "Low variability (CV < 0.1)")
        }
      }
      
      # Check for outliers
      if (!is.null(analysis$outliers[[var_name]]) && analysis$outliers[[var_name]]$count > 0) {
        insights <- c(insights, paste0("Contains ", analysis$outliers[[var_name]]$count, " outliers (", analysis$outliers[[var_name]]$percentage, "%)"))
      }
      
      html_content <- paste0(html_content, '
                <div class="numeric-viz">
                    <h4>', var_name, '</h4>
                    <ul>')
      
      for (insight in insights) {
        html_content <- paste0(html_content, '
                        <li>', insight, '</li>')
      }
      
      html_content <- paste0(html_content, '
                    </ul>
                </div>')
    }
    
    html_content <- paste0(html_content, '
            </div>')
  }
  
  # Categorical Variables Analysis
  if (length(analysis$categorical_summary) > 0) {
    html_content <- paste0(html_content, '
            <div class="section">
                <h2>Categorical Variables Analysis</h2>')
    
    for (var_name in names(analysis$categorical_summary)) {
      var_data <- analysis$categorical_summary[[var_name]]
      
      html_content <- paste0(html_content, '
                <h3>', var_name, '</h3>
                <p><strong>Unique Categories:</strong> ', nrow(var_data), ' (showing top 10)</p>
                <table>
                    <tr>
                        <th>Category</th>
                        <th>Count</th>
                        <th>Percentage</th>
                        <th>Distribution</th>
                    </tr>')
      
      for (i in 1:nrow(var_data)) {
        row <- var_data[i, ]
        html_content <- paste0(html_content, '
                    <tr>
                        <td>', row$category, '</td>
                        <td>', row$n, '</td>
                        <td>', row$percentage, '%</td>
                        <td>
                            <div class="progress-bar">
                                <div class="progress-fill" style="width: ', row$percentage, '%"></div>
                            </div>
                        </td>
                    </tr>')
      }
      
      html_content <- paste0(html_content, '
                </table>')
    }
    
    html_content <- paste0(html_content, '
            </div>')
  }
  
  # Correlation Analysis
  if (!is.null(analysis$correlation_matrix)) {
    html_content <- paste0(html_content, '
            <div class="section">
                <h2>Correlation Analysis</h2>
                <p>Correlation matrix for numeric variables:</p>
                <table>
                    <tr>
                        <th>Variable</th>')
    
    # Header row
    for (var_name in colnames(analysis$correlation_matrix)) {
      html_content <- paste0(html_content, '
                        <th>', var_name, '</th>')
    }
    html_content <- paste0(html_content, '
                    </tr>')
    
    # Data rows
    for (i in 1:nrow(analysis$correlation_matrix)) {
      html_content <- paste0(html_content, '
                    <tr>
                        <td><strong>', rownames(analysis$correlation_matrix)[i], '</strong></td>')
      
      for (j in 1:ncol(analysis$correlation_matrix)) {
        corr_val <- round(analysis$correlation_matrix[i, j], 3)
        color_class <- ""
        if (abs(corr_val) > 0.7 && i != j) {
          color_class <- 'style="background-color: #ffebee; font-weight: bold;"'
        } else if (abs(corr_val) > 0.5 && i != j) {
          color_class <- 'style="background-color: #fff3e0; font-weight: bold;"'
        }
        
        html_content <- paste0(html_content, '
                        <td ', color_class, '>', corr_val, '</td>')
      }
      
      html_content <- paste0(html_content, '
                    </tr>')
    }
    
    html_content <- paste0(html_content, '
                </table>
                <div class="insight-box">
                    <h4>Correlation Insights:</h4>
                    <ul>')
    
    # Find strong correlations
    strong_corrs <- which(abs(analysis$correlation_matrix) > 0.7 & analysis$correlation_matrix != 1, arr.ind = TRUE)
    if (nrow(strong_corrs) > 0) {
      for (i in 1:min(5, nrow(strong_corrs))) {
        row_idx <- strong_corrs[i, 1]
        col_idx <- strong_corrs[i, 2]
        var1 <- rownames(analysis$correlation_matrix)[row_idx]
        var2 <- colnames(analysis$correlation_matrix)[col_idx]
        corr_val <- round(analysis$correlation_matrix[row_idx, col_idx], 3)
        
        html_content <- paste0(html_content, '
                        <li><strong>', var1, '</strong> and <strong>', var2, '</strong> are strongly correlated (r = ', corr_val, ')</li>')
      }
    } else {
      html_content <- paste0(html_content, '
                        <li>No strong correlations (|r| > 0.7) found between variables</li>')
    }
    
    html_content <- paste0(html_content, '
                    </ul>
                </div>
            </div>')
  }
  
  # Data Quality Issues
  html_content <- paste0(html_content, '
            <div class="section">
                <h2>Data Quality Assessment</h2>')
  
  # Missing data summary
  total_missing <- analysis$missing_analysis$missing_count
  if (sum(total_missing) > 0) {
    vars_with_missing <- sum(total_missing > 0)
    html_content <- paste0(html_content, '
                <div class="warning-box">
                    <h4>Missing Data Issues:</h4>
                    <ul>
                        <li><strong>', sum(total_missing), '</strong> total missing values across all variables</li>
                        <li><strong>', vars_with_missing, '</strong> variables have missing values</li>')
    
    # Show variables with high missing percentages
    high_missing <- analysis$missing_analysis[analysis$missing_analysis$missing_percentage > 10, ]
    if (nrow(high_missing) > 0) {
      html_content <- paste0(html_content, '
                        <li>Variables with >10% missing data: ', paste(high_missing$variable, collapse = ", "), '</li>')
    }
    
    html_content <- paste0(html_content, '
                    </ul>
                </div>')
  } else {
    html_content <- paste0(html_content, '
                <div class="insight-box">
                    <h4>No Missing Data</h4>
                    <p>Excellent! Your dataset has no missing values, indicating high data quality.</p>
                </div>')
  }
  
  # Other quality issues
  if (length(analysis$quality_issues) > 0) {
    if (!is.null(analysis$quality_issues$duplicate_rows) && analysis$quality_issues$duplicate_rows > 0) {
      html_content <- paste0(html_content, '
                <div class="alert-box">
                    <h4>Duplicate Rows Detected</h4>
                    <p><strong>', analysis$quality_issues$duplicate_rows, '</strong> duplicate rows found. Consider removing duplicates before analysis.</p>
                </div>')
    }
    
    if (!is.null(analysis$quality_issues$constant_variables)) {
      html_content <- paste0(html_content, '
                <div class="warning-box">
                    <h4>Constant Variables</h4>
                    <p>These variables have only one unique value: <strong>', paste(analysis$quality_issues$constant_variables, collapse = ", "), '</strong></p>
                    <p>Consider removing these variables as they provide no information for analysis.</p>
                </div>')
    }
    
    if (!is.null(analysis$quality_issues$high_cardinality_variables)) {
      html_content <- paste0(html_content, '
                <div class="warning-box">
                    <h4>High Cardinality Variables</h4>
                    <p>These categorical variables have very high cardinality: <strong>', paste(analysis$quality_issues$high_cardinality_variables, collapse = ", "), '</strong></p>
                    <p>Consider grouping rare categories or using different encoding methods.</p>
                </div>')
    }
  }
  
  # Outlier Analysis
  if (length(analysis$outliers) > 0) {
    html_content <- paste0(html_content, '
                <h3>Outlier Detection</h3>')
    
    outlier_found <- FALSE
    for (var_name in names(analysis$outliers)) {
      outlier_info <- analysis$outliers[[var_name]]
      if (outlier_info$count > 0) {
        outlier_found <- TRUE
        html_content <- paste0(html_content, '
                <div class="warning-box">
                    <h4>', var_name, '</h4>
                    <p><strong>', outlier_info$count, '</strong> outliers detected (', outlier_info$percentage, '% of data)</p>
                    <p>Outlier bounds: [', outlier_info$lower_bound, ', ', outlier_info$upper_bound, ']</p>')
        
        if (length(outlier_info$values) > 0) {
          html_content <- paste0(html_content, '
                    <p>Sample outlier values: ', paste(head(outlier_info$values, 5), collapse = ", "), '</p>')
        }
        
        html_content <- paste0(html_content, '
                </div>')
      }
    }
    
    if (!outlier_found) {
      html_content <- paste0(html_content, '
                <div class="insight-box">
                    <h4>No Significant Outliers</h4>
                    <p>No outliers detected using the IQR method (1.5 × IQR rule).</p>
                </div>')
    }
  }
  
  html_content <- paste0(html_content, '
            </div>')
  
  # Recommendations Section
  html_content <- paste0(html_content, '
            <div class="section">
                <h2>General Recommendations & Next Steps</h2>
                <div class="insight-box">
                    <h4>Data Preparation Recommendations:</h4>
                    <ol>')
  
  # Generate specific recommendations based on analysis
  if (sum(analysis$missing_analysis$missing_count) > 0) {
    html_content <- paste0(html_content, '
                        <li><strong>Handle Missing Values:</strong> Address missing data using appropriate imputation methods or removal strategies</li>')
  }
  
  if (!is.null(analysis$quality_issues$duplicate_rows) && analysis$quality_issues$duplicate_rows > 0) {
    html_content <- paste0(html_content, '
                        <li><strong>Remove Duplicates:</strong> Clean duplicate rows to avoid bias in analysis</li>')
  }
  
  outlier_vars <- names(analysis$outliers)[sapply(analysis$outliers, function(x) x$count > 0)]
  if (length(outlier_vars) > 0) {
    html_content <- paste0(html_content, '
                        <li><strong>Investigate Outliers:</strong> Review outliers in ', paste(outlier_vars, collapse = ", "), ' - determine if they are errors or valid extreme values</li>')
  }
  
  if (!is.null(analysis$correlation_matrix)) {
    strong_corrs <- which(abs(analysis$correlation_matrix) > 0.8 & analysis$correlation_matrix != 1, arr.ind = TRUE)
    if (nrow(strong_corrs) > 0) {
      html_content <- paste0(html_content, '
                        <li><strong>Address Multicollinearity:</strong> Consider removing or combining highly correlated variables for modeling</li>')
    }
  }
  
  html_content <- paste0(html_content, '
                        <li><strong>Feature Engineering:</strong> Create new features based on domain knowledge and observed patterns</li>
                        <li><strong>Statistical Testing:</strong> Perform appropriate statistical tests based on your research questions</li>
                        <li><strong>Visualization:</strong> Create detailed plots to explore relationships and distributions</li>
                        <li><strong>Modeling:</strong> Apply machine learning or statistical models appropriate for your objectives</li>
                    </ol>
                </div>
            </div>')
  
  # Footer
  html_content <- paste0(html_content, '
        </div>
        
        <div class="footer">
            <p>Report generated by <strong>eda.mckenzie.page</strong> - McKenzie's Automated Exploratory Data Analysis Package</p>
            <p>https://eda.mckenzie.page.</p>
        </div>
    </div>
</body>
</html>')
  
  return(html_content)
}