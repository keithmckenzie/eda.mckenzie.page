#' Recommend Statistical Tests
#'
#' Provides recommendations for appropriate statistical tests based on data characteristics
#'
#' @param data A data frame to analyze
#' @return List of test recommendations
#' @export
recommend_statistical_tests <- function(data) {
  
  recommendations <- list()
  
  # Identify variable types
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  binary_vars <- names(data)[sapply(data, function(x) length(unique(x[!is.na(x)])) == 2)]
  
  # Test for normality of numeric variables
  normality_tests <- list()
  if (length(numeric_vars) > 0) {
    for (var in head(numeric_vars, 10)) {  # Limit to first 10 variables
      var_data <- data[[var]][!is.na(data[[var]])]
      if (length(var_data) >= 3) {
        tryCatch({
          # Shapiro-Wilk test for normality (sample if too large)
          if (length(var_data) > 5000) {
            var_data <- sample(var_data, 5000)
          }
          shapiro_test <- shapiro.test(var_data)
          
          normality_tests[[var]] <- list(
            test = "Shapiro-Wilk",
            statistic = shapiro_test$statistic,
            p_value = shapiro_test$p.value,
            is_normal = shapiro_test$p.value > 0.05,
            interpretation = ifelse(shapiro_test$p.value > 0.05, 
                                  "Data appears normally distributed", 
                                  "Data does not appear normally distributed")
          )
        }, error = function(e) {
          normality_tests[[var]] <<- list(
            test = "Shapiro-Wilk",
            error = "Could not perform test",
            interpretation = "Test failed - check data quality"
          )
        })
      }
    }
  }
  
  recommendations$normality_tests <- normality_tests
  
  # Correlation tests
  correlation_recommendations <- list()
  if (length(numeric_vars) >= 2) {
    correlation_recommendations$pearson <- list(
      description = "Pearson correlation for linear relationships between numeric variables",
      applicable_when = "Both variables are numeric and approximately normally distributed",
      interpretation = "Measures linear correlation strength (-1 to 1)"
    )
    
    correlation_recommendations$spearman <- list(
      description = "Spearman correlation for monotonic relationships",
      applicable_when = "Variables are numeric but not necessarily normally distributed",
      interpretation = "Measures monotonic correlation strength (-1 to 1)"
    )
  }
  
  # Comparison tests
  comparison_recommendations <- list()
  
  # Two-sample tests
  if (length(numeric_vars) > 0 && length(binary_vars) > 0) {
    comparison_recommendations$t_test <- list(
      description = "Independent samples t-test",
      applicable_when = "Comparing means of numeric variable between two groups (normally distributed)",
      interpretation = "Tests if group means are significantly different"
    )
    
    comparison_recommendations$mann_whitney <- list(
      description = "Mann-Whitney U test (Wilcoxon rank-sum)",
      applicable_when = "Comparing distributions between two groups (non-parametric)",
      interpretation = "Tests if one group tends to have larger values than the other"
    )
  }
  
  # ANOVA
  if (length(numeric_vars) > 0 && length(categorical_vars) > 0) {
    comparison_recommendations$anova <- list(
      description = "Analysis of Variance (ANOVA)",
      applicable_when = "Comparing means across multiple groups (normally distributed)",
      interpretation = "Tests if at least one group mean differs significantly"
    )
    
    comparison_recommendations$kruskal_wallis <- list(
      description = "Kruskal-Wallis test",
      applicable_when = "Comparing distributions across multiple groups (non-parametric)",
      interpretation = "Non-parametric alternative to ANOVA"
    )
  }
  
  # Chi-square tests
  if (length(categorical_vars) >= 2) {
    comparison_recommendations$chi_square <- list(
      description = "Chi-square test of independence",
      applicable_when = "Testing association between two categorical variables",
      interpretation = "Tests if variables are independent or associated"
    )
  }
  
  recommendations$correlation_tests <- correlation_recommendations
  recommendations$comparison_tests <- comparison_recommendations
  
  # General recommendations based on data characteristics
  general_recommendations <- list()
  
  # Sample size considerations
  n <- nrow(data)
  if (n < 30) {
    general_recommendations$sample_size <- "Small sample size (n < 30). Consider non-parametric tests and be cautious with assumptions."
  } else if (n > 10000) {
    general_recommendations$sample_size <- "Large sample size (n > 10000). Even small effects may be statistically significant."
  }
  
  # Missing data considerations
  missing_percentage <- mean(rowSums(is.na(data)) > 0) * 100
  if (missing_percentage > 10) {
    general_recommendations$missing_data <- paste(
      "High proportion of missing data (", round(missing_percentage, 1), 
      "%). Consider missing data mechanisms and imputation strategies."
    )
  }
  
  # Outlier considerations
  if (length(numeric_vars) > 0) {
    outlier_vars <- sapply(numeric_vars, function(var) {
      var_data <- data[[var]][!is.na(data[[var]])]
      if (length(var_data) > 0) {
        Q1 <- quantile(var_data, 0.25)
        Q3 <- quantile(var_data, 0.75)
        IQR <- Q3 - Q1
        sum(var_data < (Q1 - 1.5 * IQR) | var_data > (Q3 + 1.5 * IQR)) > 0
      } else {
        FALSE
      }
    })
    
    if (any(outlier_vars)) {
      general_recommendations$outliers <- "Outliers detected in numeric variables. Consider robust statistical methods or outlier treatment."
    }
  }
  
  recommendations$general_recommendations <- general_recommendations
  
  return(recommendations)
}
