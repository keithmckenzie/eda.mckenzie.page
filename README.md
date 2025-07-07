# eda.mckenzie.page: Simple Automated Exploratory Data Analysis.

eda.mckenzie.page is a comprehensive R package that automates exploratory data analysis (EDA) and generates professional reports with optional AI-powered insights. Transform your data analysis workflow with automated visualizations, statistical summaries, and intelligent interpretation of findings.

## Key Features

-   **Optional AI-Powered Insights**: Integration with OpenAI to provide plain English explanations of data patterns, outliers, data leakage, and potential issues
-   **Statistical Analysis**: Comprehensive descriptive statistics, outlier detection, and normality testing
-   **Professional Reports**: Beautiful HTML and PDF reports with embedded visualizations and narrative text
-   **Data Quality Assessment**: Automatic detection of missing values, outliers, and data quality issues
-   **Statistical Test Recommendations**: Intelligent suggestions for appropriate statistical tests based on data characteristics
-   **Built-in Statistical Tests**: Automated normality testing, correlation analysis, and statistical test selection
-   **Easy to Use**: Generate comprehensive EDA reports with a single function call

## Quick Start

### Install devtools
``` r
install.packages("devtools")
```

### Install dependencies
``` r
install.packages(c(
     "ggplot2", "dplyr", "tidyr", "tibble", 
     "rmarkdown", "knitr", "DT", "httr", 
     "jsonlite", "corrplot", "VIM", "psych", 
     "gridExtra", "scales", "plotly", "htmlwidgets",
     "devtools", "testthat"
   ))
   ```

### Installation

``` r
# install development version from GitHub
devtools::install_github("keithmckenzie/eda.mckenzie.page")
```

### Commands

``` r
# list of commands
analyze_data(dataname)

generate_eda_report(data, output_file = "eda_report.html", title = "Exploratory Data Analysis Report", include_ai_insights = FALSE)

recommend_statistical_tests(dataname)

simple_eda_report(dataname)

get_ai_insights(analysis reults, api_key= putapikeyhere)
```

### Basic Usage

``` r
library(eda.mckenzie.page)

# Generate a comprehensive EDA report with one line of code
report_path <- generate_eda_report(
  data = mtcars,
  output_file = "my_analysis.html",
  title = "My Data Analysis Report"
)

# View the report
browseURL(report_path)
```

### Advanced Usage with AI Insights

``` r
# Set up OpenAI API key for AI-powered insights
Sys.setenv(OPENAI_API_KEY = "your-openai-api-key-here")

# Generate report with AI insights
report_path <- generate_eda_report(
  data = your_dataset,
  output_file = "comprehensive_analysis.html",
  title = "AI-Enhanced Data Analysis",
  include_ai_insights = TRUE,
  output_format = "html"  # or "pdf"
)
```

## What You Get

### Automated Analysis

-   **Dataset Overview**: Rows, columns, variable types, memory usage
-   **Missing Values Analysis**: Detection and visualization of missing data patterns
-   **Data Quality Assessment**: Duplicate rows, constant variables, high cardinality issues
-   **Descriptive Statistics**: Mean, median, standard deviation, skewness, kurtosis for numeric variables
-   **Categorical Analysis**: Frequency tables and distributions for categorical variables
-   **Outlier Detection**: IQR-based outlier identification with statistical boundaries

### Statistical Insights

-   **Normality Testing**: Shapiro-Wilk tests with interpretations
-   **Test Recommendations**: Suggestions for appropriate statistical tests based on data characteristics
-   **Correlation Guidelines**: Advice on Pearson vs Spearman correlation
-   **Sample Size Considerations**: Warnings and recommendations based on dataset size

### AI-Powered Intelligence

-   **Plain English Explanations**: AI interprets statistical findings in understandable language
-   **Data Quality Insights**: AI identifies potential issues and suggests solutions
-   **Pattern Recognition**: AI highlights important trends and relationships
-   **Analysis Recommendations**: AI suggests next steps for deeper analysis
-   **Data Leakage Detection**: AI flags potential data leakage concerns

## Package Structure

```         
eda.mckenzie.page/
├── R/                          # Core R functions
│   ├── easyeda.R              # Main package and generate_eda_report()
│   ├── data_analysis.R        # analyze_data() function
│   ├── visualizations.R       # Plot generation functions
│   ├── ai_insights.R          # OpenAI integration
│   ├── statistical_tests.R    # Test recommendations
│   ├── report_generator.R     # R Markdown report generation
│   └── utils.R                # Utility functions
├── inst/rmd/                  # Report templates
│   └── eda_template.Rmd       # Professional report template
├── man/                       # Documentation
├── tests/testthat/            # Unit tests
├── vignettes/                 # Package vignettes
├── DESCRIPTION                # Package metadata
├── NAMESPACE                  # Package exports
└── README.md                  # This file
```

## Core Functions

### Main Function

-   `generate_eda_report()`: One-stop function for complete EDA report generation

### Analysis Functions

-   `analyze_data()`: Comprehensive data analysis and quality assessment
-   `recommend_statistical_tests()`: Statistical test recommendations based on data
-   `get_ai_insights()`: AI-powered interpretation of analysis results

## AI Integration

The package integrates with OpenAI's GPT models to provide intelligent insights:

``` r
# Enable AI insights
Sys.setenv(OPENAI_API_KEY = "your-api-key")

# AI will analyze your data and provide insights like:
# - "The income variable shows high skewness (3.4), suggesting 
#    log transformation may improve normality"
# - "Significant outliers detected in purchase_amount (4.5% of data), 
#    consider robust statistical methods"
# - "High correlation between age and income (r=0.78) suggests 
#    potential multicollinearity in modeling"
```

## Development

### Requirements

-   R (\>= 4.0.0)
-   Required packages: ggplot2, dplyr, tidyr, rmarkdown, knitr, and others (see DESCRIPTION)
-   Optional: OpenAI API key for AI insights
-   Optional: LaTeX installation for PDF reports

### Contributing

1.  Fork the repository
2.  Create a feature branch
3.  Add tests for new functionality
4.  Ensure all tests pass
5.  Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

-   Built with the R ecosystem including ggplot2, dplyr, and rmarkdown
-   AI insights powered by OpenAI's GPT models

**eda.mckenzie.page: Making data analysis accessible, automated, and intelligent.**
