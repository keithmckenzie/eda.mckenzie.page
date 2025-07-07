\name{generate_eda_report}
\alias{generate_eda_report}
\title{Generate Comprehensive EDA Report}
\usage{
generate_eda_report(
  data,
  output_file = "eda_report.html",
  output_format = "html",
  title = "Exploratory Data Analysis Report",
  include_ai_insights = TRUE,
  openai_api_key = NULL,
  max_variables = 50,
  sample_size = 10000
)
}
\arguments{
\item{data}{A data frame to analyze}

\item{output_file}{Character. Output file name (default: "eda_report.html")}

\item{output_format}{Character. Output format: "html" or "pdf" (default: "html")}

\item{title}{Character. Report title (default: "Exploratory Data Analysis Report")}

\item{include_ai_insights}{Logical. Whether to include AI-powered insights (default: TRUE)}

\item{openai_api_key}{Character. OpenAI API key (if NULL, uses environment variable)}

\item{max_variables}{Integer. Maximum number of variables to analyze (default: 50)}

\item{sample_size}{Integer. Sample size for large datasets (default: 10000)}
}
\value{
Character. Path to generated report file
}
\description{
Creates a professional exploratory data analysis report with automated
visualizations, statistical summaries, and optional AI-powered insights.
}
\examples{
\dontrun{
# Generate EDA report for mtcars dataset
report_path <- generate_eda_report(mtcars, "mtcars_eda.html")

# Generate PDF report with custom title
report_path <- generate_eda_report(
  data = iris,
  output_file = "iris_analysis.pdf",
  output_format = "pdf",
  title = "Iris Dataset Analysis"
)
}
}
