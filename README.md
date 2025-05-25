Descriptive Statistics Toolkit Shiny App

This repository contains a Shiny application for performing and visualizing descriptive statistical analyses on tabular datasets. The app allows users to load data from various sources, explore and summarize the dataset, calculate a wide range of descriptive statistics (with optional grouping), and export results in multiple formats.

Features

Data Loading

Upload CSV and Excel files (XLSX, XLS)

Load built-in R datasets (e.g., iris, mtcars, airquality, ChickWeight, PlantGrowth)

Import CSV data directly from a URL

Data Overview

Summary of variables (mean, median, quartiles, etc.)

Structure of the data frame (str() output)

Interactive table of the first few rows

Statistical Analysis

Select numeric variables and optional grouping factor

Choose which statistics to calculate: count, mean, SD, CV%, median, min, max, geometric stats, percentiles

Specify decimal precision

Interactive results table with pagination and horizontal scrolling

Export Results

Download analysis results as CSV or RTF

Customize table title and number

Option to include generation date in RTF output

Installation

Clone the repository

git clone https://github.com/oillen/descriptive_stats_app.git
cd descriptive-stats-toolkit

Install Dependencies

install.packages(c(
  "shiny", "shinydashboard", "DT", "readr", "readxl", "dplyr",
  "plotly", "shinycssloaders", "shinyWidgets", "rtf", "openxlsx", "tidyverse"
))

Run the App

library(shiny)
runApp("descriptive_stats_app2.R", launch.browser = TRUE)

Or, from the command line:

Rscript -e "shiny::runApp('descriptive_stats_app2.R', launch.browser = TRUE)"

Usage

Data Loading: Go to the "Data Loading" tab.

Upload a local CSV/Excel file, or select a built-in dataset, or enter a URL to load.

View the data preview.

Data Overview: Switch to "Data Overview".

Inspect summary statistics and structure.

Browse the first few rows in an interactive table.

Statistical Analysis: Move to the "Statistical Analysis" tab.

Select numeric variables to analyze.

(Optional) Choose a grouping variable.

Check off the statistics you need and set decimal places.

Click "Calculate Statistics" to generate results.

Export Results: On the "Export Results" tab.

Customize table title and number.

Choose CSV or RTF format (include date if desired).

Download the file and preview it in the table view.

Project Structure

├── descriptive_stats_app2.R   # Main Shiny application script
├── README.md                  # Project overview and instructions (this file)
├── data/                      # (Optional) sample datasets or templates
└── output/                    # (Optional) exported results

Contributing

Contributions, issues, and feature requests are welcome! Please fork the repository and submit a pull request.

Fork the project

Create your feature branch (git checkout -b feature/MyFeature)

Commit your changes (git commit -m 'Add some feature')

Push to the branch (git push origin feature/MyFeature)

Open a pull request

License

This project is licensed under the  Apache-2.0  License. See LICENSE for details.

Developed by [oillen]

