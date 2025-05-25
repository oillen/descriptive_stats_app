library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(rtf)
library(openxlsx)
library(tidyverse)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Descriptive Statistics Toolkit"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Loading", tabName = "data_loading", icon = icon("upload")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Statistical Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("Export Results", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
      "))
    ),
    
    tabItems(
      # Data Loading Tab
      tabItem(tabName = "data_loading",
              fluidRow(
                box(
                  title = "Load Dataset", status = "primary", solidHeader = TRUE, width = 12,
                  
                  tabsetPanel(
                    tabPanel("Upload File",
                             br(),
                             fileInput("file", "Choose CSV/Excel File",
                                       accept = c(".csv", ".xlsx", ".xls")),
                             
                             checkboxInput("header", "Header", TRUE),
                             
                             conditionalPanel(
                               condition = "input.file != null",
                               radioButtons("sep", "Separator",
                                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                            selected = ",")
                             )
                    ),
                    
                    tabPanel("Load Built-in Dataset",
                             br(),
                             selectInput("builtin_data", "Choose Built-in Dataset:",
                                         choices = c("iris", "mtcars", "airquality", "ChickWeight", "PlantGrowth"),
                                         selected = "iris"),
                             
                             actionButton("load_builtin", "Load Dataset", class = "btn-primary")
                    ),
                    
                    tabPanel("Load from URL",
                             br(),
                             textInput("url_input", "Enter CSV URL:",
                                       placeholder = "https://example.com/data.csv"),
                             
                             actionButton("load_url", "Load from URL", class = "btn-primary")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("data_preview"))
                )
              )
      ),
      
      # Data Overview Tab
      tabItem(tabName = "data_overview",
              fluidRow(
                box(
                  title = "Dataset Summary", status = "primary", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("data_summary")
                ),
                
                box(
                  title = "Dataset Structure", status = "primary", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("data_structure")
                )
              ),
              
              fluidRow(
                box(
                  title = "First Few Rows (head)", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("data_head")
                )
              )
      ),
      
      # Statistical Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Analysis Options", status = "primary", solidHeader = TRUE, width = 4,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    h4("Select Variables:"),
                    uiOutput("variable_selection"),
                    
                    br(),
                    h4("Group By (Optional):"),
                    uiOutput("group_selection"),
                    
                    br(),
                    h4("Statistics to Calculate:"),
                    checkboxGroupInput("stats_selection", "",
                                       choices = list(
                                         "N (Count)" = "n",
                                         "Mean" = "mean",
                                         "Standard Deviation" = "sd",
                                         "CV%" = "cv",
                                         "Median" = "median",
                                         "Minimum" = "min",
                                         "Maximum" = "max",
                                         "Geometric Mean" = "geomean",
                                         "Geometric SD" = "geosd",
                                         "Geometric CV%" = "geocv",
                                         "Q1 (25th percentile)" = "q1",
                                         "Q3 (75th percentile)" = "q3"
                                       ),
                                       selected = c("n", "mean", "sd", "cv", "median", "min", "max")),
                    
                    br(),
                    h4("Decimal Places:"),
                    numericInput("decimal_places", "", value = 2, min = 0, max = 6),
                    
                    br(),
                    actionButton("calculate", "Calculate Statistics", class = "btn-success")
                  )
                ),
                
                box(
                  title = "Statistical Results", status = "success", solidHeader = TRUE, width = 8,
                  withSpinner(DT::dataTableOutput("stats_results"))
                )
              )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "Export Options", status = "primary", solidHeader = TRUE, width = 12,
                  
                  conditionalPanel(
                    condition = "output.results_available",
                    
                    h4("Table Settings:"),
                    textInput("table_title", "Table Title:", value = "Descriptive Statistics"),
                    textInput("table_number", "Table Number:", value = "Table 1"),
                    
                    br(),
                    h4("Export Format:"),
                    radioButtons("export_format", "",
                                 choices = list("CSV" = "csv", "RTF" = "rtf"),
                                 selected = "csv"),
                    
                    conditionalPanel(
                      condition = "input.export_format == 'rtf'",
                      checkboxInput("include_date", "Include generation date", TRUE)
                    ),
                    
                    br(),
                    downloadButton("download_results", "Download Results", class = "btn-primary")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Preview Export", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("export_preview")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    stats_results = NULL
  )
  
  # Data loading functions
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    tryCatch({
      if(ext == "csv") {
        values$data <- read_csv(input$file$datapath, 
                                locale = locale(encoding = "UTF-8"))
      } else if(ext %in% c("xlsx", "xls")) {
        values$data <- read_excel(input$file$datapath)
      }
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_builtin, {
    tryCatch({
      if(input$builtin_data == "iris") {
        values$data <- iris
      } else if(input$builtin_data == "mtcars") {
        values$data <- mtcars
      } else if(input$builtin_data == "airquality") {
        values$data <- airquality
      } else if(input$builtin_data == "ChickWeight") {
        values$data <- ChickWeight
      } else if(input$builtin_data == "PlantGrowth") {
        values$data <- PlantGrowth
      }
      showNotification(paste(input$builtin_data, "dataset loaded successfully!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading dataset:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_url, {
    req(input$url_input)
    tryCatch({
      values$data <- read_csv(input$url_input)
      showNotification("Data loaded from URL successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error loading data from URL", type = "error")
    })
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Data overview outputs
  output$data_summary <- renderPrint({
    req(values$data)
    summary(values$data)
  })
  
  output$data_structure <- renderPrint({
    req(values$data)
    str(values$data)
  })
  
  output$data_head <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 10), options = list(scrollX = TRUE))
  })
  
  # Variable selection UI
  output$variable_selection <- renderUI({
    req(values$data)
    numeric_vars <- names(select_if(values$data, is.numeric))
    
    checkboxGroupInput("selected_vars", "Select Numeric Variables:",
                       choices = numeric_vars,
                       selected = numeric_vars[1:min(3, length(numeric_vars))])
  })
  
  # Group selection UI
  output$group_selection <- renderUI({
    req(values$data)
    factor_vars <- names(select_if(values$data, function(x) is.factor(x) || is.character(x)))
    
    selectInput("group_var", "Select Grouping Variable:",
                choices = c("None" = "", factor_vars),
                selected = "")
  })
  
  # Check if data is loaded
  output$data_loaded <- reactive({
    !is.null(values$data)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Calculate statistics
  observeEvent(input$calculate, {
    req(values$data, input$selected_vars, input$stats_selection)
    
    tryCatch({
      # Calculate statistics function
      calc_stats <- function(x, stats_list, decimal_places) {
        results <- list()
        x_clean <- x[!is.na(x)]
        
        if("n" %in% stats_list) results$N <- length(x_clean)
        if("mean" %in% stats_list) results$Mean <- round(mean(x, na.rm = TRUE), decimal_places)
        if("sd" %in% stats_list) results$SD <- round(sd(x, na.rm = TRUE), decimal_places)
        if("cv" %in% stats_list) {
          mean_val <- mean(x, na.rm = TRUE)
          sd_val <- sd(x, na.rm = TRUE)
          results$`CV%` <- if(mean_val != 0) round((sd_val / mean_val) * 100, decimal_places) else NA
        }
        if("median" %in% stats_list) results$Median <- round(median(x, na.rm = TRUE), decimal_places)
        if("min" %in% stats_list) results$Min <- round(min(x, na.rm = TRUE), decimal_places)
        if("max" %in% stats_list) results$Max <- round(max(x, na.rm = TRUE), decimal_places)
        if("geomean" %in% stats_list) {
          pos_x <- x[x > 0 & !is.na(x)]
          if(length(pos_x) > 0) {
            results$`Geo Mean` <- round(exp(mean(log(pos_x))), decimal_places)
          } else {
            results$`Geo Mean` <- NA
          }
        }
        if("geosd" %in% stats_list) {
          pos_x <- x[x > 0 & !is.na(x)]
          if(length(pos_x) > 0) {
            results$`Geo SD` <- round(exp(sd(log(pos_x))), decimal_places)
          } else {
            results$`Geo SD` <- NA
          }
        }
        if("geocv" %in% stats_list) {
          pos_x <- x[x > 0 & !is.na(x)]
          if(length(pos_x) > 0) {
            geo_mean <- exp(mean(log(pos_x)))
            geo_sd <- exp(sd(log(pos_x)))
            results$`Geo CV%` <- round((geo_sd / geo_mean) * 100, decimal_places)
          } else {
            results$`Geo CV%` <- NA
          }
        }
        if("q1" %in% stats_list) results$Q1 <- round(quantile(x, 0.25, na.rm = TRUE), decimal_places)
        if("q3" %in% stats_list) results$Q3 <- round(quantile(x, 0.75, na.rm = TRUE), decimal_places)
        
        return(results)
      }
      
      # Prepare data subset
      data_subset <- values$data %>%
        select(all_of(input$selected_vars), 
               if(input$group_var != "") all_of(input$group_var) else NULL)
      
      # Calculate statistics
      if(input$group_var == "") {
        # No grouping
        results_list <- list()
        
        for(var in input$selected_vars) {
          stats <- calc_stats(data_subset[[var]], input$stats_selection, input$decimal_places)
          stats_df <- data.frame(Variable = var, stringsAsFactors = FALSE)
          for(stat_name in names(stats)) {
            stats_df[[stat_name]] <- stats[[stat_name]]
          }
          results_list[[var]] <- stats_df
        }
        
        values$stats_results <- do.call(rbind, results_list)
        
      } else {
        # With grouping
        results_list <- list()
        group_var <- input$group_var
        
        for(var in input$selected_vars) {
          var_results <- list()
          unique_groups <- unique(data_subset[[group_var]])
          
          for(group in unique_groups) {
            group_data <- data_subset[data_subset[[group_var]] == group, ]
            stats <- calc_stats(group_data[[var]], input$stats_selection, input$decimal_places)
            
            stats_df <- data.frame(
              Variable = var,
              stringsAsFactors = FALSE
            )
            stats_df[[group_var]] <- group
            
            for(stat_name in names(stats)) {
              stats_df[[stat_name]] <- stats[[stat_name]]
            }
            
            var_results[[as.character(group)]] <- stats_df
          }
          
          results_list[[var]] <- do.call(rbind, var_results)
        }
        
        values$stats_results <- do.call(rbind, results_list)
      }
      
      showNotification("Statistics calculated successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error calculating statistics:", e$message), type = "error")
    })
  })
  
  # Display statistics results
  output$stats_results <- DT::renderDataTable({
    req(values$stats_results)
    DT::datatable(values$stats_results, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  rownames = FALSE)
  })
  
  # Check if results are available
  output$results_available <- reactive({
    !is.null(values$stats_results)
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Export preview
  output$export_preview <- DT::renderDataTable({
    req(values$stats_results)
    DT::datatable(values$stats_results, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("descriptive_stats_", Sys.Date(), ".", input$export_format)
    },
    content = function(file) {
      if(input$export_format == "csv") {
        write.csv(values$stats_results, file, row.names = FALSE)
      } else if(input$export_format == "rtf") {
        # Simple RTF export
        write.csv(values$stats_results, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
