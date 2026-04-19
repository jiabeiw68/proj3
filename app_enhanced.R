# app.R
# Deployable Shiny app converted from the user's R Markdown A/B testing version.
# Use:
#   default  -> randomly assigned A/B once per browser tab session
#   version A -> /?group=A
#   version B -> /?group=B
#
# To deploy with rsconnect from this folder:
#   rsconnect::deployApp()
#
# Google Analytics setup:
#   Google tag is manually installed in this file using:
#     G-02GZHF7V6T
#   No Sys.setenv(...) step is required.
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
# app.R
# =========================================================
# Project 2 - Web Application Development and Deployment
# Updated version:
# 1. Replaced deprecated aes_string() with aes() + .data[[...]]
# 2. Reduced plotting warnings by filtering non-finite values before plotting
# =========================================================

library(ggplot2)
library(plotly)
library(readxl)
library(jsonlite)
library(tidyr)
library(stringr)
library(scales)
library(reshape2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


# =========================
# A/B assignment + Google Analytics (GA4)
# Manual tag installation using the Measurement ID from Google Analytics
# GA4 setup recommended after deployment:
#   Event-scoped custom dimensions:
#     - app_group
#     - step_name
#     - completion_type
#     - data_source
#     - dataset_name
#     - file_name
#     - last_step
#   Event-scoped custom metric:
#     - dwell_seconds
#     - total_session_seconds
# =========================
GA_MEASUREMENT_ID <- "G-0WSLD1GV8T"

ga_head_tags <- function() {
  if (!nzchar(GA_MEASUREMENT_ID)) return(NULL)

  tags$head(
    tags$script(
      async = NA,
      src = sprintf("https://www.googletagmanager.com/gtag/js?id=%s", GA_MEASUREMENT_ID)
    ),
    tags$script(HTML(sprintf("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', '%s');

      window.gaSafeSend = function(eventName, params) {
        if (typeof gtag === 'function') {
          gtag('event', eventName, params || {});
        }
      };

      if (window.Shiny) {
        Shiny.addCustomMessageHandler('ga_event', function(message) {
          var eventName = message.event_name || 'custom_event';
          var params = Object.assign({}, message);
          delete params.event_name;
          window.gaSafeSend(eventName, params);
        });
      }
    ", GA_MEASUREMENT_ID)))
  )
}


# =========================
# Helper functions
# =========================

clean_names_custom <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-zA-Z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- make.unique(x, sep = "_")
  x
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

is_binary_like <- function(x) {
  ux <- unique(na.omit(x))
  length(ux) <= 2
}

read_uploaded_data <- function(path, ext) {
  ext <- tolower(ext)

  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(path)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else if (ext == "json") {
    obj <- fromJSON(path, flatten = TRUE)

    if (is.data.frame(obj)) {
      df <- obj
    } else if (is.list(obj) && length(obj) > 0) {
      if (all(sapply(obj, function(z) is.atomic(z) || is.null(z)))) {
        df <- as.data.frame(obj, stringsAsFactors = FALSE)
      } else {
        df <- as.data.frame(bind_rows(obj), stringsAsFactors = FALSE)
      }
    } else {
      stop("Unsupported JSON structure. Please upload a record-style JSON file.")
    }
  } else if (ext == "rds") {
    obj <- readRDS(path)
    if (is.data.frame(obj)) {
      df <- obj
    } else if (is.matrix(obj)) {
      df <- as.data.frame(obj, stringsAsFactors = FALSE)
    } else if (is.list(obj)) {
      df <- as.data.frame(obj, stringsAsFactors = FALSE)
    } else {
      stop("RDS file does not contain a data frame, matrix, or convertible list.")
    }
  } else {
    stop("Unsupported file type.")
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- clean_names_custom(names(df))

  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]][trimws(df[[col]]) %in% c("", "NA", "N/A", "null", "NULL")] <- NA
    }
  }

  df
}

cap_outliers_iqr <- function(x) {
  if (!is.numeric(x)) return(x)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  x <- ifelse(x < lower, lower, x)
  x <- ifelse(x > upper, upper, x)
  x
}

remove_outlier_rows_iqr <- function(df, cols) {
  if (length(cols) == 0) return(df)
  keep <- rep(TRUE, nrow(df))

  for (col in cols) {
    x <- df[[col]]
    if (is.numeric(x)) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      keep <- keep & (is.na(x) | (x >= lower & x <= upper))
    }
  }
  df[keep, , drop = FALSE]
}

scale_minmax <- function(x) {
  if (!is.numeric(x)) return(x)
  rng <- range(x, na.rm = TRUE)
  if (is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) return(x)
  (x - rng[1]) / (rng[2] - rng[1])
}

scale_zscore <- function(x) {
  if (!is.numeric(x)) return(x)
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x)
  (x - m) / s
}

roll_mean_vec <- function(x, w) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (w < 2L || n == 0L) return(out)
  w <- as.integer(w)
  for (i in w:n) {
    out[i] <- mean(x[(i - w + 1L):i], na.rm = TRUE)
  }
  out
}

safe_eval_formula <- function(formula_text, df) {
  env <- new.env(parent = emptyenv())

  env$log <- log
  env$log1p <- log1p
  env$sqrt <- sqrt
  env$abs <- abs
  env$sin <- sin
  env$cos <- cos
  env$tan <- tan
  env$exp <- exp
  env$round <- round
  env$floor <- floor
  env$ceiling <- ceiling
  env$ifelse <- ifelse
  env$pmin <- pmin
  env$pmax <- pmax
  env$mean <- mean
  env$median <- median
  env$sd <- sd
  env$sum <- sum
  env$as.numeric <- as.numeric
  env$as.character <- as.character

  for (nm in names(df)) {
    env[[nm]] <- df[[nm]]
  }

  eval(parse(text = formula_text), envir = env)
}

plotly_empty <- function(msg = "Not enough valid data to display this plot.") {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        x = 0.5, y = 0.5, text = msg, showarrow = FALSE,
        xref = "paper", yref = "paper", font = list(size = 16)
      )
    )
}


build_session_summary <- function(event_log, session_id, app_group, session_start, session_end, data_source, file_uploaded) {
  if (is.null(event_log)) {
    event_log <- data.frame()
  }

  total_session_seconds <- round(as.numeric(difftime(session_end, session_start, units = "secs")), 2)
  total_events <- if (nrow(event_log) > 0) nrow(event_log) else 0
  steps_visited <- if (nrow(event_log) > 0 && "step_name" %in% names(event_log)) {
    length(unique(na.omit(event_log$step_name[event_log$step_name != ""])))
  } else 0
  last_step <- if (nrow(event_log) > 0 && "step_name" %in% names(event_log)) {
    vals <- na.omit(event_log$step_name[event_log$step_name != ""])
    if (length(vals) == 0) NA_character_ else tail(vals, 1)
  } else NA_character_
  task_completed <- if (nrow(event_log) > 0 && "event_name" %in% names(event_log)) {
    as.integer(any(event_log$event_name %in% c("task_completed", "download_result")))
  } else 0

  data.frame(
    session_id = session_id,
    app_group = app_group,
    session_start = as.character(session_start),
    session_end = as.character(session_end),
    total_session_seconds = total_session_seconds,
    total_events = total_events,
    steps_visited = steps_visited,
    last_step = ifelse(is.na(last_step), "", last_step),
    data_source = data_source,
    file_uploaded = as.integer(isTRUE(file_uploaded)),
    task_completed = task_completed,
    stringsAsFactors = FALSE
  )
}

build_quality_summary <- function(raw_df, cleaned_df, event_log, summary_df) {
  get_nrow <- function(x) if (is.null(x)) NA_integer_ else nrow(x)
  get_ncol <- function(x) if (is.null(x)) NA_integer_ else ncol(x)
  get_missing <- function(x) if (is.null(x)) NA_integer_ else sum(is.na(x))
  get_dups <- function(x) if (is.null(x)) NA_integer_ else sum(duplicated(x))

  total_events <- if (!is.null(event_log) && nrow(event_log) > 0) nrow(event_log) else 0
  total_session_seconds <- if (!is.null(summary_df) && nrow(summary_df) > 0 && "total_session_seconds" %in% names(summary_df)) summary_df$total_session_seconds[1] else NA_real_
  steps_visited <- if (!is.null(summary_df) && nrow(summary_df) > 0 && "steps_visited" %in% names(summary_df)) summary_df$steps_visited[1] else NA_integer_
  task_completed <- if (!is.null(summary_df) && nrow(summary_df) > 0 && "task_completed" %in% names(summary_df)) summary_df$task_completed[1] else 0

  data.frame(
    Metric = c(
      "rows_before", "rows_after",
      "columns_before", "columns_after",
      "missing_before", "missing_after",
      "duplicates_before", "duplicates_after",
      "total_events", "steps_visited",
      "total_session_seconds", "task_completed"
    ),
    Value = c(
      get_nrow(raw_df), get_nrow(cleaned_df),
      get_ncol(raw_df), get_ncol(cleaned_df),
      get_missing(raw_df), get_missing(cleaned_df),
      get_dups(raw_df), get_dups(cleaned_df),
      total_events, steps_visited,
      total_session_seconds, task_completed
    ),
    stringsAsFactors = FALSE
  )
}

# ------ 456 add ------
top_levels <- function(x, n = 8) {
  x <- as.character(x)
  x[is.na(x)] <- "Missing"
  freq <- sort(table(x), decreasing = TRUE)
  keep <- names(freq)[seq_len(min(n, length(freq)))]
  x[!(x %in% keep)] <- "Other"
  factor(x, levels = c(keep, "Other")[c(keep, "Other") %in% unique(x)])
}

numeric_profile_table <- function(x) {
  x_valid <- x[is.finite(x)]
  
  if (length(x_valid) == 0) {
    return(data.frame(
      Metric = c("Valid N", "Missing", "Mean", "Median", "SD", "Min", "Q1", "Q3", "Max"),
      Value = c(0, sum(is.na(x)), rep(NA, 7))
    ))
  }
  
  data.frame(
    Metric = c("Valid N", "Missing", "Mean", "Median", "SD", "Min", "Q1", "Q3", "Max"),
    Value = c(
      length(x_valid),
      sum(is.na(x)),
      round(mean(x_valid), 4),
      round(median(x_valid), 4),
      round(sd(x_valid), 4),
      round(min(x_valid), 4),
      round(quantile(x_valid, 0.25), 4),
      round(quantile(x_valid, 0.75), 4),
      round(max(x_valid), 4)
    )
  )
}

# ------ 456 add ------

# =========================
# UI
# =========================

make_ui <- function(group = "A") {

  dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Data Explorer Pro"),
  dashboardSidebar(
    width = 230,
    sidebarMenu(id = "tabs",
      menuItem("User Guide", tabName = "guide", icon = icon("book")),
      menuItem("Load Data", tabName = "load", icon = icon("upload")),
      menuItem("Cleaning & Preprocessing", tabName = "clean", icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feature", icon = icon("gears")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  dashboardBody(
    if (!is.null(ga_head_tags())) ga_head_tags(),
tags$head(
  tags$style(HTML("
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow-x: hidden;
    }

    body .wrapper {
      min-height: 100vh;
    }

    .main-container.container-fluid {
      max-width: 100%;
      width: 100%;
      padding-left: 0;
      padding-right: 0;
      margin: 0;
    }

    .content-wrapper, .right-side {
      min-height: calc(100vh - 50px);
      background-color: #f6f8fb;
    }

    .box {
      border-radius: 12px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      border: none;
    }

    .small-box {
      border-radius: 12px;
    }

    .dataTables_wrapper {
      overflow-x: auto;
    }

    .help-note {
      background: #eef5ff;
      border-left: 5px solid #3c8dbc;
      padding: 12px;
      border-radius: 8px;
      margin-bottom: 12px;
      line-height: 1.5;
    }

    .section-card {
      background: #ffffff;
      border: 1px solid #e5e9f0;
      border-radius: 12px;
      padding: 14px 14px 10px 14px;
      margin-bottom: 16px;
    }

    .section-title {
      font-weight: 700;
      font-size: 16px;
      color: #1f2d3d;
      margin-bottom: 8px;
    }

    .section-note {
      font-size: 12px;
      color: #6c757d;
      background: #f8f9fb;
      border: 1px solid #e6eaf0;
      border-radius: 8px;
      padding: 8px 10px;
      margin-bottom: 12px;
      line-height: 1.45;
    }

    .section-divider {
      margin: 16px 0;
      border-top: 1px solid #edf1f5;
    }

    .result-block-title {
      font-weight: 700;
      font-size: 15px;
      margin-bottom: 8px;
      color: #1f2d3d;
    }

    .result-note {
      font-size: 12px;
      color: #6c757d;
      margin-bottom: 12px;
    }

    .clean-main-note {
      font-size: 13px;
      color: #4f5b67;
      background: #f8fbff;
      border: 1px solid #dbe8f6;
      border-radius: 10px;
      padding: 10px 12px;
      margin-bottom: 16px;
      line-height: 1.5;
    }

    .box-header.with-border {
      font-weight: 700;
    }

    .b-next-wrap {
      margin-top: 16px;
      text-align: right;
    }

    .b-only {
      display: none;
    }

    body.ab-group-b .b-only {
      display: block !important;
    }

    body.ab-group-b .main-sidebar,
    body.ab-group-b .left-side {
      display: none !important;
      transform: translate(-230px, 0) !important;
    }

    body.ab-group-b .content-wrapper,
    body.ab-group-b .right-side,
    body.ab-group-b .main-footer {
      margin-left: 0 !important;
    }

    body.ab-group-b.fixed .content-wrapper,
    body.ab-group-b.fixed .right-side {
      padding-top: 0;
    }
  ")),
  tags$script(HTML("
    (function() {
      function resolveGroup() {
        var params = new URLSearchParams(window.location.search || '');
        var urlGroup = (params.get('group') || '').toUpperCase();
        if (urlGroup === 'A' || urlGroup === 'B') return urlGroup;

        // Randomize once per browser tab/window session
        var saved = sessionStorage.getItem('ab_group');
        if (saved === 'A' || saved === 'B') return saved;

        var assigned = Math.random() < 0.5 ? 'A' : 'B';
        sessionStorage.setItem('ab_group', assigned);
        return assigned;
      }

      var group = resolveGroup();

      function applyGroup() {
        if (!document.body) return;
        document.body.classList.remove('ab-group-a', 'ab-group-b');
        document.body.classList.add('ab-group-' + group.toLowerCase());

        var textEls = document.querySelectorAll('.ab-group-text');
        textEls.forEach(function(el) { el.textContent = group; });

        var descEls = document.querySelectorAll('.ab-mode-description');
        descEls.forEach(function(el) {
          el.textContent = group === 'B'
            ? 'You are viewing Version B: the sidebar is hidden and users move step by step with Next buttons.'
            : 'You are viewing Version A: the original version with a visible sidebar and free navigation.';
        });

        if (window.Shiny && window.Shiny.setInputValue) {
          Shiny.setInputValue('client_group', group, {priority: 'event'});
        }
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', applyGroup);
      } else {
        applyGroup();
      }

      window.addEventListener('load', applyGroup);
      setTimeout(applyGroup, 300);
    })();
  "))
),
    tabItems(

      # -------------------------
      # Guide
      # -------------------------
      tabItem(
        tabName = "guide",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Project Overview / How to Use This App",
            div(class = "help-note",
                tags$strong("Purpose: "),
                "This app supports end-to-end interactive data analysis, including dataset upload, cleaning, preprocessing, feature engineering, EDA, and export."
            ),
            div(class = "help-note",
                tags$strong("A/B Mode: "),
                tags$span(class = "ab-mode-description", "Loading version...")
            ),
            tags$ol(
              tags$li("Go to ", tags$strong("Load Data"), " and upload a CSV / Excel / JSON / RDS file, or use a built-in dataset."),
              tags$li("In ", tags$strong("Cleaning & Preprocessing"), ", remove duplicates, handle missing values, treat outliers, scale numeric variables, and encode categorical variables."),
              tags$li("In ", tags$strong("Feature Engineering"), ", use formulas (with preview), transforms, pairs, binning, rolling means, text features; inspect distributions; ", tags$strong("Reset"), " restores the cleaned table."),
              tags$li("In ", tags$strong("EDA"), ", interactively explore summary statistics, missingness, distributions, scatter plots, categorical plots, and correlations."),
              tags$li("In ", tags$strong("Export"), ", download the fully processed dataset.")
            ),
            tags$hr(),
            tags$h4("Recommended Workflow"),
            tags$p("Upload data → Inspect structure → Clean and preprocess → Engineer useful features → Explore patterns and relationships → Export processed data."),
            tags$h4("Tooltips / Tips"),
            tags$ul(
              tags$li("Formulas: use column names and safe functions (log, sqrt, ifelse, ...). Example: ", tags$code("col_a / col_b"), ". Built-in iris uses names like ", tags$code("sepal_length"), " after cleaning."),
              tags$li("For one-hot encoding, the app automatically creates dummy columns for selected categorical variables."),
              tags$li("Correlation heatmap requires at least 2 numeric variables."),
              tags$li("Outlier handling uses the IQR rule.")
            ),
            div(class = "b-only b-next-wrap",
                actionButton("next_from_guide", "Next: Load Data", icon = icon("arrow-right"))
            )
          )
        )
      ),
 # -------------------------
      # Load data
      # -------------------------
      tabItem(
        tabName = "load",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Data Source",
            radioButtons(
              "data_source", "Choose data source:",
              choices = c("Built-in dataset" = "builtin", "Upload your own dataset" = "upload"),
              selected = "builtin"
            ),
            conditionalPanel(
              condition = "input.data_source == 'builtin'",
              selectInput(
                "builtin_data", "Built-in dataset:",
                choices = c("iris", "mtcars", "airquality")
              )
            ),
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput(
                "file", "Upload dataset",
                accept = c(".csv", ".xlsx", ".xls", ".json", ".rds")
              )
            ),
            helpText("Supported formats: CSV, Excel, JSON, RDS.")
          ),
          valueBoxOutput("vb_rows", width = 2),
          valueBoxOutput("vb_cols", width = 2),
          valueBoxOutput("vb_missing", width = 2),
          valueBoxOutput("vb_dups", width = 2)
        ),
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Raw Data Preview",
            DTOutput("raw_preview")
          )
        ),
        fluidRow(
          box(
            width = 6, status = "warning", solidHeader = TRUE,
            title = "Data Structure",
            verbatimTextOutput("raw_structure")
          ),
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Quick Summary",
            tableOutput("quick_summary")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            div(class = "b-only b-next-wrap",
                actionButton("next_from_load", "Next: Cleaning & Preprocessing", icon = icon("arrow-right"))
            )
          )
        )
      ),

      # -------------------------
      # Cleaning & preprocessing
      # -------------------------
tabItem(
  tabName = "clean",
  fluidRow(
    box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = "Preprocessing Options",

      div(
        class = "clean-main-note",
        tags$strong("How this panel works: "),
        "Choose the preprocessing steps on the left. The cleaned dataset updates automatically, and the right panel shows the changes in summary, log, and preview form."
      ),

      div(
        class = "section-card",
        div(class = "section-title", "Step 1: Basic Cleaning"),
        div(
          class = "section-note",
          "Use this step to standardize variable names and remove exact duplicate rows before handling missing values or transformations."
        ),
        checkboxInput("standardize_names", "Standardize column names", TRUE),
        checkboxInput("remove_duplicates", "Remove duplicate rows", TRUE)
      ),

      div(
        class = "section-card",
        div(class = "section-title", "Step 2: Missing Values"),
        div(
          class = "section-note",
          "Choose how missing values should be handled separately for numeric and categorical variables. Row deletion removes observations with any missing value in the dataset."
        ),
        selectInput(
          "missing_num_method", "Numeric columns:",
          choices = c("Do nothing" = "none",
                      "Drop rows with any missing values in the dataset" = "drop_rows",
                      "Mean imputation" = "mean",
                      "Median imputation" = "median")
        ),
        selectInput(
          "missing_cat_method", "Categorical columns:",
          choices = c("Do nothing" = "none",
                      "Drop rows with any missing values in the dataset" = "drop_rows",
                      "Mode imputation" = "mode",
                      "Create 'Missing' category" = "missing_level")
        )
      ),

      div(
        class = "section-card",
        div(class = "section-title", "Step 3: Outlier Handling"),
        div(
          class = "section-note",
          "Select numeric columns and decide whether to cap extreme values or remove rows using the IQR rule."
        ),
        uiOutput("numeric_cols_ui_clean"),
        selectInput(
          "outlier_method", "Outlier handling for selected numeric columns:",
          choices = c("Do nothing" = "none",
                      "Cap using IQR rule" = "cap",
                      "Remove outlier rows using IQR rule" = "remove")
        )
      ),

      div(
        class = "section-card",
        div(class = "section-title", "Step 4: Scaling"),
        div(
          class = "section-note",
          "Scale selected numeric columns for modeling or comparison. Z-score standardization and Min-Max scaling are both available."
        ),
        pickerInput(
          "scale_cols", "Numeric columns to scale:",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        selectInput(
          "scale_method", "Scaling method:",
          choices = c("Do nothing" = "none",
                      "Z-score standardization" = "zscore",
                      "Min-Max scaling" = "minmax")
        )
      ),

      div(
        class = "section-card",
        div(class = "section-title", "Step 5: Encoding"),
        div(
          class = "section-note",
          "Encode selected categorical columns for downstream analysis or modeling. One-hot encoding is usually safer when categories should not imply order."
        ),
        pickerInput(
          "encode_cols", "Categorical columns to encode:",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        selectInput(
          "encoding_method", "Encoding method:",
          choices = c("Do nothing" = "none",
                      "Integer / label encoding" = "label",
                      "One-hot encoding" = "onehot")
        )
      )
    ),

    box(
      width = 8, status = "info", solidHeader = TRUE,
      title = "Preprocessing Feedback",

      div(
        class = "clean-main-note",
        tags$strong("Result view: "),
        "Use this panel to compare the original and cleaned datasets. The summary gives a quick before/after comparison, the cleaning log records the operations applied, and the preview shows the processed table."
      ),

      div(
        class = "section-card",
        div(class = "result-block-title", "Before vs After Summary"),
        div(class = "result-note", "A compact comparison of dataset size, variable count, and missingness before and after preprocessing."),
        tableOutput("preprocess_summary")
      ),

      div(
        class = "section-card",
        div(class = "result-block-title", "Cleaning Log"),
        div(class = "result-note", "A running record of preprocessing actions applied to the dataset."),
        verbatimTextOutput("cleaning_log")
      ),

      div(
        class = "section-card",
        div(class = "result-block-title", "Cleaned Data Preview"),
        div(class = "result-note", "Preview of the cleaned dataset after the selected preprocessing steps are applied."),
        DTOutput("clean_preview")
      ),
      fluidRow(
        box(
          width = 12, status = "primary", solidHeader = FALSE,
          div(class = "b-only b-next-wrap",
              actionButton("next_from_clean", "Next: Feature Engineering", icon = icon("arrow-right"))
          )
        )
      )
    )
  )
),

      # -------------------------
      # Feature engineering
      # -------------------------
      tabItem(
        tabName = "feature",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Feature engineering (how to use)",
            div(
              class = "help-note",
              tags$p(tags$strong("Workflow: "), "Add features below, then use ", tags$strong("Visual impact"), " + profile table to see distributions. ", tags$strong("Preview only"), " tests a formula without saving."),
              tags$ul(
                tags$li(tags$strong("Transforms:"), " option labels describe the math (Z-score, min-max, lag/lead, etc.)."),
                tags$li(tags$strong("Binning:"), " equal-width splits the range; quantile targets similar counts per bin."),
                tags$li(tags$strong("Rolling mean:"), " uses current row order; first w-1 rows are NA."),
                tags$li(tags$strong("Reset:"), " reloads from the latest ", tags$strong("cleaned"), " dataset (undoes engineered columns here).")
              ),
              div(style = "margin-top: 12px;",
                  actionButton("fe_reset", "Reset to cleaned data", icon = icon("undo"), class = "btn-warning"))
            )
          )
        ),
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Formula-based feature",
            textInput("new_col_name", "New column name:", "new_feature"),
            textAreaInput("formula_text", "Formula (R expression):",
                          value = "sepal_length / sepal_width", rows = 2,
                          placeholder = "e.g. col_a / col_b or log1p(income)"),
            tags$p(class = "text-muted", style = "font-size:12px;line-height:1.4;",
                   "Use column names; helpers include log, log1p, sqrt, abs, exp, ifelse, pmin, pmax, round."),
            actionButton("fe_preview_formula", "Preview only (dry run)", icon = icon("search")),
            tags$hr(),
            verbatimTextOutput("fe_formula_preview"),
            actionButton("add_formula_feature", "Add formula-based feature", icon = icon("plus"))
          ),
          box(
            width = 4, status = "warning", solidHeader = TRUE,
            title = "Single-column transformation",
            uiOutput("feature_numeric_col_ui"),
            selectInput(
              "single_transform", "Transformation:",
              choices = c(
                "Square (x^2)" = "square",
                "Sqrt (sqrt abs x)" = "sqrt",
                "Log1p" = "log1p",
                "Abs" = "abs",
                "Center (x - mean)" = "center",
                "Z-score" = "zscore",
                "Min-max [0,1]" = "minmax",
                "Rank" = "rank",
                "Reciprocal 1/x" = "inv",
                "Lag (prev row)" = "lag1",
                "Lead (next row)" = "lead1"
              )
            ),
            textInput("single_transform_name", "Output column name:", "transformed_feature"),
            actionButton("add_single_transform", "Add transformed feature", icon = icon("magic"))
          ),
          box(
            width = 4, status = "success", solidHeader = TRUE,
            title = "Two-column (numeric)",
            uiOutput("feature_num_col_a_ui"),
            uiOutput("feature_num_col_b_ui"),
            selectInput(
              "pair_op", "Operation:",
              choices = c("Multiply" = "multiply",
                          "Add" = "add",
                          "Subtract A - B" = "subtract",
                          "Ratio A / B" = "ratio")
            ),
            textInput("pair_feature_name", "Output column name:", "pair_feature"),
            actionButton("add_pair_feature", "Add pair feature", icon = icon("link"))
          )
        ),
        fluidRow(
          box(
            width = 4, status = "info", solidHeader = TRUE,
            title = "Binning",
            uiOutput("bin_col_ui"),
            selectInput(
              "bin_method", "Bin edges:",
              choices = c("Equal width" = "equal",
                          "Quantiles" = "quantile")
            ),
            numericInput("n_bins", "Number of bins:", 4, min = 2, max = 20, step = 1),
            textInput("bin_feature_name", "Binned feature name:", "binned_feature"),
            actionButton("add_binning", "Add binned feature", icon = icon("layer-group"))
          ),
          box(
            width = 4, status = "warning", solidHeader = TRUE,
            title = "Rolling mean",
            uiOutput("rolling_col_ui"),
            numericInput("rolling_window", "Window (rows):", 3, min = 2, max = 50, step = 1),
            textInput("rolling_name", "New column name:", "rolling_mean_3"),
            actionButton("add_rolling_mean", "Add rolling mean", icon = icon("chart-line")),
            tags$p(class = "text-muted", style = "font-size:11px;", "Row order = time index; large n may be slow.")
          ),
          box(
            width = 4, status = "success", solidHeader = TRUE,
            title = "Text-derived",
            uiOutput("text_feature_col_ui"),
            selectInput(
              "text_feature_op", "Derive:",
              choices = c("String length" = "nchar",
                          "Word count" = "words")
            ),
            textInput("text_feature_name", "New column name:", "text_feature"),
            actionButton("add_text_feature", "Add text feature", icon = icon("font"))
          )
        ),
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Visual impact",
            uiOutput("fe_inspect_col_ui"),
            uiOutput("fe_compare_col_ui"),
            plotlyOutput("fe_impact_plot", height = "320px"),
            tags$p(class = "text-muted", style = "font-size:12px;",
                   "Numeric: histogram(s). Optional second column: side-by-side. Non-numeric: bar of top levels.")
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Profile (selected column)",
            DTOutput("fe_profile_table")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Engineered data preview and log",
            verbatimTextOutput("feature_log"),
            DTOutput("feature_preview")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            div(class = "b-only b-next-wrap",
                actionButton("next_from_feature", "Next: EDA", icon = icon("arrow-right"))
            )
          )
        )
      ),

      # -------------------------
      # EDA
      # -------------------------
      # ------ 456 change ------
      tabItem(
        tabName = "eda",
        fluidRow(
          valueBoxOutput("eda_rows", width = 3),
          valueBoxOutput("eda_cols", width = 3),
          valueBoxOutput("eda_num", width = 3),
          valueBoxOutput("eda_cat", width = 3)
        ),
        fluidRow(
          box(
            width = 3, status = "primary", solidHeader = TRUE,
            title = "Interactive Filters",
            uiOutput("filter_var_ui"),
            uiOutput("filter_control_ui"),
            checkboxInput("keep_na_filter", "Keep missing values when filtering", TRUE),
            actionButton("clear_filter", "Clear Filter", icon = icon("eraser")),
            tags$hr(),
            uiOutput("summary_var_ui")
          ),
          box(
            width = 9, status = "info", solidHeader = TRUE,
            title = "Filtered Data Preview",
            div(class = "help-note",
          "Tip: use the filter panel on the left, then inspect how the summary tables and plots update  automatically."),
            DTOutput("filtered_preview")
          )
        ),
        fluidRow(
          box(
            width = 6, status = "warning", solidHeader = TRUE,
            title = "Summary Statistics",
            DTOutput("summary_stats")
          ),
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Selected Variable Profile",
            DTOutput("variable_profile")
          )
        ),
        
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Distribution Explorer",
            uiOutput("dist_var_ui"),
            selectInput(
              "dist_plot_type", "Plot type:",
              choices = c("Histogram" = "hist",
                          "Boxplot" = "box",
                          "Density" = "density",
                          "Bar chart (categorical)" = "bar")
            ),
            conditionalPanel(
              condition = "input.dist_plot_type == 'hist'",
              sliderInput("dist_bins", "Number of bins:", min = 5, max = 60, value = 25)
            ),
            conditionalPanel(
              condition = "input.dist_plot_type == 'bar'",
              sliderInput("top_n_cat", "Show top N categories:", min = 3, max = 15, value = 8)
            ),
            checkboxInput("show_mean_line", "Add mean reference line for numeric plots", FALSE),
            plotlyOutput("dist_plot", height = "420px")
          ),
  
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Relationship Explorer",
            uiOutput("x_var_ui"),
            uiOutput("y_var_ui"),
            uiOutput("color_var_ui"),
            selectInput(
              "relation_type", "Relationship plot style:",
              choices = c("Auto detect" = "auto",
                          "Scatter" = "scatter",
                          "Boxplot" = "box",
                          "Bar chart" = "bar")
            ),
            checkboxInput("add_smooth", "Add trend line when possible", FALSE),
            plotlyOutput("relation_plot", height = "420px")
          )
        ),
  
        fluidRow(
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Missingness by Column",
            plotlyOutput("missing_plot", height = "360px")
          ),
          box(
            width = 6, status = "danger", solidHeader = TRUE,
            title = "Top Correlations",
            DTOutput("top_corr_table")
          )
        ),
  
        fluidRow(
          box(
            width = 12, status = "danger", solidHeader = TRUE,
            title = "Correlation Heatmap (Numeric Variables)",
            plotlyOutput("corr_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            div(class = "b-only b-next-wrap",
                actionButton("next_from_eda", "Next: Export", icon = icon("arrow-right"))
            )
          )
        )
      ),
  # ------ 456 ------

      # -------------------------
      # Export
      # -------------------------
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Download Processed Dataset and Experiment Logs",
            tags$p("Download the final cleaned and engineered dataset together with the experiment log generated for this session."),
            tags$p(tags$strong("Current A/B group: "), tags$span(class = "ab-group-text", "A")),
            div(class = "help-note",
                tags$strong("Data collection note: "),
                "This app records randomized group assignment, step transitions, dwell time, data source selection, file upload events, cleaning usage, feature engineering actions, and final task completion. These logs can be used directly in the project report as original A/B testing data documentation."
            ),
            fluidRow(
              column(4, downloadButton("download_data", "Download Processed CSV")),
              column(4, downloadButton("download_session_log", "Download Session Log")),
              column(4, downloadButton("download_quality_summary", "Download Quality Summary"))
            ),
            tags$hr(),
            h4("Session-level quality summary"),
            tableOutput("experiment_quality_summary"),
            tags$hr(),
            tags$div(class = "help-note", "Google Analytics is used as an external tracking layer, while the downloadable session log below provides a reproducible record for the report and code appendix.")
          )
        )
      )
    )
  )
)

}

# =========================
# Server
# =========================

server <- function(input, output, session) {

  track_ga <- function(event_name, ...) {
    if (!nzchar(GA_MEASUREMENT_ID)) return(invisible(NULL))
    payload <- list(event_name = event_name, ...)
    session$sendCustomMessage("ga_event", payload)
    invisible(NULL)
  }

  tracked_steps <- c("guide", "load", "clean", "feature", "eda", "export")

  make_session_id <- function() {
    paste0(
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      paste(sample(c(letters, LETTERS, 0:9), 10, replace = TRUE), collapse = "")
    )
  }

  current_group <- reactive({
    g <- toupper(as.character(input$client_group %||% "A"))
    if (!g %in% c("A", "B")) g <- "A"
    g
  })

  rv <- reactiveValues(
    feature_log = character(0),
    session_id = make_session_id(),
    session_start = Sys.time(),
    current_step = NULL,
    step_enter_time = NULL,
    data_source = "builtin",
    file_uploaded = FALSE,
    completed_download = FALSE,
    event_log = data.frame(
      timestamp = character(0),
      session_id = character(0),
      app_group = character(0),
      event_name = character(0),
      step_name = character(0),
      value = numeric(0),
      detail = character(0),
      stringsAsFactors = FALSE
    )
  )

  record_event <- function(event_name, step_name = NA_character_, value = NA_real_, detail = NA_character_, send_to_ga = TRUE, ...) {
    event_row <- data.frame(
      timestamp = as.character(Sys.time()),
      session_id = rv$session_id,
      app_group = current_group(),
      event_name = event_name,
      step_name = step_name,
      value = if (length(value) == 0) NA_real_ else suppressWarnings(as.numeric(value)[1]),
      detail = if (length(detail) == 0) NA_character_ else as.character(detail)[1],
      stringsAsFactors = FALSE
    )
    rv$event_log <- bind_rows(rv$event_log, event_row)

    if (isTRUE(send_to_ga)) {
      track_ga(
        event_name,
        session_id = rv$session_id,
        app_group = current_group(),
        step_name = step_name,
        value = event_row$value,
        detail = detail,
        ...
      )
    }
    invisible(NULL)
  }

  send_step_exit <- function(step_name) {
    if (is.null(step_name) || !(step_name %in% tracked_steps) || is.null(rv$step_enter_time)) return(invisible(NULL))
    dwell_seconds <- as.numeric(difftime(Sys.time(), rv$step_enter_time, units = "secs"))
    if (!is.finite(dwell_seconds) || dwell_seconds < 0) return(invisible(NULL))

    record_event(
      "step_exit",
      step_name = step_name,
      value = round(dwell_seconds, 2),
      detail = "dwell_seconds",
      dwell_seconds = round(dwell_seconds, 2)
    )
    invisible(NULL)
  }

  observeEvent(input$client_group, {
    req(input$client_group)
    record_event(
      "ab_group_view",
      detail = paste0("group=", current_group())
    )
  }, once = TRUE, ignoreInit = FALSE)

  observeEvent(input$tabs, {
    req(input$tabs)
    if (!(input$tabs %in% tracked_steps)) return(invisible(NULL))

    if (!is.null(rv$current_step) && !identical(rv$current_step, input$tabs)) {
      send_step_exit(rv$current_step)
    }

    rv$current_step <- input$tabs
    rv$step_enter_time <- Sys.time()

    record_event(
      "step_enter",
      step_name = input$tabs,
      detail = paste0("entered_", input$tabs)
    )

    if (identical(input$tabs, "export")) {
      record_event(
        "task_completed",
        step_name = "export",
        detail = "completion_type=export",
        completion_type = "export"
      )
    }
  }, ignoreInit = FALSE)

  observeEvent(input$data_source, {
    req(input$data_source)
    rv$data_source <- input$data_source
    record_event(
      "data_source_selected",
      step_name = "load",
      detail = input$data_source,
      data_source = input$data_source
    )
  }, ignoreInit = FALSE)

  observeEvent(input$builtin_data, {
    req(input$builtin_data)
    record_event(
      "builtin_dataset_selected",
      step_name = "load",
      detail = input$builtin_data,
      dataset_name = input$builtin_data
    )
  }, ignoreInit = TRUE)

  observeEvent(input$file, {
    req(input$file)
    rv$file_uploaded <- TRUE
    record_event(
      "file_upload",
      step_name = "load",
      detail = input$file$name,
      file_name = input$file$name
    )
  }, ignoreInit = TRUE)

  observeEvent(input$download_data, {
    record_event(
      "task_completed",
      step_name = "export",
      detail = "completion_type=download",
      completion_type = "download"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$next_from_guide, {
    updateTabItems(session, "tabs", "load")
  })
  observeEvent(input$next_from_load, {
    updateTabItems(session, "tabs", "clean")
  })
  observeEvent(input$next_from_clean, {
    updateTabItems(session, "tabs", "feature")
  })
  observeEvent(input$next_from_feature, {
    updateTabItems(session, "tabs", "eda")
  })
  observeEvent(input$next_from_eda, {
    updateTabItems(session, "tabs", "export")
  })

  session$onSessionEnded(function() {
    session_id_local <- isolate(rv$session_id)
    session_start_local <- isolate(rv$session_start)
    last_step_local <- isolate(rv$current_step)
    step_enter_time_local <- isolate(rv$step_enter_time)
    data_source_local <- isolate(rv$data_source)
    file_uploaded_local <- isolate(rv$file_uploaded)
    app_group_local <- isolate(current_group())
    event_log_local <- isolate(rv$event_log)
    session_end_local <- Sys.time()

    if (!is.null(last_step_local) && last_step_local %in% tracked_steps && !is.null(step_enter_time_local)) {
      dwell_seconds <- as.numeric(difftime(session_end_local, step_enter_time_local, units = "secs"))
      if (is.finite(dwell_seconds) && dwell_seconds >= 0) {
        exit_row <- data.frame(
          timestamp = as.character(session_end_local),
          session_id = session_id_local,
          app_group = app_group_local,
          event_name = "step_exit",
          step_name = last_step_local,
          value = round(dwell_seconds, 2),
          detail = "dwell_seconds",
          stringsAsFactors = FALSE
        )
        event_log_local <- bind_rows(event_log_local, exit_row)
        track_ga(
          "step_exit",
          session_id = session_id_local,
          app_group = app_group_local,
          step_name = last_step_local,
          value = round(dwell_seconds, 2),
          detail = "dwell_seconds",
          dwell_seconds = round(dwell_seconds, 2)
        )
      }
    }

    total_session_seconds_local <- round(as.numeric(difftime(session_end_local, session_start_local, units = "secs")), 2)
    end_row <- data.frame(
      timestamp = as.character(session_end_local),
      session_id = session_id_local,
      app_group = app_group_local,
      event_name = "session_end",
      step_name = if (is.null(last_step_local)) NA_character_ else last_step_local,
      value = total_session_seconds_local,
      detail = paste0("data_source=", data_source_local, ";file_uploaded=", file_uploaded_local),
      stringsAsFactors = FALSE
    )
    event_log_local <- bind_rows(event_log_local, end_row)

    track_ga(
      "session_end",
      session_id = session_id_local,
      app_group = app_group_local,
      last_step = if (is.null(last_step_local)) NA_character_ else last_step_local,
      total_session_seconds = total_session_seconds_local,
      data_source = data_source_local,
      file_uploaded = file_uploaded_local
    )

    session_summary_local <- build_session_summary(
      event_log = event_log_local,
      session_id = session_id_local,
      app_group = app_group_local,
      session_start = session_start_local,
      session_end = session_end_local,
      data_source = data_source_local,
      file_uploaded = file_uploaded_local
    )

    raw_df_local <- tryCatch(isolate(raw_data()), error = function(e) NULL)
    clean_df_local <- tryCatch(isolate(cleaned_data()), error = function(e) NULL)
    quality_summary_local <- build_quality_summary(
      raw_df = raw_df_local,
      cleaned_df = clean_df_local,
      event_log = event_log_local,
      summary_df = session_summary_local
    )

    out_dir <- getwd()
    try(write.csv(event_log_local, file.path(out_dir, paste0("session_log_", session_id_local, ".csv")), row.names = FALSE), silent = TRUE)
    try(write.csv(quality_summary_local, file.path(out_dir, paste0("quality_summary_", session_id_local, ".csv")), row.names = FALSE), silent = TRUE)
  })

  raw_data <- reactive({
    if (input$data_source == "builtin") {
      df <- switch(
        input$builtin_data,
        "iris" = iris,
        "mtcars" = mtcars,
        "airquality" = airquality
      )
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      names(df) <- clean_names_custom(names(df))
      return(df)
    }

    req(input$file)
    ext <- tools::file_ext(input$file$name)

    tryCatch(
      read_uploaded_data(input$file$datapath, ext),
      error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
        NULL
      }
    )
  })

  observeEvent(raw_data(), {
    df <- raw_data()
    req(df)
    record_event(
      "raw_data_loaded",
      step_name = "load",
      value = nrow(df),
      detail = paste0("rows=", nrow(df), ";cols=", ncol(df))
    )
  }, ignoreInit = FALSE)

  observeEvent(cleaned_data(), {
    df <- cleaned_data()
    req(df)
    record_event(
      "cleaning_applied",
      step_name = "clean",
      value = nrow(df),
      detail = paste0("rows=", nrow(df), ";cols=", ncol(df), ";missing=", sum(is.na(df)))
    )
  }, ignoreInit = FALSE)

  session_summary_data <- reactive({
    build_session_summary(
      event_log = rv$event_log,
      session_id = rv$session_id,
      app_group = current_group(),
      session_start = rv$session_start,
      session_end = Sys.time(),
      data_source = rv$data_source,
      file_uploaded = rv$file_uploaded
    )
  })

  quality_summary_data <- reactive({
    raw_df <- tryCatch(raw_data(), error = function(e) NULL)
    clean_df <- tryCatch(cleaned_data(), error = function(e) NULL)
    build_quality_summary(
      raw_df = raw_df,
      cleaned_df = clean_df,
      event_log = rv$event_log,
      summary_df = session_summary_data()
    )
  })

  output$experiment_quality_summary <- renderTable({
    quality_summary_data()
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  observe({
    df <- raw_data()
    req(df)

    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[!sapply(df, is.numeric)]

    updatePickerInput(session, "scale_cols", choices = num_cols, selected = NULL)
    updatePickerInput(session, "encode_cols", choices = cat_cols, selected = NULL)
  })

  output$numeric_cols_ui_clean <- renderUI({
    df <- raw_data()
    req(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    pickerInput(
      "outlier_cols", "Numeric columns for outlier handling:",
      choices = num_cols,
      selected = num_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })

  output$vb_rows <- renderValueBox({
    df <- raw_data()
    req(df)
    valueBox(nrow(df), "Rows", icon = icon("table"), color = "blue")
  })

  output$vb_cols <- renderValueBox({
    df <- raw_data()
    req(df)
    valueBox(ncol(df), "Columns", icon = icon("columns"), color = "purple")
  })

  output$vb_missing <- renderValueBox({
    df <- raw_data()
    req(df)
    valueBox(sum(is.na(df)), "Missing Values", icon = icon("circle-exclamation"), color = "yellow")
  })

  output$vb_dups <- renderValueBox({
    df <- raw_data()
    req(df)
    valueBox(sum(duplicated(df)), "Duplicate Rows", icon = icon("clone"), color = "red")
  })

  output$raw_preview <- renderDT({
    df <- raw_data()
    req(df)
    datatable(df, options = list(scrollX = TRUE, pageLength = 8))
  })

  output$raw_structure <- renderPrint({
    df <- raw_data()
    req(df)
    str(df)
  })

  output$quick_summary <- renderTable({
    df <- raw_data()
    req(df)
    data.frame(
      Metric = c("Rows", "Columns", "Numeric columns", "Categorical columns", "Missing values", "Duplicate rows"),
      Value = c(
        nrow(df),
        ncol(df),
        sum(sapply(df, is.numeric)),
        sum(!sapply(df, is.numeric)),
        sum(is.na(df)),
        sum(duplicated(df))
      )
    )
  })

  cleaned_data <- reactive({
  df <- raw_data()
  req(df)

  log_msgs <- c()
  before_rows <- nrow(df)
  before_cols <- ncol(df)
  before_missing <- sum(is.na(df))
  before_dups <- sum(duplicated(df))

  # helper for formatting
  fmt_num <- function(x) {
    if (is.numeric(x)) {
      format(round(x, 4), trim = TRUE, scientific = FALSE)
    } else {
      as.character(x)
    }
  }

  # =========================================================
  # Step 1: Basic Cleaning
  # =========================================================
  step1_msgs <- c("Step 1: Basic Cleaning")

  if (isTRUE(input$standardize_names)) {
    old_names <- names(df)
    new_names <- clean_names_custom(old_names)
    changed_idx <- which(old_names != new_names)
    names(df) <- new_names

    if (length(changed_idx) > 0) {
      step1_msgs <- c(
        step1_msgs,
        paste0("- Standardized column names: ", length(changed_idx), " column name(s) changed.")
      )

      preview_n <- min(5, length(changed_idx))
      preview_pairs <- paste0(
        old_names[changed_idx][1:preview_n], " -> ", new_names[changed_idx][1:preview_n]
      )

      step1_msgs <- c(
        step1_msgs,
        paste0("  Example changes: ", paste(preview_pairs, collapse = "; "),
               if (length(changed_idx) > preview_n) "; ..." else "")
      )
    } else {
      step1_msgs <- c(step1_msgs, "- Standardized column names: no changes needed.")
    }
  } else {
    step1_msgs <- c(step1_msgs, "- Standardized column names: skipped.")
  }

  if (isTRUE(input$remove_duplicates)) {
    dup_n_before <- sum(duplicated(df))
    rows_before <- nrow(df)

    df <- distinct(df)

    rows_after <- nrow(df)
    removed <- rows_before - rows_after

    step1_msgs <- c(
      step1_msgs,
      paste0("- Duplicate removal: ", removed, " duplicate row(s) removed."),
      paste0("  Rows: ", rows_before, " -> ", rows_after,
             " | Duplicate rows before cleaning: ", dup_n_before)
    )
  } else {
    step1_msgs <- c(step1_msgs, "- Duplicate removal: skipped.")
  }

  log_msgs <- c(log_msgs, step1_msgs, "")

  # refresh column groups
  num_cols <- names(df)[sapply(df, is.numeric)]
  cat_cols <- names(df)[!sapply(df, is.numeric)]

  # =========================================================
  # Step 2: Missing Values
  # =========================================================
  step2_msgs <- c("Step 2: Missing Values")

  if (input$missing_num_method == "drop_rows" || input$missing_cat_method == "drop_rows") {
    rows_before <- nrow(df)
    missing_before <- sum(is.na(df))

    col_missing_before <- sapply(df, function(x) sum(is.na(x)))
    affected_cols <- names(col_missing_before[col_missing_before > 0])

    df <- df[complete.cases(df), , drop = FALSE]

    rows_after <- nrow(df)
    missing_after <- sum(is.na(df))

    step2_msgs <- c(
      step2_msgs,
      "- Row deletion for missing data: applied across the full dataset.",
      paste0("- Rows removed: ", rows_before - rows_after, "."),
      paste0("- Missing values: ", missing_before, " -> ", missing_after, ".")
    )

    if (length(affected_cols) > 0) {
      step2_msgs <- c(
        step2_msgs,
        paste0("- Columns with missing values before deletion: ",
               paste(affected_cols, collapse = ", "), ".")
      )
    }
  } else {
    # ---------- numeric missing ----------
    if (input$missing_num_method == "none") {
      step2_msgs <- c(step2_msgs, "- Numeric missing values: no action taken.")
    } else {
      numeric_na_cols <- num_cols[sapply(df[num_cols], function(x) anyNA(x))]

      if (length(numeric_na_cols) == 0) {
        step2_msgs <- c(step2_msgs, "- Numeric missing values: no missing values detected.")
      } else {
        method_label <- ifelse(input$missing_num_method == "mean", "Mean imputation", "Median imputation")
        step2_msgs <- c(step2_msgs, paste0("- Numeric missing values: ", method_label, " applied."))

        for (col in numeric_na_cols) {
          na_count <- sum(is.na(df[[col]]))

          if (input$missing_num_method == "mean") {
            fill_value <- mean(df[[col]], na.rm = TRUE)
            df[[col]][is.na(df[[col]])] <- fill_value
            step2_msgs <- c(
              step2_msgs,
              paste0("  * ", col, ": ", na_count,
                     " missing value(s) filled with mean = ", fmt_num(fill_value), ".")
            )
          } else if (input$missing_num_method == "median") {
            fill_value <- median(df[[col]], na.rm = TRUE)
            df[[col]][is.na(df[[col]])] <- fill_value
            step2_msgs <- c(
              step2_msgs,
              paste0("  * ", col, ": ", na_count,
                     " missing value(s) filled with median = ", fmt_num(fill_value), ".")
            )
          }
        }
      }
    }

    # ---------- categorical missing ----------
    if (input$missing_cat_method == "none") {
      step2_msgs <- c(step2_msgs, "- Categorical missing values: no action taken.")
    } else {
      cat_na_cols <- cat_cols[sapply(df[cat_cols], function(x) anyNA(x))]

      if (length(cat_na_cols) == 0) {
        step2_msgs <- c(step2_msgs, "- Categorical missing values: no missing values detected.")
      } else if (input$missing_cat_method == "mode") {
        step2_msgs <- c(step2_msgs, "- Categorical missing values: mode imputation applied.")

        for (col in cat_na_cols) {
          na_count <- sum(is.na(df[[col]]))
          fill_value <- mode_value(df[[col]])
          df[[col]][is.na(df[[col]])] <- fill_value

          step2_msgs <- c(
            step2_msgs,
            paste0("  * ", col, ": ", na_count,
                   " missing value(s) filled with mode = ", as.character(fill_value), ".")
          )
        }
      } else if (input$missing_cat_method == "missing_level") {
        step2_msgs <- c(step2_msgs, "- Categorical missing values: replaced with 'Missing' level.")

        for (col in cat_na_cols) {
          na_count <- sum(is.na(df[[col]]))
          df[[col]] <- as.character(df[[col]])
          df[[col]][is.na(df[[col]])] <- "Missing"

          step2_msgs <- c(
            step2_msgs,
            paste0("  * ", col, ": ", na_count,
                   " missing value(s) replaced with 'Missing'.")
          )
        }
      }
    }
  }

  log_msgs <- c(log_msgs, step2_msgs, "")

  # =========================================================
  # Step 3: Outlier Handling
  # =========================================================
  step3_msgs <- c("Step 3: Outlier Handling")

  selected_outlier_cols <- input$outlier_cols

  if (is.null(selected_outlier_cols) ||
      length(selected_outlier_cols) == 0 ||
      input$outlier_method == "none") {
    step3_msgs <- c(step3_msgs, "- Outlier handling: no action taken.")
  } else {
    valid_outlier_cols <- selected_outlier_cols[selected_outlier_cols %in% names(df)]

    if (length(valid_outlier_cols) == 0) {
      step3_msgs <- c(step3_msgs, "- Outlier handling: no valid numeric columns selected.")
    } else if (input$outlier_method == "cap") {
      step3_msgs <- c(step3_msgs, "- Method: IQR capping.")

      for (col in valid_outlier_cols) {
        if (is.numeric(df[[col]])) {
          x <- df[[col]]
          q1 <- quantile(x, 0.25, na.rm = TRUE)
          q3 <- quantile(x, 0.75, na.rm = TRUE)
          iqr_val <- q3 - q1
          lower <- q1 - 1.5 * iqr_val
          upper <- q3 + 1.5 * iqr_val
          outlier_n <- sum(x < lower | x > upper, na.rm = TRUE)

          df[[col]] <- cap_outliers_iqr(x)

          step3_msgs <- c(
            step3_msgs,
            paste0("  * ", col, ": ", outlier_n,
                   " outlier value(s) capped within [",
                   fmt_num(lower), ", ", fmt_num(upper), "].")
          )
        }
      }
    } else if (input$outlier_method == "remove") {
      step3_msgs <- c(step3_msgs, "- Method: IQR row removal.")

      rows_before <- nrow(df)

      for (col in valid_outlier_cols) {
        if (is.numeric(df[[col]])) {
          x <- df[[col]]
          q1 <- quantile(x, 0.25, na.rm = TRUE)
          q3 <- quantile(x, 0.75, na.rm = TRUE)
          iqr_val <- q3 - q1
          lower <- q1 - 1.5 * iqr_val
          upper <- q3 + 1.5 * iqr_val
          outlier_n <- sum(x < lower | x > upper, na.rm = TRUE)

          step3_msgs <- c(
            step3_msgs,
            paste0("  * ", col, ": ", outlier_n,
                   " row(s) flagged outside [",
                   fmt_num(lower), ", ", fmt_num(upper), "].")
          )
        }
      }

      df <- remove_outlier_rows_iqr(df, valid_outlier_cols)
      rows_after <- nrow(df)

      step3_msgs <- c(
        step3_msgs,
        paste0("- Total rows removed after combined outlier filtering: ",
               rows_before - rows_after, "."),
        paste0("- Columns checked: ", paste(valid_outlier_cols, collapse = ", "), ".")
      )
    }
  }

  log_msgs <- c(log_msgs, step3_msgs, "")

  # =========================================================
  # Step 4: Scaling
  # =========================================================
  step4_msgs <- c("Step 4: Scaling")

  if (is.null(input$scale_cols) ||
      length(input$scale_cols) == 0 ||
      input$scale_method == "none") {
    step4_msgs <- c(step4_msgs, "- Scaling: no action taken.")
  } else {
    valid_scale_cols <- input$scale_cols[input$scale_cols %in% names(df)]

    if (length(valid_scale_cols) == 0) {
      step4_msgs <- c(step4_msgs, "- Scaling: no valid numeric columns selected.")
    } else {
      method_label <- ifelse(input$scale_method == "zscore",
                             "Z-score standardization",
                             "Min-Max scaling")

      step4_msgs <- c(
        step4_msgs,
        paste0("- Method: ", method_label, "."),
        paste0("- Columns scaled: ", paste(valid_scale_cols, collapse = ", "), ".")
      )

      for (col in valid_scale_cols) {
        if (is.numeric(df[[col]])) {
          old_min <- suppressWarnings(min(df[[col]], na.rm = TRUE))
          old_max <- suppressWarnings(max(df[[col]], na.rm = TRUE))
          old_mean <- suppressWarnings(mean(df[[col]], na.rm = TRUE))

          if (input$scale_method == "zscore") {
            df[[col]] <- as.numeric(scale_zscore(df[[col]]))
            new_mean <- suppressWarnings(mean(df[[col]], na.rm = TRUE))
            new_sd <- suppressWarnings(sd(df[[col]], na.rm = TRUE))

            step4_msgs <- c(
              step4_msgs,
              paste0("  * ", col,
                     ": before mean = ", fmt_num(old_mean),
                     "; after mean = ", fmt_num(new_mean),
                     ", sd = ", fmt_num(new_sd), ".")
            )
          } else if (input$scale_method == "minmax") {
            df[[col]] <- as.numeric(scale_minmax(df[[col]]))
            new_min <- suppressWarnings(min(df[[col]], na.rm = TRUE))
            new_max <- suppressWarnings(max(df[[col]], na.rm = TRUE))

            step4_msgs <- c(
              step4_msgs,
              paste0("  * ", col,
                     ": before range = [", fmt_num(old_min), ", ", fmt_num(old_max),
                     "]; after range = [", fmt_num(new_min), ", ", fmt_num(new_max), "].")
            )
          }
        }
      }
    }
  }

  log_msgs <- c(log_msgs, step4_msgs, "")

  # =========================================================
  # Step 5: Encoding
  # =========================================================
  step5_msgs <- c("Step 5: Encoding")

  if (is.null(input$encode_cols) ||
      length(input$encode_cols) == 0 ||
      input$encoding_method == "none") {
    step5_msgs <- c(step5_msgs, "- Encoding: no action taken.")
  } else {
    valid_encode_cols <- input$encode_cols[input$encode_cols %in% names(df)]

    if (length(valid_encode_cols) == 0) {
      step5_msgs <- c(step5_msgs, "- Encoding: no valid categorical columns selected.")
    } else if (input$encoding_method == "label") {
      step5_msgs <- c(step5_msgs, "- Method: Integer / label encoding.")

      for (col in valid_encode_cols) {
        original_levels <- sort(unique(as.character(df[[col]])))
        original_levels <- original_levels[!is.na(original_levels)]

        df[[col]] <- as.integer(as.factor(df[[col]]))

        step5_msgs <- c(
          step5_msgs,
          paste0("  * ", col, ": ", length(original_levels),
                 " distinct level(s) converted to integer labels.")
        )
      }

      step5_msgs <- c(
        step5_msgs,
        paste0("- Columns encoded: ", paste(valid_encode_cols, collapse = ", "), ".")
      )
    } else if (input$encoding_method == "onehot") {
      step5_msgs <- c(step5_msgs, "- Method: One-hot encoding.")

      old_col_count <- ncol(df)

      dummy_parts <- lapply(valid_encode_cols, function(col) {
        original_levels <- sort(unique(as.character(df[[col]])))
        original_levels <- original_levels[!is.na(original_levels)]

        mm <- model.matrix(~ . - 1, data = data.frame(tmp = as.factor(df[[col]])))
        mm <- as.data.frame(mm)
        names(mm) <- paste0(col, "_", clean_names_custom(gsub("^tmp", "", names(mm))))

        step5_msgs <<- c(
          step5_msgs,
          paste0("  * ", col, ": ", length(original_levels),
                 " level(s) generated ", ncol(mm), " indicator column(s).")
        )

        mm
      })

      df <- bind_cols(df %>% select(-all_of(valid_encode_cols)), bind_cols(dummy_parts))

      new_col_count <- ncol(df)

      step5_msgs <- c(
        step5_msgs,
        paste0("- Original columns removed after one-hot encoding: ",
               paste(valid_encode_cols, collapse = ", "), "."),
        paste0("- Total columns: ", old_col_count, " -> ", new_col_count, ".")
      )
    }
  }

  log_msgs <- c(log_msgs, step5_msgs, "")

  # =========================================================
  # Final Result
  # =========================================================
  final_rows <- nrow(df)
  final_cols <- ncol(df)
  final_missing <- sum(is.na(df))
  final_dups <- sum(duplicated(df))

  final_msgs <- c(
    "Final Result",
    paste0("- Rows: ", before_rows, " -> ", final_rows,
           " (change: ", final_rows - before_rows, ")."),
    paste0("- Columns: ", before_cols, " -> ", final_cols,
           " (change: ", final_cols - before_cols, ")."),
    paste0("- Missing values: ", before_missing, " -> ", final_missing,
           " (change: ", final_missing - before_missing, ")."),
    paste0("- Duplicate rows: ", before_dups, " -> ", final_dups,
           " (change: ", final_dups - before_dups, ").")
  )

  log_msgs <- c(log_msgs, final_msgs)

  attr(df, "log_msgs") <- log_msgs
  df
})

output$preprocess_summary <- renderTable({
  before_df <- raw_data()
  after_df  <- cleaned_data()
  req(before_df, after_df)

  data.frame(
    Metric = c(
      "Rows",
      "Columns",
      "Numeric Columns",
      "Categorical Columns",
      "Missing Values",
      "Duplicate Rows"
    ),
    Before = c(
      nrow(before_df),
      ncol(before_df),
      sum(sapply(before_df, is.numeric)),
      sum(!sapply(before_df, is.numeric)),
      sum(is.na(before_df)),
      sum(duplicated(before_df))
    ),
    After = c(
      nrow(after_df),
      ncol(after_df),
      sum(sapply(after_df, is.numeric)),
      sum(!sapply(after_df, is.numeric)),
      sum(is.na(after_df)),
      sum(duplicated(after_df))
    ),
    Change = c(
      nrow(after_df) - nrow(before_df),
      ncol(after_df) - ncol(before_df),
      sum(sapply(after_df, is.numeric)) - sum(sapply(before_df, is.numeric)),
      sum(!sapply(after_df, is.numeric)) - sum(!sapply(before_df, is.numeric)),
      sum(is.na(after_df)) - sum(is.na(before_df)),
      sum(duplicated(after_df)) - sum(duplicated(before_df))
    ),
    check.names = FALSE
  )
}, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

output$cleaning_log <- renderPrint({
  df <- cleaned_data()
  req(df)

  msgs <- attr(df, "log_msgs")

  if (is.null(msgs) || length(msgs) == 0) {
    cat("No preprocessing steps applied.\n")
  } else {
    cat("Data Cleaning Log\n")
    cat(paste(rep("=", 65), collapse = ""), "\n\n", sep = "")
    cat(paste(msgs, collapse = "\n"))
    cat("\n")
  }
})

output$clean_preview <- renderDT({
  df <- cleaned_data()
  req(df)
  datatable(
    df,
    options = list(
      scrollX = TRUE,
      pageLength = 8
    )
  )
})

  observe({
    df <- feature_data()
    req(df)
    num_cols <- names(df)[sapply(df, is.numeric)]

    output$feature_numeric_col_ui <- renderUI({
      selectInput("feature_numeric_col", "Numeric column:", choices = num_cols)
    })

    output$feature_num_col_a_ui <- renderUI({
      selectInput("feature_num_col_a", "Column A:", choices = num_cols)
    })

    output$feature_num_col_b_ui <- renderUI({
      selectInput("feature_num_col_b", "Column B:", choices = num_cols)
    })

    output$bin_col_ui <- renderUI({
      selectInput("bin_col", "Numeric column to bin:", choices = num_cols)
    })

    output$rolling_col_ui <- renderUI({
      selectInput("rolling_col", "Numeric column:", choices = num_cols)
    })

    char_cols <- names(df)[vapply(df, function(z) is.character(z) || is.factor(z), logical(1))]
    output$text_feature_col_ui <- renderUI({
      if (length(char_cols) == 0) {
        helpText("No character/factor columns in the current table.")
      } else {
        selectInput("text_feature_col", "Text/category column:", choices = char_cols)
      }
    })
  })

  feature_data <- reactiveVal(NULL)

  observeEvent(cleaned_data(), {
    feature_data(cleaned_data())
    rv$feature_log <- c("Feature data reset from current cleaned dataset.")
  })

  observeEvent(input$fe_reset, {
    df <- cleaned_data()
    req(df)
    feature_data(df)
    rv$feature_log <- c("Manually reset to cleaned dataset.", rv$feature_log)
    record_event(
      "feature_reset",
      step_name = "feature",
      detail = "feature_table_reset_to_cleaned_data"
    )
    showNotification("Feature table reset to cleaned data.", type = "message")
  })

  fe_formula_preview_val <- eventReactive(input$fe_preview_formula, {
    df <- feature_data()
    req(df, input$formula_text)
    tryCatch({
      val <- safe_eval_formula(input$formula_text, df)
      if (length(val) != nrow(df)) {
        stop("Result length must equal number of rows (", nrow(df), ").")
      }
      list(ok = TRUE, val = val)
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
  })

  output$fe_formula_preview <- renderPrint({
    if (!isTRUE(input$fe_preview_formula > 0)) {
      cat("Click \"Preview only (dry run)\" to validate the formula (nothing is saved).")
      return(invisible())
    }
    r <- fe_formula_preview_val()
    req(r)
    if (!isTRUE(r$ok)) {
      cat("Error: ", r$msg, sep = "")
      return(invisible())
    }
    v <- r$val
    cat("Dry run - not saved.\n")
    cat("Class:", paste(class(v), collapse = ", "), "\n")
    cat("Length:", length(v), "  NA count:", sum(is.na(v)), "\n")
    if (is.numeric(v)) {
      xf <- v[is.finite(v)]
      if (length(xf) > 0) {
        cat("Mean:", mean(xf), "  SD:", sd(xf), "  Min:", min(xf), "  Max:", max(xf), "\n")
      }
    }
    cat("\nFirst rows:\n")
    print(utils::head(data.frame(.value = v), 10))
  })

  observeEvent(input$add_formula_feature, {
    df <- feature_data()
    req(df)
    req(input$new_col_name, input$formula_text)

    tryCatch({
      val <- safe_eval_formula(input$formula_text, df)
      if (length(val) != nrow(df)) {
        stop("Formula result must have the same length as the number of rows.")
      }
      df[[clean_names_custom(input$new_col_name)]] <- val
      feature_data(df)
      rv$feature_log <- c(
        paste0("Added formula-based feature: ", clean_names_custom(input$new_col_name), " = ", input$formula_text),
        rv$feature_log
      )
      record_event(
        "feature_added",
        step_name = "feature",
        detail = paste0("formula:", clean_names_custom(input$new_col_name))
      )
      showNotification("Formula-based feature added successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Feature creation failed:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$add_single_transform, {
    df <- feature_data()
    req(df, input$feature_numeric_col, input$single_transform_name)

    col <- input$feature_numeric_col
    new_name <- clean_names_custom(input$single_transform_name)

    tryCatch({
      x <- df[[col]]
      if (!is.numeric(x)) stop("Selected column is not numeric.")

      out <- switch(
        input$single_transform,
        "square" = x^2,
        "sqrt" = sqrt(abs(x)),
        "log1p" = log1p(pmax(x, 0)),
        "abs" = abs(x),
        "center" = x - mean(x, na.rm = TRUE),
        "zscore" = scale_zscore(x),
        "minmax" = scale_minmax(x),
        "rank" = rank(x, ties.method = "average", na.last = "keep"),
        "inv" = ifelse(x == 0, NA_real_, 1 / x),
        "lag1" = dplyr::lag(x, 1),
        "lead1" = dplyr::lead(x, 1)
      )

      df[[new_name]] <- out
      feature_data(df)
      rv$feature_log <- c(
        paste0("Added transformed feature: ", new_name, " from ", col, " using transformation '", input$single_transform, "'."),
        rv$feature_log
      )
      record_event(
        "feature_added",
        step_name = "feature",
        detail = paste0("transform:", new_name)
      )
      showNotification("Transformed feature added successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Transformation failed:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$add_pair_feature, {
    df <- feature_data()
    req(df, input$feature_num_col_a, input$feature_num_col_b, input$pair_feature_name)

    a <- df[[input$feature_num_col_a]]
    b <- df[[input$feature_num_col_b]]
    new_name <- clean_names_custom(input$pair_feature_name)

    tryCatch({
      if (!is.numeric(a) || !is.numeric(b)) stop("Both selected columns must be numeric.")

      out <- switch(
        input$pair_op,
        "multiply" = a * b,
        "add" = a + b,
        "subtract" = a - b,
        "ratio" = ifelse(b == 0, NA, a / b)
      )

      df[[new_name]] <- out
      feature_data(df)
      rv$feature_log <- c(
        paste0("Added pair feature: ", new_name, " using operation '", input$pair_op, "' on ", input$feature_num_col_a, " and ", input$feature_num_col_b, "."),
        rv$feature_log
      )
      record_event(
        "feature_added",
        step_name = "feature",
        detail = paste0("pair:", new_name)
      )
      showNotification("Pair feature added successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Pair feature creation failed:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$add_binning, {
    df <- feature_data()
    req(df, input$bin_col, input$n_bins, input$bin_feature_name, input$bin_method)

    tryCatch({
      x <- df[[input$bin_col]]
      if (!is.numeric(x)) stop("Selected binning column must be numeric.")
      new_name <- clean_names_custom(input$bin_feature_name)
      nb <- input$n_bins
      if (input$bin_method == "equal") {
        df[[new_name]] <- cut(x, breaks = nb, include.lowest = TRUE, dig.lab = 6)
        method_lbl <- "equal-width"
      } else {
        brks <- unique(as.numeric(quantile(x, probs = seq(0, 1, length.out = nb + 1), na.rm = TRUE)))
        if (length(brks) < 2) {
          stop("Not enough distinct quantiles - try fewer bins or equal-width.")
        }
        df[[new_name]] <- cut(x, breaks = brks, include.lowest = TRUE, dig.lab = 6)
        method_lbl <- "quantile"
      }
      feature_data(df)
      rv$feature_log <- c(
        paste0("Binned: ", new_name, " from ", input$bin_col, " (", method_lbl, ", ", nb, " bins)."),
        rv$feature_log
      )
      showNotification("Binned feature added successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Binning failed:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$add_rolling_mean, {
    df <- feature_data()
    req(df, input$rolling_col, input$rolling_window, input$rolling_name)

    tryCatch({
      x <- df[[input$rolling_col]]
      if (!is.numeric(x)) stop("Rolling mean needs a numeric column.")
      w <- input$rolling_window
      new_name <- clean_names_custom(input$rolling_name)
      df[[new_name]] <- roll_mean_vec(x, w)
      feature_data(df)
      rv$feature_log <- c(
        paste0("Rolling mean: ", new_name, " from ", input$rolling_col, " (window ", w, ")."),
        rv$feature_log
      )
      showNotification("Rolling mean added.", type = "message")
    }, error = function(e) {
      showNotification(paste("Rolling mean failed:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$add_text_feature, {
    df <- feature_data()
    req(df, input$text_feature_name)
    req(input$text_feature_col)

    tryCatch({
      col <- input$text_feature_col
      raw <- as.character(df[[col]])
      new_name <- clean_names_custom(input$text_feature_name)
      out <- switch(
        input$text_feature_op,
        "nchar" = nchar(raw, type = "chars", allowNA = TRUE),
        "words" = str_count(str_trim(raw), "\\S+")
      )
      df[[new_name]] <- out
      feature_data(df)
      rv$feature_log <- c(
        paste0("Text feature: ", new_name, " from ", col, " (", input$text_feature_op, ")."),
        rv$feature_log
      )
      showNotification("Text feature added.", type = "message")
    }, error = function(e) {
      showNotification(paste("Text feature failed:", e$message), type = "error", duration = 8)
    })
  })

  output$fe_inspect_col_ui <- renderUI({
    df <- feature_data()
    req(df)
    nms <- names(df)
    selectInput("fe_inspect_col", "Column to inspect:", choices = nms, selected = nms[length(nms)])
  })

  output$fe_compare_col_ui <- renderUI({
    df <- feature_data()
    req(df)
    nms <- names(df)
    selectInput("fe_compare_col", "Compare to (optional):", choices = c("(none)" = "", nms), selected = "")
  })

  output$fe_impact_plot <- renderPlotly({
    df <- feature_data()
    req(df)
    req(input$fe_inspect_col)
    v <- df[[input$fe_inspect_col]]
    cmp <- input$fe_compare_col
    if (!isTRUE(nzchar(cmp))) {
      if (is.numeric(v)) {
        vv <- v[is.finite(v)]
        if (length(vv) == 0) return(plotly_empty("No finite numeric values."))
        plot_ly(x = vv, type = "histogram", nbinsx = 30, name = input$fe_inspect_col) %>%
          layout(title = "Distribution", xaxis = list(title = input$fe_inspect_col))
      } else {
        tab <- sort(table(as.character(v), useNA = "ifany"), decreasing = TRUE)
        tab <- head(tab, 25)
        plot_ly(x = names(tab), y = as.numeric(tab), type = "bar", name = "count") %>%
          layout(title = "Top levels", xaxis = list(title = NULL))
      }
    } else {
      v2 <- df[[cmp]]
      if (is.numeric(v) && is.numeric(v2)) {
        v1f <- v[is.finite(v)]
        v2f <- v2[is.finite(v2)]
        if (length(v1f) == 0 && length(v2f) == 0) return(plotly_empty("No finite values."))
        p1 <- plot_ly(x = v1f, type = "histogram", nbinsx = 25, name = input$fe_inspect_col, alpha = 0.7)
        p2 <- plot_ly(x = v2f, type = "histogram", nbinsx = 25, name = cmp, alpha = 0.7)
        subplot(p1, p2, titleX = TRUE, margin = 0.06) %>%
          layout(title = "Side-by-side")
      } else {
        plotly_empty("Compare needs two numeric columns.")
      }
    }
  })

  output$fe_profile_table <- renderDT({
    df <- feature_data()
    req(df, input$fe_inspect_col)
    v <- df[[input$fe_inspect_col]]
    if (is.numeric(v)) {
      datatable(numeric_profile_table(v), options = list(dom = "t", scrollX = TRUE))
    } else {
      tab <- sort(table(as.character(v), useNA = "ifany"), decreasing = TRUE)
      tab <- head(tab, 20)
      dt <- data.frame(Level = names(tab), Count = as.integer(tab), stringsAsFactors = FALSE)
      datatable(dt, options = list(dom = "t", scrollX = TRUE))
    }
  })

  output$feature_log <- renderPrint({
    if (length(rv$feature_log) == 0) {
      cat("No feature engineering steps applied yet.")
    } else {
      cat(paste0("- ", rv$feature_log, collapse = "\n"))
    }
  })

  output$feature_preview <- renderDT({
    df <- feature_data()
    req(df)
    datatable(df, options = list(scrollX = TRUE, pageLength = 8))
  })

  # ------ 456 ------
  eda_data <- reactive({
    df <- feature_data()
    req(df)
    df
  })

  output$eda_rows <- renderValueBox({
    df <- eda_data()
    req(df)
    valueBox(nrow(df), "Rows", icon = icon("table"), color = "blue")
  })

  output$eda_cols <- renderValueBox({
    df <- eda_data()
    req(df)
    valueBox(ncol(df), "Columns", icon = icon("columns"), color = "purple")
  })

  output$eda_num <- renderValueBox({
    df <- eda_data()
    req(df)
    valueBox(sum(sapply(df, is.numeric)), "Numeric Columns", icon = icon("calculator"), color = "green")
  })

  output$eda_cat <- renderValueBox({
    df <- eda_data()
    req(df)
    valueBox(sum(!sapply(df, is.numeric)), "Categorical Columns", icon = icon("font"), color = "yellow")
  })

  output$filter_var_ui <- renderUI({
    df <- eda_data()
    req(df)
    selectInput("filter_var", "Select one variable to filter:", choices = c("None", names(df)))
  })

  output$summary_var_ui <- renderUI({
    df <- eda_data()
    req(df)
    selectInput("summary_var", "Variable to inspect:", choices = names(df))
  })

  observeEvent(input$clear_filter, {
    updateSelectInput(session, "filter_var", selected = "None")
  })

  output$filter_control_ui <- renderUI({
    df <- eda_data()
    req(df)

    if (is.null(input$filter_var) || input$filter_var == "None") return(NULL)

    col <- input$filter_var
    x <- df[[col]]

    if (is.numeric(x)) {
      x_valid <- x[is.finite(x)]
      if (length(x_valid) == 0) return(NULL)

      sliderInput(
        "numeric_filter", paste("Range for", col),
        min = floor(min(x_valid)),
        max = ceiling(max(x_valid)),
        value = c(floor(min(x_valid)), ceiling(max(x_valid)))
      )
   } else {
      choices_now <- sort(unique(as.character(x)))
      choices_now[is.na(choices_now)] <- "Missing"

      pickerInput(
        "categorical_filter", paste("Values for", col),
        choices = unique(choices_now),
        selected = unique(choices_now),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    }
  })

  filtered_data <- reactive({
    df <- eda_data()
    req(df)

    if (is.null(input$filter_var) || input$filter_var == "None") return(df)

    col <- input$filter_var
    x <- df[[col]]

    if (is.numeric(x)) {
      req(input$numeric_filter)

      if (isTRUE(input$keep_na_filter)) {
        df <- df %>%
          filter(is.na(.data[[col]]) | (.data[[col]] >= input$numeric_filter[1] & .data[[col]] <= input$numeric_filter[2]))
      } else {
        df <- df %>%
          filter(!is.na(.data[[col]]) & .data[[col]] >= input$numeric_filter[1] & .data[[col]] <= input$numeric_filter[2])
      }

    } else {
      req(input$categorical_filter)

      if (isTRUE(input$keep_na_filter)) {
        df <- df %>%
          filter(is.na(.data[[col]]) | as.character(.data[[col]]) %in% input$categorical_filter)
      } else {
        df <- df %>%
          filter(!is.na(.data[[col]]) & as.character(.data[[col]]) %in% input$categorical_filter)
      }
    }

    df
  })

  output$filtered_preview <- renderDT({
    df <- filtered_data()
    req(df)
    datatable(df, options = list(scrollX = TRUE, pageLength = 8))
  })

  output$summary_stats <- renderDT({
    df <- filtered_data()
    req(df)

    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      return(datatable(data.frame(Message = "No numeric columns available.")))
    }

    summ <- data.frame(
      Variable = num_cols,
      Mean = sapply(df[num_cols], function(x) round(mean(x, na.rm = TRUE), 4)),
      Median = sapply(df[num_cols], function(x) round(median(x, na.rm = TRUE), 4)),
      SD = sapply(df[num_cols], function(x) round(sd(x, na.rm = TRUE), 4)),
      Q1 = sapply(df[num_cols], function(x) round(quantile(x, 0.25, na.rm = TRUE), 4)),
      Q3 = sapply(df[num_cols], function(x) round(quantile(x, 0.75, na.rm = TRUE), 4)),
      Missing = sapply(df[num_cols], function(x) sum(is.na(x)))
    )

    datatable(summ, options = list(scrollX = TRUE, pageLength = 8))
  })

  output$variable_profile <- renderDT({
    df <- filtered_data()
    req(df, input$summary_var)

    x <- df[[input$summary_var]]

    if (is.numeric(x)) {
      prof <- numeric_profile_table(x)
    } else {
      x2 <- as.character(x)
      x2[is.na(x2)] <- "Missing"
      prof <- as.data.frame(sort(table(x2), decreasing = TRUE), stringsAsFactors = FALSE)
      names(prof) <- c("Level", "Count")
      prof$Percent <- round(100 * prof$Count / sum(prof$Count), 2)
    }

    datatable(prof, options = list(scrollX = TRUE, pageLength = 10, dom = 't'))
  })

  output$missing_plot <- renderPlotly({
    df <- filtered_data()
    req(df)

    miss_df <- data.frame(
      Variable = names(df),
      Missing = sapply(df, function(x) sum(is.na(x))),
      MissingPct = round(100 * sapply(df, function(x) mean(is.na(x))), 2)
    )

    p <- ggplot(
      miss_df,
      aes(
        x = reorder(Variable, Missing),
        y = Missing,
        text = paste0("Variable: ", Variable,
                      "<br>Missing count: ", Missing,
                      "<br>Missing %: ", MissingPct, "%")
      )
    ) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(x = "Variable", y = "Missing Count")

    ggplotly(p, tooltip = "text")
  })

  output$dist_var_ui <- renderUI({
    df <- filtered_data()
    req(df)
    selectInput("dist_var", "Variable:", choices = names(df))
  })

  output$dist_plot <- renderPlotly({
    df <- filtered_data()
    req(df, input$dist_var)

    var_name <- input$dist_var
    x <- df[[var_name]]

    if (input$dist_plot_type == "hist") {
      if (!is.numeric(x)) return(plotly_empty("Histogram requires a numeric variable."))

      plot_df <- df %>% filter(is.finite(.data[[var_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No finite numeric values available."))

      p <- ggplot(plot_df, aes(x = .data[[var_name]])) +
        geom_histogram(bins = input$dist_bins, alpha = 0.8) +
        theme_minimal(base_size = 13) +
        labs(x = var_name, y = "Count")

      if (isTRUE(input$show_mean_line)) {
        p <- p + geom_vline(xintercept = mean(plot_df[[var_name]], na.rm = TRUE), linetype = "dashed")
      }

      ggplotly(p)

    } else if (input$dist_plot_type == "box") {
      if (!is.numeric(x)) return(plotly_empty("Boxplot requires a numeric variable."))

      plot_df <- df %>% filter(is.finite(.data[[var_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No finite numeric values available."))

      p <- ggplot(plot_df, aes(x = "", y = .data[[var_name]])) +
        geom_boxplot() +
        theme_minimal(base_size = 13) +
        labs(x = "", y = var_name)

      if (isTRUE(input$show_mean_line)) {
        p <- p + geom_hline(yintercept = mean(plot_df[[var_name]], na.rm = TRUE), linetype = "dashed")
      }

      ggplotly(p)

    } else if (input$dist_plot_type == "density") {
      if (!is.numeric(x)) return(plotly_empty("Density plot requires a numeric variable."))

      plot_df <- df %>% filter(is.finite(.data[[var_name]]))
      if (nrow(plot_df) < 2) return(plotly_empty("Not enough valid values for density plot."))

      p <- ggplot(plot_df, aes(x = .data[[var_name]])) +
        geom_density(alpha = 0.4) +
        theme_minimal(base_size = 13) +
        labs(x = var_name, y = "Density")

      if (isTRUE(input$show_mean_line)) {
        p <- p + geom_vline(xintercept = mean(plot_df[[var_name]], na.rm = TRUE), linetype = "dashed")
      }

      ggplotly(p)

    } else {
      plot_df <- df %>% filter(!is.na(.data[[var_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No non-missing values available."))

      plot_df$cat_view <- top_levels(plot_df[[var_name]], n = input$top_n_cat)

      p <- ggplot(plot_df, aes(x = cat_view)) +
        geom_bar() +
        theme_minimal(base_size = 13) +
        labs(x = var_name, y = "Count")

      ggplotly(p)
    }
  })

  output$x_var_ui <- renderUI({
    df <- filtered_data()
    req(df)
    selectInput("x_var", "X variable:", choices = names(df))
  })

  output$y_var_ui <- renderUI({
    df <- filtered_data()
    req(df)
    selectInput("y_var", "Y variable:", choices = names(df))
  })

  output$color_var_ui <- renderUI({
    df <- filtered_data()
    req(df)
    selectInput("color_var", "Color / Group variable (optional):", choices = c("None", names(df)))
  })

  output$relation_plot <- renderPlotly({
    df <- filtered_data()
    req(df, input$x_var, input$y_var)

    x_name <- input$x_var
    y_name <- input$y_var
    color_name <- if (!is.null(input$color_var) && input$color_var != "None") input$color_var else NULL

    x <- df[[x_name]]
    y <- df[[y_name]]

    plot_mode <- input$relation_type

    if (plot_mode == "auto") {
      if (is.numeric(x) && is.numeric(y)) {
        plot_mode <- "scatter"
      } else if ((!is.numeric(x) && is.numeric(y)) || (is.numeric(x) && !is.numeric(y))) {
        plot_mode <- "box"
      } else {
        plot_mode <- "bar"
      }
    }

    if (plot_mode == "scatter") {
      if (!(is.numeric(x) && is.numeric(y))) {
        return(plotly_empty("Scatter plot requires two numeric variables."))
      }

      plot_df <- df %>% filter(is.finite(.data[[x_name]]), is.finite(.data[[y_name]]))
      if (!is.null(color_name)) plot_df <- plot_df %>% filter(!is.na(.data[[color_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No valid rows available for scatter plot."))

      if (is.null(color_name)) {
        p <- ggplot(plot_df, aes(x = .data[[x_name]], y = .data[[y_name]])) +
          geom_point(alpha = 0.75, size = 2) +
          theme_minimal(base_size = 13)
      } else {
        p <- ggplot(plot_df, aes(x = .data[[x_name]], y = .data[[y_name]], color = .data[[color_name]])) +
          geom_point(alpha = 0.75, size = 2) +
          theme_minimal(base_size = 13)
      }

      if (isTRUE(input$add_smooth)) {
        p <- p + geom_smooth(method = "lm", se = FALSE)
      }

      ggplotly(p)

    } else if (plot_mode == "box") {
      if (!is.numeric(y) && is.numeric(x)) {
        tmp <- x_name
        x_name <- y_name
        y_name <- tmp
      }

      if (!is.numeric(df[[y_name]]) || is.numeric(df[[x_name]])) {
        return(plotly_empty("Boxplot mode needs one categorical and one numeric variable."))
      }

      plot_df <- df %>% filter(!is.na(.data[[x_name]]), is.finite(.data[[y_name]]))
      if (!is.null(color_name)) plot_df <- plot_df %>% filter(!is.na(.data[[color_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No valid rows available for boxplot."))

      if (is.null(color_name)) {
        p <- ggplot(plot_df, aes(x = .data[[x_name]], y = .data[[y_name]])) +
          geom_boxplot() +
          theme_minimal(base_size = 13)
      } else {
        p <- ggplot(plot_df, aes(x = .data[[x_name]], y = .data[[y_name]], fill = .data[[color_name]])) +
          geom_boxplot() +
          theme_minimal(base_size = 13)
      }

      ggplotly(p)

    } else {
      plot_df <- df %>% filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]]))
      if (nrow(plot_df) == 0) return(plotly_empty("No valid rows available for bar chart."))

      count_df <- plot_df %>% count(.data[[x_name]], .data[[y_name]], name = "n")
      names(count_df)[1:2] <- c("xvar", "yvar")

      p <- ggplot(count_df, aes(x = xvar, y = n, fill = yvar)) +
        geom_col(position = "dodge") +
        theme_minimal(base_size = 13) +
        labs(x = x_name, fill = y_name, y = "Count")

      ggplotly(p)
    }
  })

  output$top_corr_table <- renderDT({
    df <- filtered_data()
    req(df)

    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num_df) < 2) {
      return(datatable(data.frame(Message = "Need at least 2 numeric columns.")))
    }

    keep_cols <- sapply(num_df, function(x) {
      finite_vals <- x[is.finite(x)]
      length(unique(finite_vals)) > 1
    })
    num_df <- num_df[, keep_cols, drop = FALSE]

    if (ncol(num_df) < 2) {
      return(datatable(data.frame(Message = "Not enough usable numeric columns.")))
    }

    corr_mat <- suppressWarnings(cor(num_df, use = "pairwise.complete.obs"))
    corr_long <- reshape2::melt(corr_mat)
    corr_long <- corr_long[corr_long$Var1 != corr_long$Var2, ]
    corr_long$pair <- apply(corr_long[, c("Var1", "Var2")], 1, function(z) paste(sort(z), collapse = " | "))
    corr_long <- corr_long[!duplicated(corr_long$pair), ]
    corr_long <- corr_long[order(-abs(corr_long$value)), c("Var1", "Var2", "value")]
    names(corr_long) <- c("Variable_1", "Variable_2", "Correlation")

    datatable(head(corr_long, 10), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$corr_plot <- renderPlotly({
    df <- filtered_data()
    req(df)

    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num_df) < 2) {
      return(plotly_empty("At least 2 numeric columns are required for a correlation heatmap."))
    }

    keep_cols <- sapply(num_df, function(x) {
      finite_vals <- x[is.finite(x)]
      length(unique(finite_vals)) > 1
    })

    num_df <- num_df[, keep_cols, drop = FALSE]

    if (ncol(num_df) < 2) {
      return(plotly_empty("At least 2 numeric columns with non-constant finite values are required."))
    }

    corr_mat <- suppressWarnings(cor(num_df, use = "pairwise.complete.obs"))
    corr_long <- reshape2::melt(corr_mat)

    p <- ggplot(
      corr_long,
      aes(
        x = Var1,
        y = Var2,
        fill = value,
        text = paste0("Var 1: ", Var1,
                      "<br>Var 2: ", Var2,
                      "<br>Correlation: ", round(value, 3))
      )
    ) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), size = 4) +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL, fill = "Correlation")

    ggplotly(p, tooltip = "text")
  })

  # ------ 456 above ------

  observeEvent(input$download_session_log, {
    record_event(
      "download_result",
      step_name = "export",
      detail = "download_type=session_log",
      download_type = "session_log"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$download_quality_summary, {
    record_event(
      "download_result",
      step_name = "export",
      detail = "download_type=quality_summary",
      download_type = "quality_summary"
    )
  }, ignoreInit = TRUE)

  output$download_session_log <- downloadHandler(
    filename = function() {
      paste0("session_log_", rv$session_id, ".csv")
    },
    content = function(file) {
      out <- rv$event_log
      if (nrow(out) == 0) {
        out <- data.frame(
          timestamp = as.character(Sys.time()),
          session_id = rv$session_id,
          app_group = current_group(),
          event_name = "no_events_recorded",
          step_name = NA_character_,
          value = NA_real_,
          detail = NA_character_,
          stringsAsFactors = FALSE
        )
      }
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$download_quality_summary <- downloadHandler(
    filename = function() {
      paste0("quality_summary_", rv$session_id, ".csv")
    },
    content = function(file) {
      write.csv(quality_summary_data(), file, row.names = FALSE)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("processed_data_", rv$session_id, ".csv")
    },
    content = function(file) {
      df <- feature_data()
      if (is.null(df)) df <- cleaned_data()
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(
  ui = function(request) {
    qs <- shiny::parseQueryString(if (is.null(request$QUERY_STRING)) "" else request$QUERY_STRING)
    group <- if (!is.null(qs$group) && nzchar(qs$group)) toupper(qs$group) else "A"
    if (!group %in% c("A", "B")) group <- "A"
    make_ui(group)
  },
  server = server
)
