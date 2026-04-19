# Project3: Data Explorer Pro: A/B Testing for an Interactive Data Analysis App

## Overview

This project evaluates two interface designs for **Data Explorer Pro**, an interactive Shiny application built to help users explore, process, and analyze datasets in an intuitive and scalable way.

The app supports a complete end-to-end workflow, including:

- data upload or built-in dataset selection
- data cleaning and preprocessing
- feature engineering
- exploratory data analysis (EDA)
- export of processed data and session logs

The main goal of this project is to determine whether a **traditional sidebar-based interface** or a **step-by-step workflow interface** leads to better user engagement and task completion.

---

## A/B Test Design

Two versions of the app were tested:

- **Version A (Control):**  
  A classic dashboard layout with a visible sidebar and free navigation across sections.

- **Version B (Treatment):**  
  A guided, process-oriented version where the sidebar is hidden and users progress through the workflow step by step using **Next** buttons.

Users are assigned to a group automatically at the browser-session level, although the version can also be manually specified through URL parameters:

- `/?group=A`
- `/?group=B`

---

## App Workflow

The application is organized into six main stages:

1. **User Guide**  
   Introduces the app and recommended workflow.

2. **Load Data**  
   Users can upload files (`.csv`, `.xlsx`, `.xls`, `.json`, `.rds`) or select built-in datasets.

3. **Cleaning & Preprocessing**  
   Includes:
   - duplicate removal
   - missing value handling
   - outlier treatment using the IQR rule
   - scaling
   - categorical encoding

4. **Feature Engineering**  
   Includes:
   - formula-based feature creation
   - single-column transformations
   - pairwise numeric features
   - binning
   - rolling means
   - text-derived features

5. **EDA**  
   Includes:
   - summary statistics
   - variable profiling
   - distributions
   - relationship plots
   - missingness visualization
   - top correlations
   - correlation heatmaps

6. **Export**  
   Users can download:
   - processed dataset
   - session log
   - quality summary

---

## Data Collection

The app is instrumented with **Google Analytics 4 (GA4)** and an internal session log system to capture user behavior during the experiment.

### Tracked Events

Examples of tracked events include:

- A/B group assignment
- step entry and exit
- dwell time per step
- dataset selection
- file upload
- preprocessing actions
- feature engineering actions
- task completion
- session end

### Key Custom Dimensions / Metrics

The app tracks variables such as:

- `app_group`
- `step_name`
- `completion_type`
- `data_source`
- `dataset_name`
- `file_name`
- `last_step`
- `dwell_seconds`
- `total_session_seconds`

In addition to GA4, the app also exports reproducible session-level logs and quality summaries for downstream analysis.

---

## Analysis Dataset

The final analytical dataset used for the A/B test contains **5,000 user-level observations** and **8 variables**. Each row represents one user session and records both the assigned interface condition and the resulting behavioral outcomes.

### Variables

- **user_id**: unique identifier for each user/session  
- **group**: A/B treatment assignment  
  - `A` = sidebar-based interface  
  - `B` = step-by-step workflow interface  
- **click**: binary indicator of whether the user performed a key click action  
- **completed**: binary indicator of whether the user completed the target task  
- **time_spent**: amount of time spent in the app during the session  
- **page_views**: number of pages or views visited during the session  
- **steps_reached**: number of workflow steps reached by the user  
- **bounce**: binary indicator of whether the user exited quickly without substantial interaction  

### Variable Types

The dataset includes both **binary outcome variables** and **continuous/count-based engagement variables**:

- **Binary variables**: `click`, `completed`, `bounce`
- **Continuous / count variables**: `time_spent`, `page_views`, `steps_reached`

### Purpose of the Dataset

This dataset is designed to evaluate whether interface design affects user behavior. In particular, it supports comparison between Version A and Version B in terms of:

- engagement
- navigation depth
- task completion
- interaction intensity
- bounce behavior

---

## Analysis Approach

The accompanying notebook performs both **exploratory data analysis** and **statistical testing** to compare the two interface conditions.

### Exploratory Data Analysis
- dataset shape and structure checks
- missing value inspection
- group balance inspection
- descriptive statistics by A/B group
- visual comparison of binary and continuous outcomes

### Statistical Testing
- **two-sample proportion z-tests** for binary outcomes such as:
  - click-through rate
  - completion rate
  - bounce rate

- **Welch’s t-tests** for continuous outcomes such as:
  - time spent
  - page views
  - steps reached

This combination of EDA and hypothesis testing helps evaluate whether interface design changes lead to meaningful differences in user engagement and task success.

---

## Repository Structure

```bash
.
├── app_enhanced.R            # Shiny app with A/B assignment and GA4 instrumentation
├── ab_analysis_dataset.csv   # A/B testing dataset used for analysis
├── ab_test_analysis.ipynb    # EDA and statistical analysis notebook
└── README.md
