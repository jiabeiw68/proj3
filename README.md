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

## Live Demo

The deployed Shiny app is available here:

**[Launch the app](https://proj3.shinyapps.io/abtesting_random/)**

You can also manually open a specific experimental version through URL parameters:

- **Version A:** `https://proj3.shinyapps.io/abtesting_random/?group=A`
- **Version B:** `https://proj3.shinyapps.io/abtesting_random/?group=B`

---

## A/B Test Design

Two versions of the app were tested:

- **Version A (Control):**  
  A classic dashboard layout with a visible sidebar and free navigation across sections.

- **Version B (Treatment):**  
  A guided, process-oriented version where the sidebar is hidden and users progress through the workflow step by step using **Next** buttons.

Users are assigned to a group automatically at the browser-session level, although the version can also be manually specified through URL parameters.

## Hypothesis

- **H0 (Null Hypothesis):** There is no difference in user behavior between Version A and Version B.  
- **H1 (Alternative Hypothesis):** There is a significant difference in user engagement and task completion between the two interface designs.

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

## Dataset Description

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

### Dataset Purpose

This dataset is designed to evaluate whether interface design affects user behavior. In particular, it allows comparison between Version A and Version B in terms of:

- engagement
- navigation depth
- task completion
- interaction intensity
- bounce behavior

---
## Analysis Approach

The analysis combines exploratory data analysis (EDA) with hypothesis testing to evaluate whether the treatment (Version B) leads to statistically significant differences compared to the control (Version A).

### Exploratory Data Analysis
- dataset structure and consistency checks  
- missing value inspection  
- group balance validation  
- descriptive statistics by A/B group  
- visual comparison of key engagement and conversion metrics  

### Statistical Testing

To formally evaluate the experimental results:

- **Two-sample proportion z-tests** are applied to binary outcomes:
  - click-through rate  
  - completion rate  
  - bounce rate  

- **Welch’s t-tests** are used for continuous outcomes:
  - time spent  
  - page views  
  - steps reached  

Statistical significance is evaluated at the 5% level (α = 0.05), and results are interpreted in the context of both statistical and practical significance.

## Key Findings

The analysis shows that Version B does not outperform Version A on key performance metrics.

- Version B exhibits lower completion and click rates compared to Version A  
- Engagement metrics such as time spent, page views, and steps reached are also lower for Version B  
- Although bounce rate may improve slightly, this does not compensate for the decline in task completion  

Overall, the results suggest that the guided step-by-step interface introduces friction that negatively impacts user performance.

---

## Conclusion

Based on the experimental results, Version A (sidebar-based interface) remains the more effective design for supporting user engagement and task completion.

The findings do not support deploying Version B in its current form. Further iterations may focus on reducing friction in the guided workflow while preserving its structural clarity.


## Repository Structure

```bash
.
├── app_enhanced.R            # Shiny app with A/B assignment and GA4 instrumentation
├── ab_analysis_dataset.csv   # A/B testing dataset used for analysis
├── ab_test_analysis.ipynb    # EDA and statistical analysis notebook
└── README.md
