# ==============================================================================
# ui.R
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Advanced ML Analyzer"),
  
  dashboardSidebar(
    numericInput("seed_value", "Global Seed for Reproducibility:", value = 123, min = 1),
    hr(),
    sidebarMenu(
      id = "tabs",
      menuItem("1. Load Data", tabName = "load_data", icon = icon("database")),
      menuItem("2. Exploratory Data Analysis", tabName = "eda", icon = icon("chart-pie")),
      menuItem("3. Feature Selection", tabName = "feature_selection", icon = icon("filter")),
      menuItem("4. Data Balancing (SMOTE)", tabName = "balancing", icon = icon("balance-scale")),
      menuItem("5. Model Training", tabName = "model_training", icon = icon("cogs")),
      menuItem("6. Model Evaluation", tabName = "model_evaluation", icon = icon("check-circle")),
      menuItem("7. Model Interpretation", tabName = "model_interpretation", icon = icon("project-diagram")),
      menuItem("8. Error Analysis", tabName = "error_analysis", icon = icon("search-plus")),
      menuItem("9. Risk Prediction", tabName = "prediction", icon = icon("user-md"))
    )
  ),
  
  dashboardBody(
    withMathJax(),
    tabItems(
      tabItem(
        tabName = "load_data", h2("Upload and Preview Data"),
        box(width = 12, status = "primary", solidHeader = TRUE,
            fileInput("file1", "Choose CSV File", accept = ".csv"),
            uiOutput("target_variable_selector")),
        box(width = 12, status = "info", title = "Data Preview",
            solidHeader = TRUE, withSpinner(DTOutput("contents")))
      ),
      
      tabItem(
        tabName = "eda", h2("Exploratory Data Analysis"),
        fluidRow(
          box(title = "Plot Controls", status = "primary", solidHeader = TRUE, width = 4,
              selectInput("plot_type", "Select Plot Type",
                          choices = c("Histogram/Bar Chart" = "bar", "Box Plot" = "box",
                                      "Correlation Heatmap" = "cor", "Pairs Plot" = "pairs")),
              uiOutput("eda_variable_selectors"),
              downloadButton("download_eda_plot", "Save Plot (300 DPI)")
          ),
          box(title = "Generated Plot", status = "info", solidHeader = TRUE, width = 8,
              withSpinner(plotOutput("eda_plot", height = "600px")))
        )
      ),
      
      tabItem(
        tabName = "feature_selection", h2("Feature Selection Techniques"),
        fluidRow(
          box(title = "Selection Controls", status = "primary", solidHeader = TRUE, width = 4,
              p("The Global Seed in the sidebar will be used for reproducibility."),
              radioButtons("fs_method", "Select Method:",
                           choices = c("Boruta" = "boruta", "RFE" = "rfe", "LASSO" = "lasso")),
              actionButton("run_fs", "Run Feature Selection", icon = icon("play-circle"), class = "btn-success"),
              hr(), uiOutput("fs_results_display"),
              downloadButton("download_features_list", "Save Feature List")
          ),
          box(title = "Feature Selection Plot", status = "info", solidHeader = TRUE, width = 8,
              withSpinner(plotOutput("fs_plot", height = "600px")),
              downloadButton("download_fs_plot", "Save Plot (300 DPI)")
          )
        )
      ),
      
      tabItem(
        tabName = "balancing", h2("Data Balancing with SMOTE"),
        fluidRow(
          shinydashboard::box(
            width = 12, 
            status = "primary", 
            title = "Resampling Controls",
            sliderInput("smote_split", "Train/Test Split used for Resampling preview:",
                        min = 0.5, max = 0.9, value = 0.8, step = 0.05),
            
            radioButtons("resampling_method_viz", "Select Resampling Method:",
                         choices = c("SMOTE", "Up", "Down", "ROSE"),
                         selected = "SMOTE", inline = TRUE),
            
            actionButton("run_smote_viz", "Generate/Update Comparison Plot", icon = shiny::icon("chart-bar"))
          ),
          box(title = "Class Distribution: Before vs. After SMOTE", status = "success", solidHeader = TRUE, width = 12,
              withSpinner(plotOutput("smote_comparison_plot")),
              downloadButton("download_smote_plot", "Save Comparison Plot (300 DPI)")
          )
        )
      ),
      
      tabItem(
        tabName = "model_training", h2("Train Machine Learning Models"),
        fluidRow(
          box(title = "Training Parameters", status = "primary", solidHeader = TRUE, width = 12,
              p("The Global Seed in the sidebar will be used for reproducibility."),
              uiOutput("feature_set_selector"),
              selectInput("training_resampling_method", "Select Resampling Method for Training:",
                          choices = c("None" = "none",
                                      "SMOTE" = "smote",
                                      "Up-Sample" = "up",
                                      "Down-Sample" = "down",
                                      "ROSE" = "rose"),
                          selected = "none"),
              hr(), # Ayırıcı olarak bir yatay çizgi ekleyin
              numericInput("cv_folds", "Number of Cross-Validation Folds:", value = 10, min = 2, max = 20),
              sliderInput("train_split", "Train/Test Split Ratio:", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
              checkboxGroupInput("selected_models", "Select Models to Train:",
                                 choices = c(
                                   "Logistic Regression" = "glm",
                                   "Decision Tree" = "rpart",
                                   "Random Forest" = "rf",
                                   "XGBoost" = "xgbTree",
                                   "Neural Network" = "nnet",
                                   "k-Nearest Neighbors" = "knn"
                                 ),
                                 selected = c("glm", "rpart", "rf", "xgbTree", "nnet", "knn"),
                                 inline = TRUE),
              actionButton("train_models_btn", "Train Models", icon = icon("play-circle"), class = "btn-success")
          ),
          box(title = "Training Log", status = "info", solidHeader = TRUE, width = 12,
              withSpinner(verbatimTextOutput("training_log")))
        )
      ),
      
      tabItem(
        tabName = "model_evaluation", h2("Model Performance Evaluation"),
        fluidRow(
          box(title = "Performance Metrics (Train vs. Test)", status = "primary", solidHeader = TRUE, width = 12,
              withSpinner(DTOutput("metrics_table")),
              downloadButton("download_metrics", "Save Metrics Table")
          )
        ),
        fluidRow(
          box(title = "ROC Curves (Test Set)", status = "info", solidHeader = TRUE, width = 12,
              withSpinner(plotOutput("roc_curve_plot")),
              downloadButton("download_roc_plot", "Save Plot (300 DPI)")
          )
        ),
        fluidRow(
          box(title = "Confusion Matrix (Test Set)", status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("model_selector_cm"),
              withSpinner(plotOutput("confusion_matrix_plot")),
              downloadButton("download_cm_plot", "Save Plot (300 DPI)")
          ),
          box(title = "Model Training Times", status = "info", solidHeader = TRUE, width = 6,
              withSpinner(plotOutput("training_time_plot")),
              downloadButton("download_training_time_plot", "Save Plot (300 DPI)")
          )
        ),
        fluidRow(
          box(title = "Cross-Validation (CV) Results Distribution", status = "primary", solidHeader = TRUE, width = 12,
              p("This plot shows the distribution of performance scores for each model across all CV folds. It helps to assess model stability."),
              uiOutput("cv_metric_selector"),
              withSpinner(plotOutput("cv_boxplot")),
              downloadButton("download_cv_boxplot", "Save Plot (300 DPI)")
          )
        )
      ),
      
      tabItem(
        tabName = "model_interpretation", h2("Model Interpretation"),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE, uiOutput("interpretation_model_selector"))),
        fluidRow(uiOutput("interpretation_ui_output"))
      ),
      
      tabItem(
        tabName = "error_analysis", h2("Error Analysis (Misclassifications)"),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              p("This table shows the patients from the test set that were incorrectly classified by the selected model."),
              uiOutput("error_model_selector"),
              downloadButton("download_misclassifications", "Download This Table")),
          box(width = 12, status = "info", title = "Misclassified Rows",
              withSpinner(DTOutput("misclassified_table")))
        )
      ),
      
      tabItem(
        tabName = "prediction", h2("Individual Patient Risk Prediction"),
        fluidRow(
          box(title = "1. Select Model & Enter Patient Data", status = "primary", solidHeader = TRUE, width = 5,
              uiOutput("model_selector_pred"), hr(), uiOutput("prediction_controls"), br(),
              actionButton("predict_btn", "Calculate Risk", icon = icon("calculator"), class = "btn-primary")),
          box(title = "2. Prediction Result", status = "success", solidHeader = TRUE, width = 7,
              valueBoxOutput("prediction_result_box", width = 12))
        )
      )
    )
  )
)
