# server.R

server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Reactive container
  # ----------------------------------------------------------------------------
  vals <- reactiveValues(
    data = NULL,
    fs_results = list(),
    train_data = NULL,
    test_data  = NULL,
    trained_models = list(),
    lime_explainers = list(),
    model_metrics = NULL,
    shap_plots = list(),
    misclassifications = list(),
    model_times = NULL,
    resample_results = NULL
  )
  
  # ----------------------------------------------------------------------------
  # Helpers
  # ----------------------------------------------------------------------------
  mode_level <- function(x) {
    ux <- stats::na.omit(x)
    if (length(ux) == 0) return(NA)
    tab <- sort(table(ux), decreasing = TRUE)
    names(tab)[1]
  }
  
  simple_impute <- function(df) {
    for (nm in names(df)) {
      col <- df[[nm]]
      if (is.numeric(col)) {
        med <- stats::median(col, na.rm = TRUE)
        if (!is.finite(med)) med <- 0
        col[is.na(col)] <- med
        df[[nm]] <- col
      } else if (is.factor(col)) {
        m <- mode_level(col)
        if (!is.na(m)) col[is.na(col)] <- m
        df[[nm]] <- droplevels(col)
      } else if (is.logical(col)) {
        col[is.na(col)] <- FALSE
        df[[nm]] <- as.integer(col)
      } else {
        col <- as.factor(col)
        m <- mode_level(col)
        if (!is.na(m)) col[is.na(col)] <- m
        df[[nm]] <- droplevels(col)
      }
    }
    df
  }
  
  # ----------------------------------------------------------------------------
  # 1) DATA LOAD & PREVIEW
  # ----------------------------------------------------------------------------
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      df <- df |>
        dplyr::mutate(dplyr::across(where(is.character), ~ type.convert(., as.is = TRUE))) |>
        dplyr::mutate(dplyr::across(where(is.logical), as.integer))
      vals$data <- df
    }, error = function(e) {
      showNotification("Error while reading the CSV.", type = "error")
    })
  })
  
  output$contents <- DT::renderDT({
    req(vals$data)
    DT::datatable(vals$data, options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE))
  })
  
  output$target_variable_selector <- renderUI({
    req(vals$data)
    selectInput("target_variable", "Select Target Variable:",
                choices = names(vals$data),
                selected = names(vals$data)[1])
  })
  
  target_var <- reactive({ req(input$target_variable); input$target_variable })
  
  # ----------------------------------------------------------------------------
  # 2) EDA
  # ----------------------------------------------------------------------------
  output$eda_variable_selectors <- renderUI({
    req(vals$data)
    if (input$plot_type %in% c("hist", "bar", "box")) {
      tagList(
        selectInput("eda_var1", "Variable:", choices = names(vals$data)),
        if (input$plot_type == "box") {
          selectInput("eda_var2", "Group by:", choices = names(vals$data), selected = target_var())
        }
      )
    } else if (input$plot_type == "pairs") {
      selectInput("eda_vars_pairs", "Pick 2–6 numeric variables:",
                  choices = names(vals$data)[sapply(vals$data, is.numeric)],
                  multiple = TRUE)
    }
  })
  
  eda_plot_reactive <- reactive({
    req(vals$data, input$plot_type)
    df <- vals$data
    
    if (input$plot_type %in% c("hist", "bar")) {
      req(input$eda_var1)
      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = input$eda_var1)) +
        ggplot2::geom_bar(ggplot2::aes(fill = after_stat(x))) +
        ggplot2::labs(x = input$eda_var1, y = "Count") +
        ggplot2::theme(legend.position = "none")
    } else if (input$plot_type == "box") {
      req(input$eda_var1, input$eda_var2)
      df[[input$eda_var2]] <- as.factor(df[[input$eda_var2]])
      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = input$eda_var2, y = input$eda_var1, fill = input$eda_var2)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = input$eda_var2, y = input$eda_var1) +
        ggplot2::theme(legend.position = "none")
    } else if (input$plot_type == "cor") {
      num_df <- df[, sapply(df, is.numeric), drop = FALSE]
      validate(need(ncol(num_df) >= 2, "Correlation heatmap needs at least two numeric variables."))
      cmat <- stats::cor(num_df, use = "complete.obs")
      m <- reshape2::melt(cmat)
      p <- ggplot2::ggplot(m, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                      midpoint = 0, limit = c(-1, 1)) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()
        )
    } else if (input$plot_type == "pairs") {
      req(input$eda_vars_pairs)
      validate(need(length(input$eda_vars_pairs) >= 2 && length(input$eda_vars_pairs) <= 6,
                    "Please pick 2–6 variables."))
      p <- GGally::ggpairs(df[, input$eda_vars_pairs, drop = FALSE])
    } else {
      p <- NULL
    }
    p
  })
  
  output$eda_plot <- renderPlot({ eda_plot_reactive() })
  
  output$download_eda_plot <- downloadHandler(
    filename = function() {
      pt <- input$plot_type
      if (pt %in% c("hist", "bar")) {
        req(input$eda_var1); paste0("eda_bar_", input$eda_var1, ".png")
      } else if (pt == "box") {
        req(input$eda_var1, input$eda_var2); paste0("eda_box_", input$eda_var1, "_by_", input$eda_var2, ".png")
      } else if (pt == "cor") {
        "eda_correlation.png"
      } else if (pt == "pairs") {
        req(input$eda_vars_pairs); paste0("eda_pairs_", paste(input$eda_vars_pairs, collapse = "_"), ".png")
      } else "eda_plot.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = eda_plot_reactive(), device = "png", width = 10, height = 8, dpi = 300)
    }
  )
  
  # ----------------------------------------------------------------------------
  # 3) FEATURE SELECTION (with robust RFE + LASSO)
  # ----------------------------------------------------------------------------
  observeEvent(input$run_fs, {
    req(vals$data, target_var(), input$fs_method, input$seed_value)
    set.seed(input$seed_value)
    
    df <- vals$data
    # Ensure target is factor
    df[[target_var()]] <- as.factor(df[[target_var()]])
    
    withProgress(message = 'Running feature selection...', value = 0, {
      method_name <- input$fs_method
      
      if (method_name == "boruta") {
        incProgress(0.3, detail = "Boruta")
        bfit <- Boruta::Boruta(stats::as.formula(paste(target_var(), "~ .")),
                               data = stats::na.omit(df), doTrace = 0, maxRuns = 100)
        stats_tbl <- Boruta::attStats(bfit)
        stats_tbl$feature <- rownames(stats_tbl)
        stats_tbl <- dplyr::arrange(stats_tbl, meanImp)
        stats_tbl$feature <- factor(stats_tbl$feature, levels = stats_tbl$feature)
        stats_tbl$decision_factor <- factor(stats_tbl$decision,
                                            levels = c("Confirmed", "Tentative", "Rejected"))
        p <- ggplot2::ggplot(stats_tbl, ggplot2::aes(x = feature, y = meanImp, fill = decision_factor)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = c("Confirmed" = "darkgreen",
                                                "Tentative" = "orange",
                                                "Rejected"  = "darkred")) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) +
          ggplot2::labs(x = "Features", y = "Mean Importance (Z)")
        features <- Boruta::getSelectedAttributes(bfit, withTentative = FALSE)
        vals$fs_results[[method_name]] <- list(features = features, plot = p, method = method_name)
        
      } else if (method_name == "rfe") {
        incProgress(0.3, detail = "RFE")
        
        df_imp <- df
        pred_names <- setdiff(names(df_imp), target_var())
        
        df_imp[pred_names] <- simple_impute(df_imp[pred_names])
        
        y <- droplevels(as.factor(df_imp[[target_var()]]))
        validate(need(nlevels(y) >= 2, "The target variable must have at least two classes for RFE."))
        
        x <- df_imp[, pred_names, drop = FALSE]
        
        class_counts <- table(y)
        if (min(class_counts) < 2) {
          up <- caret::upSample(x, y, yname = "Class")
          x <- subset(up, select = -Class)
          y <- droplevels(up$Class)
          
          validate(need(all(table(y) >= 2), "RFE: At least two observations per class are required, even after up-sampling."))
        }
        
        validate(need(ncol(x) >= 1, "RFE: No predictor variables remain after processing."),
                 need(length(y) >= 5, "RFE: There must be enough observations (at least 5) to run CV."))
        
        
        control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv", number = 5)
        r <- caret::rfe(x, y, sizes = c(5, 10, 15, 20), rfeControl = control)
        
        r_plot_fun <- function() plot(r)
        vals$fs_results[[method_name]] <- list(
          features = caret::predictors(r),
          plot = r_plot_fun,
          method = method_name
        )
        
      } else if (method_name == "lasso") {
        incProgress(0.3, detail = "LASSO CV")
        
        y <- droplevels(as.factor(df[[target_var()]]))
        validate(need(nlevels(y) >= 2, "Target variable must have at least two classes for LASSO."))
        
        x <- stats::model.matrix(stats::as.formula(paste(target_var(), "~ . -1")), data = df)
        
        keep_rows <- stats::complete.cases(x) & !is.na(y)
        x <- x[keep_rows, , drop = FALSE]
        y <- y[keep_rows]
        
        class_tab <- table(y)
        if (any(class_tab < 2)) {
          xdf <- as.data.frame(x)
          up <- caret::upSample(x = xdf, y = y, yname = "Class")
          x <- as.matrix(subset(up, select = -Class))
          y <- droplevels(up$Class)
          class_tab <- table(y)
        }
        
        fam <- if (nlevels(y) == 2) "binomial" else "multinomial"
        validate(need(all(table(y) >= 2),
                      "After balancing, at least two observations per class are required for LASSO."))
        
        set.seed(input$seed_value)
        cvfit <- glmnet::cv.glmnet(x, y, alpha = 1, family = fam)
        
        plot_fun <- function() plot(cvfit)
        
        if (fam == "binomial") {
          coefs <- stats::coef(cvfit, s = "lambda.min")
          feat <- rownames(coefs)[as.vector(coefs != 0) & rownames(coefs) != "(Intercept)"]
        } else {
          coef_list <- stats::coef(cvfit, s = "lambda.min")
          nz <- unique(unlist(lapply(coef_list, function(m) {
            rownames(m)[as.vector(m != 0)]
          })))
          feat <- setdiff(nz, "(Intercept)")
        }
        
        vals$fs_results[[method_name]] <- list(features = feat, plot = plot_fun, method = method_name)
      }
    })
  })
  
  current_fs_result <- reactive({
    req(input$fs_method, vals$fs_results[[input$fs_method]])
    vals$fs_results[[input$fs_method]]
  })
  
  output$fs_plot <- renderPlot({
    res <- current_fs_result()
    if (is.function(res$plot)) res$plot() else print(res$plot)
  })
  
  output$fs_results_display <- renderUI({
    res_list <- vals$fs_results
    if (length(res_list) == 0) return(p("Run a feature selection method."))
    tagList(lapply(names(res_list), function(nm) {
      tagList(
        h4(paste(toupper(nm), "Features (", length(res_list[[nm]]$features), "):")),
        verbatimTextOutput(paste0("fs_text_", nm))
      )
    }))
  })
  
  observe({
    for (nm in names(vals$fs_results)) {
      local({
        ln <- nm
        output[[paste0("fs_text_", ln)]] <- renderPrint({
          cat(vals$fs_results[[ln]]$features, sep = ", ")
        })
      })
    }
  })
  
  output$download_fs_plot <- downloadHandler(
    filename = function() paste0("fs_", input$fs_method, "_plot.png"),
    content  = function(file) {
      res <- current_fs_result()
      if (is.function(res$plot)) {
        png(file, width = 10, height = 8, units = "in", res = 300); res$plot(); dev.off()
      } else {
        ggplot2::ggsave(file, plot = res$plot, device = "png", width = 10, height = 8, dpi = 300)
      }
    }
  )
  
  output$download_features_list <- downloadHandler(
    filename = function() paste0("selected_features_", input$fs_method, ".txt"),
    content  = function(file) writeLines(current_fs_result()$features, file)
  )
  
  # ----------------------------------------------------------------------------
  # 4) DATA BALANCING PREVIEW (SMOTE, Up, Down, ROSE)
  # ----------------------------------------------------------------------------
  
  # Dynamic title for the plot box
  output$smote_plot_title <- renderUI({
    if (is.null(input$run_smote_viz) || input$run_smote_viz == 0) {
      h3("Class Distribution: Before vs. After Resampling")
    } else {
      h3(paste("Class Distribution: Before vs. After", input$resampling_method_viz))
    }
  })
  
  smote_comparison_plot_reactive <- eventReactive(input$run_smote_viz, {
    req(vals$data, target_var(), input$resampling_method_viz)
    
    # Initial Data Preparation
    df <- vals$data
    validate(need(nrow(stats::na.omit(df)) > 0, "Please ensure there are no missing rows in the data."))
    
    df <- stats::na.omit(df)
    target_name <- target_var()
    method <- input$resampling_method_viz
    
    # Target variable preparation and validation
    if (!is.factor(df[[target_name]])) df[[target_name]] <- factor(df[[target_name]])
    
    n_levels <- nlevels(df[[target_name]])
    validate(need(n_levels >= 2, "Target needs at least two classes."))
    
    if (method == "ROSE" && n_levels > 2) {
      stop("ROSE is only supported for binary (2-class) classification problems.")
    }
    
    split_ratio <- if (!is.null(input$smote_split)) input$smote_split else 0.8
    seed <- if (!is.null(input$seed_value)) input$seed_value else 123
    set.seed(seed)
    
    idx <- caret::createDataPartition(df[[target_name]], p = split_ratio, list = FALSE)
    before <- df[idx, , drop = FALSE]
    
    if (method == "SMOTE") {
      class_counts <- table(before[[target_name]])
      if (any(class_counts < 2)) {
        showNotification("SMOTE: Up-sampling extremely rare classes in train set to prevent failure.", type = "warning", duration = 5)
        
        x_up <- before |> dplyr::select(-dplyr::all_of(target_name))
        y_up <- before[[target_name]]
        
        # Use caret::upSample for classes < 2 obs
        up_df <- caret::upSample(x = x_up, y = y_up, yname = target_name)
        before <- up_df
        before[[target_name]] <- droplevels(before[[target_name]])
      }
    }
    
    rec <- recipes::recipe(stats::as.formula(paste(target_name, "~ .")), data = before) |>
      recipes::step_dummy(recipes::all_nominal_predictors())
    
    if (method == "SMOTE") {
      rec <- rec |> themis::step_smote(recipes::all_outcomes(), over_ratio = 1, neighbors = 2) 
    } else if (method == "Up") {
      rec <- rec |> themis::step_upsample(recipes::all_outcomes())
    } else if (method == "Down") {
      rec <- rec |> themis::step_downsample(recipes::all_outcomes())
    } else if (method == "ROSE") {
      rec <- rec |> themis::step_rose(recipes::all_outcomes())
    }
    
    juiced <- recipes::juice(recipes::prep(rec))
    
    before_plot <- before[, target_name, drop = FALSE]
    before_plot$State <- paste0("Before ", method)
    
    juiced_plot <- juiced[, target_name, drop = FALSE]
    juiced_plot$State <- paste0("After ", method)
    
    combined <- dplyr::bind_rows(before_plot, juiced_plot)
    combined$State <- factor(combined$State, levels = c(paste0("Before ", method), paste0("After ", method)))
    
    ggplot2::ggplot(combined, ggplot2::aes_string(x = target_name)) +
      ggplot2::geom_bar(ggplot2::aes_string(fill = target_name)) +
      ggplot2::geom_text(stat = "count", ggplot2::aes(label = after_stat(count)), vjust = -0.5) +
      ggplot2::facet_wrap(~ State) +
      ggplot2::labs(x = "Class", y = "Count") +
      ggplot2::theme(legend.position = "none",
                     strip.text = ggplot2::element_text(size = 12, face = "bold"),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  
  output$smote_comparison_plot <- renderPlot({ smote_comparison_plot_reactive() })
  
  output$download_smote_plot <- downloadHandler(
    filename = function() {
      pt <- input$resampling_method_viz
      sp <- if (!is.null(input$smote_split)) round(input$smote_split * 100) else 80
      paste0("resampling_", pt, "_train", sp, ".png")
    },
    content  = function(file) {
      p <- smote_comparison_plot_reactive() 
      ggplot2::ggsave(file, p, width = 10, height = 7, dpi = 300)
    }
  )
  
  # ----------------------------------------------------------------------------
  # Utility for file name prefix
  # ----------------------------------------------------------------------------
  filename_prefix <- reactive({
    train_split <- if (!is.null(input$train_split)) input$train_split else 0.8
    cv <- if (!is.null(input$cv_folds)) input$cv_folds else 10
    seed <- if (!is.null(input$seed_value)) input$seed_value else 123
    fs_name <- if (!is.null(input$feature_set)) tolower(input$feature_set) else "none"
    train_pct <- round(train_split * 100); test_pct <- 100 - train_pct
    paste0("FS-", fs_name, "_seed-", seed, "_cv-", cv, "_split-", train_pct, "-", test_pct)
  })
  
  # ----------------------------------------------------------------------------
  # 5) MODEL TRAINING
  # ----------------------------------------------------------------------------
  
  observeEvent(vals$data, {
    req(vals$data, target_var())
    all_features <- setdiff(names(vals$data), target_var())
    
    vals$fs_results[["none"]] <- list(
      features = all_features,
      plot = NULL, 
      method = "NONE"
    )
  }, ignoreNULL = FALSE) 
  
  output$feature_set_selector <- renderUI({
    req(length(vals$fs_results) > 0)
    
    choices_list <- names(vals$fs_results)
    choices_list <- c("NONE", setdiff(choices_list, "none")) 
    
    selectInput("feature_set", "Use Features From:",
                choices = toupper(choices_list),
                selected = "NONE") 
  })
  
  observeEvent(input$train_models_btn, {
    req(vals$data, target_var(), input$feature_set, input$seed_value, input$cv_folds, input$train_split, input$selected_models, input$training_resampling_method)
    
    withProgress(message = 'Preparing data & training models...', value = 0, {
      
      incProgress(0.1, detail = "1. Preparing data and splitting sets...")
      
      fs_method <- tolower(input$feature_set)
      
      if (fs_method == "none") {
        features <- setdiff(names(vals$data), target_var())
      } else {
        features <- vals$fs_results[[fs_method]]$features
      }
      
      df <- vals$data[, c(features, target_var()), drop = FALSE]
      df <- stats::na.omit(df)
      
      set.seed(input$seed_value)
      idx <- caret::createDataPartition(df[[target_var()]], p = input$train_split, list = FALSE)
      vals$train_data <- df[idx, , drop = FALSE]
      vals$test_data  <- df[-idx, , drop = FALSE]
      
      if (!is.factor(vals$train_data[[target_var()]])) {
        vals$train_data[[target_var()]] <- factor(vals$train_data[[target_var()]])
        vals$test_data[[target_var()]]  <- factor(vals$test_data[[target_var()]],
                                                  levels = levels(vals$train_data[[target_var()]]))
      }
      if (all(levels(vals$train_data[[target_var()]]) %in% c("0", "1"))) {
        vals$train_data[[target_var()]] <- factor(vals$train_data[[target_var()]],
                                                  levels = c("0", "1"),
                                                  labels = c("Survived", "Died"))
        vals$test_data[[target_var()]]  <- factor(vals$test_data[[target_var()]],
                                                  levels = c("0", "1"),
                                                  labels = c("Survived", "Died"))
      } else if (all(levels(vals$train_data[[target_var()]]) %in% c("2", "1"))) {
        vals$train_data[[target_var()]] <- factor(vals$train_data[[target_var()]],
                                                  levels = c("2", "1"),
                                                  labels = c("Survived", "Died"))
        vals$test_data[[target_var()]]  <- factor(vals$test_data[[target_var()]],
                                                  levels = c("2", "1"),
                                                  labels = c("Survived", "Died"))
      }
      
      incProgress(0.1, detail = "2. Setting up Cross-Validation controls...")
      
      resampling_method <- input$training_resampling_method
      n_levels <- nlevels(vals$train_data[[target_var()]])
      
      if (resampling_method == "rose" && n_levels > 2) {
        showNotification("Warning: ROSE is only for binary classification. Using 'None'.", type = "warning", duration = 8)
        resampling_method <- "none"
      }
      
      if (resampling_method != "none") {
        ctrl <- caret::trainControl(
          method = "cv", number = input$cv_folds,
          summaryFunction = caret::twoClassSummary, classProbs = TRUE,
          savePredictions = "all", sampling = resampling_method 
        )
      } else {
        ctrl <- caret::trainControl(
          method = "cv", number = input$cv_folds,
          summaryFunction = caret::twoClassSummary, classProbs = TRUE,
          savePredictions = "all"
        )
      }

      vals$trained_models <- list()
      vals$shap_plots <- list()
      vals$misclassifications <- list()
      
      total_models <- length(input$selected_models)
      step_increment <- 0.8 / total_models 
      
      for (mn in input$selected_models) {
        incProgress(step_increment, detail = paste("Training:", mn))
        set.seed(input$seed_value)
        fitted <- caret::train(
          stats::as.formula(paste(target_var(), "~ .")),
          data = vals$train_data,
          method = mn,
          trControl = ctrl,
          metric = "ROC",
          preProcess = c("center", "scale")
        )
        vals$trained_models[[mn]] <- fitted
      }
      
      incProgress(0.1, detail = "3. Finalizing results and evaluation objects...")
    })
  })
  
  output$training_log <- renderPrint({ 
    req(length(vals$trained_models) > 0)
    
    log_messages <- character(0)
    for (mn in names(vals$trained_models)) {
      log_messages <- c(log_messages, paste("Model", mn, "trained successfully."))
    }
    cat(log_messages, sep = "\n")
  })
  
  # ----------------------------------------------------------------------------
  # 6) EVALUATION / ROC / MISCLASSIFICATIONS / RESAMPLES
  # ----------------------------------------------------------------------------
  observeEvent(vals$trained_models, {
    req(length(vals$trained_models) > 0, vals$train_data, vals$test_data)
    
    sets <- list(Train = vals$train_data, Test = vals$test_data)
    all_metrics <- list()
    roc_list <- list()
    vals$misclassifications <- list()
    tlist <- list()
    
    for (sname in names(sets)) {
      d <- sets[[sname]]
      for (mname in names(vals$trained_models)) {
        mdl <- vals$trained_models[[mname]]
        preds <- predict(mdl, newdata = d)
        probs <- predict(mdl, newdata = d, type = "prob")
        
        if (nlevels(d[[target_var()]]) == 2) {
          roc_obj <- pROC::roc(response = d[[target_var()]],
                               predictor = probs[, "Died"],
                               levels = c("Survived", "Died"))
          auc_val <- round(pROC::auc(roc_obj), 3)
          roc_data <- data.frame(
            FPR = 1 - roc_obj$specificities,
            TPR = roc_obj$sensitivities,
            Model = paste0(mname, " (AUC = ", auc_val, ")")
          )
          
        } else {
          auc_val <- NA
          roc_data <- NULL 
        }
        
        cm <- caret::confusionMatrix(preds, d[[target_var()]], positive = "Died")
        
        metr <- data.frame(
          Model = mname, DataSet = sname,
          AUC = auc_val,
          Accuracy = round(cm$overall['Accuracy'], 3),
          Sensitivity = round(cm$byClass['Sensitivity'], 3),
          Specificity = round(cm$byClass['Specificity'], 3),
          F1_Score = round(cm$byClass['F1'], 3),
          row.names = NULL
        )
        all_metrics[[paste(sname, mname)]] <- metr
        
        if (sname == "Test" && !is.null(roc_data)) {
          roc_list[[mname]] <- roc_data
          
          mis_idx <- which(preds != d[[target_var()]])
          if (length(mis_idx) > 0) {
            mis <- d[mis_idx, , drop = FALSE]
            vals$misclassifications[[mname]] <- cbind(
              "Truth" = mis[[target_var()]],
              "Predicted" = preds[mis_idx],
              "Prob_Survived" = round(probs[mis_idx, "Survived"], 3),
              "Prob_Died" = round(probs[mis_idx, "Died"], 3),
              mis[, setdiff(names(mis), target_var()), drop = FALSE]
            )
          }
        }
      }
    }
    
    for (mname in names(vals$trained_models)) {
      tm <- vals$trained_models[[mname]]$times$everything
      elapsed <- suppressWarnings(as.numeric(tm["elapsed"]))
      tlist[[mname]] <- data.frame(Model = mname, Time = round(elapsed, 2))
    }
    
    vals$model_metrics <- list(metrics = do.call(rbind, all_metrics),
                               roc_data = if(length(roc_list) > 0) do.call(rbind, roc_list) else data.frame())
    vals$model_times  <- do.call(rbind, tlist)
    vals$resample_results <- caret::resamples(vals$trained_models)
  })
  
  output$metrics_table <- DT::renderDT({
    req(vals$model_metrics)
    DT::datatable(vals$model_metrics$metrics, rownames = FALSE, options = list(pageLength = 10))
  })
  
  output$roc_curve_plot <- renderPlot({
    req(vals$model_metrics, nrow(vals$model_metrics$roc_data) > 0)
    ggplot2::ggplot(vals$model_metrics$roc_data, ggplot2::aes(x = FPR, y = TPR, color = Model)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_abline(linetype = "dashed") +
      ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate") +
      ggplot2::theme(legend.position = "bottom")
  })
  
  output$model_selector_cm <- renderUI({
    req(length(vals$trained_models) > 0)
    selectInput("cm_model_select", "Select Model:", choices = names(vals$trained_models))
  })
  
  output$confusion_matrix_plot <- renderPlot({
    req(input$cm_model_select, vals$test_data)
    mdl <- vals$trained_models[[input$cm_model_select]]
    preds <- predict(mdl, newdata = vals$test_data)
    cm <- caret::confusionMatrix(preds, vals$test_data[[target_var()]], positive = "Died")
    tbl <- as.data.frame(cm$table)
    ggplot2::ggplot(tbl, ggplot2::aes(x = Prediction, y = Reference, fill = Freq)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = Freq), size = 6) +
      ggplot2::scale_fill_gradient(low = "white", high = "cornflowerblue") +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
  })
  
  output$training_time_plot <- renderPlot({
    req(vals$model_times)
    ggplot2::ggplot(vals$model_times, ggplot2::aes(x = reorder(Model, -Time), y = Time, fill = Model)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Time, "s")), vjust = -0.3) +
      ggplot2::labs(x = "Model", y = "Total Training Time (s)") +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  
  output$cv_metric_selector <- renderUI({
    req(vals$resample_results)
    cols <- colnames(vals$resample_results$values)
    metric_cols <- grep("~", cols, value = TRUE)
    metrics <- unique(vapply(strsplit(metric_cols, "~"), function(z) z[[2]], character(1)))
    selectInput("cv_metric", "CV Metric:", choices = metrics, selected = metrics[1])
  })
  
  output$cv_boxplot <- renderPlot({
    req(vals$resample_results, input$cv_metric)
    long <- reshape2::melt(vals$resample_results$values)
    tmp <- do.call(rbind, strsplit(as.character(long$variable), "~"))
    long$Model  <- tmp[, 1]
    long$Metric <- tmp[, 2]
    plot_data <- dplyr::filter(long, Metric == input$cv_metric)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(Model, value, FUN = median), y = value, fill = Model)) +
      ggplot2::geom_boxplot() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Model", y = paste("CV scores –", input$cv_metric)) +
      ggplot2::theme(legend.position = "none")
  })
  
  output$download_metrics <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_metrics.csv"),
    content  = function(file) utils::write.csv(vals$model_metrics$metrics, file, row.names = FALSE)
  )
  
  output$download_roc_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_roc_curves.png"),
    content  = function(file) {
      p <- ggplot2::ggplot(vals$model_metrics$roc_data, ggplot2::aes(x = FPR, y = TPR, color = Model)) +
        ggplot2::geom_line(linewidth = 1.2) + ggplot2::geom_abline(linetype = "dashed") +
        ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate") +
        ggplot2::theme(legend.position = "bottom")
      ggplot2::ggsave(file, p, width = 8, height = 7, dpi = 300)
    }
  )
  
  output$download_cm_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_cm_", input$cm_model_select, ".png"),
    content  = function(file) {
      mdl <- vals$trained_models[[input$cm_model_select]]
      preds <- predict(mdl, newdata = vals$test_data)
      cm <- caret::confusionMatrix(preds, vals$test_data[[target_var()]], positive = "Died")
      tbl <- as.data.frame(cm$table)
      p <- ggplot2::ggplot(tbl, ggplot2::aes(x = Prediction, y = Reference, fill = Freq)) +
        ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = Freq), size = 6) +
        ggplot2::scale_fill_gradient(low = "white", high = "cornflowerblue") +
        ggplot2::theme(legend.position = "none",
                       axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
      ggplot2::ggsave(file, p, width = 6, height = 5, dpi = 300)
    }
  )
  
  output$download_training_time_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_training_times.png"),
    content  = function(file) {
      p <- ggplot2::ggplot(vals$model_times, ggplot2::aes(x = reorder(Model, -Time), y = Time, fill = Model)) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::geom_text(ggplot2::aes(label = paste0(Time, "s")), vjust = -0.3) +
        ggplot2::labs(x = "Model", y = "Total Training Time (s)") +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      ggplot2::ggsave(file, p, width = 8, height = 7, dpi = 300)
    }
  )
  
  output$download_cv_boxplot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_cv_boxplot_", input$cv_metric, ".png"),
    content  = function(file) {
      long <- reshape2::melt(vals$resample_results$values)
      tmp <- do.call(rbind, strsplit(as.character(long$variable), "~"))
      long$Model  <- tmp[, 1]
      long$Metric <- tmp[, 2]
      plot_data <- dplyr::filter(long, Metric == input$cv_metric)
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(Model, value, FUN = median), y = value, fill = Model)) +
        ggplot2::geom_boxplot() + ggplot2::coord_flip() +
        ggplot2::labs(x = "Model", y = paste("CV scores –", input$cv_metric)) +
        ggplot2::theme(legend.position = "none")
      ggplot2::ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  
  # ----------------------------------------------------------------------------
  # 7) INTERPRETATION (GLM, rpart, SHAP, LIME)
  # ----------------------------------------------------------------------------
  output$interpretation_model_selector <- renderUI({
    req(length(vals$trained_models) > 0)
    selectInput("interp_model_select", "Select a Model to Interpret:",
                choices = names(vals$trained_models))
  })
  
  output$interpretation_ui_output <- renderUI({
    req(input$interp_model_select)
    nm <- input$interp_model_select
    black_box <- c("rf", "xgbTree", "nnet", "knn")
    
    if (nm == "glm") {
      tagList(
        shinydashboard::box(width = 12, status = "primary", title = "Logistic Regression Formula",
                            shinycssloaders::withSpinner(uiOutput("glm_formula_display")),
                            downloadButton("download_glm_formula", "Save Formula (TXT)")), 
        
        shinydashboard::box(width = 12, status = "info", title = "Coefficient and Significance Table",
                            shinycssloaders::withSpinner(DTOutput("glm_table")),
                            downloadButton("download_glm_table", "Save Table (CSV)"))
      )
    } else if (nm == "rpart") {
      shinydashboard::box(width = 12, status = "primary", title = "Decision Tree",
                          shinycssloaders::withSpinner(plotOutput("dt_plot", height = "800px")),
                          downloadButton("download_dt_plot", "Save Plot"))
    } else if (nm %in% black_box) {
      shinydashboard::tabBox(width = 12, id = "bb_interp",
                             tabPanel("Global (SHAP)",
                                      p("Feature importance (kernel SHAP)."),
                                      shinycssloaders::withSpinner(plotOutput("shap_summary_plot", height = "600px")),
                                      downloadButton("download_shap_plot", "Save SHAP Plot")),
                             tabPanel("Local (LIME)",
                                      p("Pick a case from the test set."),
                                      uiOutput("lime_case_selector"),
                                      shinycssloaders::withSpinner(plotOutput("lime_plot", height = "600px")),
                                      downloadButton("download_lime_plot", "Save LIME Plot"))
      )
    }
  })
  
  output$glm_formula_display <- renderUI({
    req(vals$trained_models$glm, input$interp_model_select == "glm")
    model <- vals$trained_models$glm
    coefs <- stats::coef(model$finalModel)
    
    terms <- character(0)
    for (i in 2:length(coefs)) {
      sgn <- ifelse(coefs[i] >= 0, " + ", " - ")
      terms <- c(terms, paste0(sgn, round(abs(coefs[i]), 3), " \\times \\texttt{", names(coefs)[i], "}"))
    }

    fstr <- paste0("log\\left(\\frac{p}{1-p}\\right) &= ", round(coefs[1], 3))
    
    max_terms_per_line <- 2 
    
    for (i in seq_along(terms)) {
      if ((i - 1) %% max_terms_per_line == 0) {
        fstr <- paste0(fstr, " \\\\ & \\quad ")
      }
      fstr <- paste0(fstr, terms[i])
    }
    
    latex_output <- paste0("$$\\begin{aligned}", fstr, "\\end{aligned}$$")
    
    withMathJax(latex_output)
  })

  output$dt_plot <- renderPlot({
    req(input$interp_model_select == "rpart", !is.null(vals$trained_models$rpart))
    
    train_data <- vals$train_data 
    target_name <- target_var()
    
    mdl_plot <- rpart::rpart(stats::as.formula(paste(target_name, "~ .")), 
                             data = train_data, 
                             method = "class")
    
    # Karar ağacını çiz
    rpart.plot::rpart.plot(mdl_plot, 
                           box.palette = "auto", 
                           roundint = FALSE, 
                           fallen.leaves = TRUE, 
                           extra = 101,
                           clip.facs = TRUE,
                           main = "Decision Tree (Original Scale, for Visualization)") 
  })
  
  
  output$download_dt_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_decision_tree_original_scale.png"),
    content  = function(file) {
      png(file, width = 12, height = 10, units = "in", res = 300)
      
      train_data <- vals$train_data
      target_name <- target_var()
      
      mdl_plot <- rpart::rpart(stats::as.formula(paste(target_name, "~ .")), 
                               data = train_data, 
                               method = "class")
      
      # Karar ağacını çiz
      rpart.plot::rpart.plot(mdl_plot, 
                             box.palette = "auto", 
                             roundint = FALSE, 
                             fallen.leaves = TRUE, 
                             extra = 101,
                             clip.facs = TRUE,
                             main = "Decision Tree (Original Scale, for Visualization)") 
      
      dev.off()
    }
  )
  
  observeEvent(vals$trained_models, {
    req(length(vals$trained_models) > 0, vals$train_data)
    withProgress(message = 'Computing interpretation objects...', value = 0, {
      bg <- head(vals$train_data[, setdiff(names(vals$train_data), target_var()), drop = FALSE], 50)
      black_box <- c("rf", "xgbTree", "nnet", "knn")
      
      incProgress(0.2, detail = "LIME explainers")
      for (mn in names(vals$trained_models)) {
        if (mn %in% black_box) {
          mdl <- vals$trained_models[[mn]]
          x <- vals$train_data[, caret::predictors(mdl), drop = FALSE]
          vals$lime_explainers[[mn]] <- lime::lime(x, mdl, bin_continuous = TRUE)
        }
      }
      
      incProgress(0.5, detail = "SHAP (kernel)")
      for (mn in black_box) {
        if (!is.null(vals$trained_models[[mn]])) {
          mdl <- vals$trained_models[[mn]]
          pfun <- function(m, d) { predict(m, newdata = d, type = "prob")[, "Died"] }
          shap_vals <- tryCatch(
            kernelshap::kernelshap(mdl, X = bg, bg_X = bg, pred_fun = pfun),
            error = function(e) NULL
          )
          if (!is.null(shap_vals)) {
            sv <- shapviz::shapviz(shap_vals)
            vals$shap_plots[[mn]] <- shapviz::sv_importance(sv, kind = "bar") +
              ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
          }
        }
      }
    })
  })
  
  output$shap_summary_plot <- renderPlot({
    req(input$interp_model_select, vals$shap_plots[[input$interp_model_select]])
    vals$shap_plots[[input$interp_model_select]]
  })
  
  output$download_shap_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_shap_", input$interp_model_select, ".png"),
    content  = function(file) ggplot2::ggsave(file, vals$shap_plots[[input$interp_model_select]], width = 8, height = 8, dpi = 300)
  )
  
  output$lime_case_selector <- renderUI({
    req(vals$test_data)
    sliderInput("lime_case_idx", "Test Row to Explain:",
                min = 1, max = nrow(vals$test_data), value = 1, step = 1)
  })
  
  output$lime_plot <- renderPlot({
    req(input$interp_model_select, input$lime_case_idx, vals$lime_explainers[[input$interp_model_select]])
    mn <- input$interp_model_select
    mdl <- vals$trained_models[[mn]]
    xcols <- caret::predictors(mdl)
    rowx <- vals$test_data[input$lime_case_idx, xcols, drop = FALSE]
    expl <- lime::explain(rowx, vals$lime_explainers[[mn]], n_features = 10, n_labels = 1)
    lime::plot_features(expl) + ggplot2::labs(title = NULL)
  })
  
  output$download_lime_plot <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_lime_", input$interp_model_select, "_case-", input$lime_case_idx, ".png"),
    content  = function(file) {
      mn <- input$interp_model_select
      mdl <- vals$trained_models[[mn]]
      xcols <- caret::predictors(mdl)
      rowx <- vals$test_data[input$lime_case_idx, xcols, drop = FALSE]
      expl <- lime::explain(rowx, vals$lime_explainers[[mn]], n_features = 10, n_labels = 1)
      p <- lime::plot_features(expl) + ggplot2::labs(title = NULL)
      ggplot2::ggsave(file, p, width = 8, height = 7, dpi = 300)
    }
  )
  
  output$glm_table <- DT::renderDT({
    req(vals$trained_models$glm, input$interp_model_select == "glm")
    model <- vals$trained_models$glm
    
    s <- summary(model$finalModel)
    
    coef_df <- as.data.frame(s$coefficients)
    coef_df$Feature <- rownames(coef_df)
    
    coef_df <- coef_df |>
      dplyr::select(Feature, Estimate, `Std. Error`, `Pr(>|z|)`) |>
      dplyr::rename(
        Coefficient = Estimate,
        StdError = `Std. Error`,
        P_Value = `Pr(>|z|)`
      )
    
    DT::datatable(
      coef_df,
      options = list(dom = 't', paging = FALSE, searching = FALSE),
      rownames = FALSE
    ) |>
      DT::formatStyle('P_Value', target = 'row',
                      backgroundColor = DT::styleInterval(0.05, c('#d9edf7', 'white')),
                      fontWeight = DT::styleInterval(0.05, c('bold', 'normal')))
  })
  # --- GLM Formülünü TXT olarak indirme ---
  output$download_glm_formula <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_glm_formula.txt"),
    content = function(file) {
      req(vals$trained_models$glm)
      model <- vals$trained_models$glm
      coefs <- stats::coef(model$finalModel)
      
      fstr <- paste0("log(p/(1-p)) = ", round(coefs[1], 3))
      for (i in 2:length(coefs)) {
        sgn <- ifelse(coefs[i] >= 0, " + ", " - ")
        fstr <- paste0(fstr, sgn, round(abs(coefs[i]), 3), " * ", names(coefs)[i])
      }
      
      writeLines(fstr, file)
    }
  )
  
  output$download_glm_table <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_glm_coefficients.csv"),
    content = function(file) {
      req(vals$trained_models$glm)
      model <- vals$trained_models$glm
      s <- summary(model$finalModel)
      coef_df <- as.data.frame(s$coefficients)
      coef_df$Feature <- rownames(coef_df)
      
      coef_df <- coef_df |>
        dplyr::select(Feature, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`) |>
        dplyr::rename(
          Coefficient = Estimate,
          StdError = `Std. Error`,
          Z_Value = `z value`,
          P_Value = `Pr(>|z|)`
        )
      
      utils::write.csv(coef_df, file, row.names = FALSE)
    }
  )
  # ----------------------------------------------------------------------------
  # 8) ERROR ANALYSIS
  # ----------------------------------------------------------------------------
  
  output$error_model_selector <- renderUI({
    req(length(vals$trained_models) > 0)
    selectInput("error_model_select", "Inspect Errors for Model:", choices = names(vals$trained_models))
  })
  
  output$misclassified_table <- DT::renderDT({
    req(input$error_model_select, !is.null(vals$misclassifications[[input$error_model_select]]))
    DT::datatable(vals$misclassifications[[input$error_model_select]],
                  options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE))
  })
  
  output$download_misclassifications <- downloadHandler(
    filename = function() paste0(filename_prefix(), "_misclassifications_", input$error_model_select, ".csv"),
    content  = function(file) utils::write.csv(vals$misclassifications[[input$error_model_select]], file, row.names = FALSE)
  )
  
  # ----------------------------------------------------------------------------
  # 9) PREDICTION
  # ----------------------------------------------------------------------------
  output$model_selector_pred <- renderUI({
    req(length(vals$trained_models) > 0)
    selectInput("pred_model_select", "1. Choose Model:", choices = names(vals$trained_models))
  })
  
  # server.R (9. PREDICTION, output$prediction_controls bloğu, ~Satır 1109)
  
  output$prediction_controls <- renderUI({
    req(input$pred_model_select, vals$trained_models[[input$pred_model_select]], vals$train_data)
    
    mdl <- vals$trained_models[[input$pred_model_select]]
    

    fs_method <- tolower(input$feature_set)
    if (fs_method == "none") {
      feats <- setdiff(names(vals$data), target_var())
    } else {
      feats <- vals$fs_results[[fs_method]]$features
    }
    
    dfc <- vals$train_data[, feats, drop = FALSE] 
    
    tagList(
      h4("2. Enter Patient Data:"),
      lapply(feats, function(f) {
        col <- dfc[[f]]
        
        validate(
          need(f %in% names(dfc), paste("Error: Predictor column not found in training data:", f))
        )
        
        if (is.numeric(col)) {
          sliderInput(paste0("pred_", f), label = f,
                      min = min(col, na.rm = TRUE),
                      max = max(col, na.rm = TRUE),
                      value = stats::median(col, na.rm = TRUE))
        } else {
          selectInput(paste0("pred_", f), label = f, choices = levels(droplevels(as.factor(col))))
        }
      })
    )
  })
  
  
  prediction_risk <- eventReactive(input$predict_btn, {
    req(input$pred_model_select, vals$trained_models[[input$pred_model_select]])
    mdl <- vals$trained_models[[input$pred_model_select]]
    
    fs_method <- tolower(input$feature_set)
    if (fs_method == "none") {
      feats <- setdiff(names(vals$data), target_var())
    } else {
      feats <- vals$fs_results[[fs_method]]$features
    }
    
    in_list <- lapply(feats, function(f) input[[paste0("pred_", f)]])
    names(in_list) <- feats
    
    newx <- as.data.frame(in_list, stringsAsFactors = FALSE)
    
    for (f in feats) {
      if (is.numeric(vals$train_data[[f]])) {
        newx[[f]] <- as.numeric(newx[[f]])
      } else {
        newx[[f]] <- factor(newx[[f]], levels = levels(as.factor(vals$train_data[[f]])))
      }
    }
    
    probs <- predict(mdl, newdata = newx, type = "prob")
    probs[["Died"]]
  })
  
  output$prediction_result_box <- shinydashboard::renderValueBox({
    r <- prediction_risk(); req(r)
    shinydashboard::valueBox(
      paste0(round(r * 100, 2), "%"),
      "Predicted Mortality Risk",
      color = ifelse(r > 0.5, "red", "green"),
      icon  = shiny::icon("heart-pulse")
    )
  })
}