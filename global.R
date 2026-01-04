# global.R

packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "caret", "Boruta", "glmnet", "randomForest", "xgboost", "pROC",
  "reshape2", "shinycssloaders", "GGally",
  "rpart", "rpart.plot", "lime",
  "kernelshap", "shapviz",
  "nnet", "kernlab",
  "recipes", "themis", "rlang"
)

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

theme_set(ggplot2::theme_minimal(base_size = 14))
options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)
