# Diabetic Foot Infections Advanced ML Analyzer: RShiny Application

This RShiny application provides a comprehensive pipeline for machine learning analysis, including:
* **EDA:** Interactive data exploration and correlation heatmaps.
* **Feature Selection:** Boruta, RFE, and LASSO algorithms.
* **Data Balancing:** SMOTE, Up-Sampling, Down-Sampling, and ROSE.
* **Model Training:** GLM, Random Forest, XGBoost, Neural Networks, and KNN.
* **XAI:** Model interpretation using SHAP values and LIME.

## Installation & Usage
1. Clone this repository.
2. Ensure you have R installed.
3. Run `shiny::runApp()` in the project directory.
4. The application will automatically check and install missing packages defined in `global.R`.

## Citation
If you use this software, please cite it as follows:
