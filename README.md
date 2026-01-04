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

cff-version: 1.2.0
message: "If you use this software, please cite it as below."
authors:
- family-names: "Cakir"
  given-names: "Mustafa"
  orcid: "https://orcid.org/0000-0002-1794-9242"
title: "Advanced ML Analyzer: RShiny Application for Automated Machine Learning and XAI"
version: 1.0.0
doi: 10.5281/zenodo.XXXXX # Zenodo DOI aldıktan sonra burayı güncelleyin
date-released: 2026-01-04
url: "https://github.com/kullaniciadi/repo-adi"
