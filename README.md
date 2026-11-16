# Football Player Performance Decline Prediction Using Machine Learning

## Project Overview

This project was developed as part of my Master's thesis in Business Intelligence at Aarhus University.

The objective was to investigate whether machine learning models can predict if a professional football player will experience a decline in performance based on historical performance data.

By analyzing player statistics across multiple seasons, the project explores how data-driven methods can help identify players at risk of declining performance.

## Research Question

*How can machine learning models be used to predict whether football players will experience a decline in performance based on historical performance data?*

## Dataset

The project uses historical football player statistics sourced from a publicly available Kaggle dataset containing WhoScored performance statistics.

The dataset covers players competing in major European domestic leagues across multiple seasons.

Data preparation involved:

- Cleaning and preprocessing historical player statistics.
- Handling missing values and inconsistent observations.
- Filtering players based on available historical performance data.
- Creating new features to capture historical player performance.
- Constructing a binary target variable indicating whether a player experienced a decline in performance.

## Methodology

The project follows the **CRISP-DM framework**, covering data understanding, data preparation, modeling, evaluation, and interpretation.

### Feature Engineering

A player performance metric was developed by combining WhoScored ratings with the proportion of available playing minutes:

**Performance = WhoScored Rating × √(Percentage of Minutes Played)**

Historical and future performance were calculated using weighted averages across multiple seasons.

The difference between historical and future performance was used to construct the target variable:

- **Decline (1):** The player's future performance was lower than their historical performance.
- **No Decline (0):** The player's future performance was equal to or higher than their historical performance.

### Machine Learning Models

Several classification algorithms were implemented and compared:

- Logistic Regression
- Random Forest
- XGBoost
- Gradient Boosting
- Support Vector Machine (SVM)
- K-Nearest Neighbors (KNN)
- Naive Bayes
- Elastic Net

### Model Evaluation

The models were evaluated using cross-validation and classification performance metrics.

Particular attention was given to:

- Classification accuracy
- ROC AUC
- Hyperparameter tuning
- Class imbalance
- Overfitting and generalization

## Key Findings

The analysis demonstrated that historical player statistics can provide useful information for predicting future performance decline.

The different machine learning models achieved relatively comparable predictive performance. The results highlight both the potential and limitations of using historical performance data to predict changes in professional football players' performance.

## Technologies and Skills

- **Programming:** R
- **Data Analysis:** Data cleaning, exploratory data analysis, statistical analysis
- **Machine Learning:** Binary classification, model training, hyperparameter tuning
- **Feature Engineering:** Performance metrics and weighted historical features
- **Model Evaluation:** Cross-validation, ROC AUC, accuracy
- **Methodology:** CRISP-DM

## Project Purpose

This project demonstrates my ability to work with real-world datasets, perform feature engineering, develop predictive machine learning models, and evaluate their performance.

It combines my academic background in Business Intelligence with an interest in applying data science to practical problems in professional football.

## Author

**Oskar Konnerup Mosberg**

Master's Degree in Business Intelligence  
Aarhus University
