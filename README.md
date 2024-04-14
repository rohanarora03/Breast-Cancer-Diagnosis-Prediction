# Exploratory-Data-Analysis-For-Breast-Cancer-Diagnosis-Prediction-using-Logistic-Regression

**Description:**
This repository presents an exploratory data analysis (EDA) and logistic regression modeling for predicting breast cancer diagnosis (benign or malignant) based on features extracted from cell nuclei. The analysis utilizes the Wisconsin Diagnostic Breast Cancer [dataset](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data) sourced from Kaggle, comprising 569 instances with 32 attributes.

**Key Highlights:**
- **Dataset Overview:** The dataset includes 30 features computed for each cell nucleus, with mean values of area, smoothness, and symmetry chosen as predictor variables. The outcome variable, diagnosis, is converted into a binary factor variable (Benign/Malignant).

- **Data Cleaning and Preparation:** The dataset underwent cleaning to handle missing values and select relevant predictor and outcome variables. The diagnosis variable was converted into a binary factor, and no outlier analysis was performed based on dataset characteristics.

- **Data Analysis:** Logistic regression was utilized to build a predictive model for breast cancer diagnosis. Assumptions were checked, including multicollinearity, linearity of independent variables, and independence of errors, ensuring the validity of the model.

- **Statistical Analysis:** Statistical tests were conducted to assess the significance of predictor variables and confirm model fit. The analysis included checking p-values, deviance comparisons, AIC values, and confidence intervals for predictor variables.

- **Conclusion:** The logistic regression model effectively predicts cancer diagnosis based on area, smoothness, and symmetry features of cell nuclei. The analysis concludes that these variables significantly influence diagnosis, with smoothness demonstrating particularly strong predictive power.

**Dataset Citation:**
- Kaggle. "Breast Cancer Wisconsin (Diagnostic) Dataset. (2016, September 25)." Available at: [Dataset Link](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data)
