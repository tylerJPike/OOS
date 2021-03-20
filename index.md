# Out-of-sample time series forecasting

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![CRAN status](https://www.r-pkg.org/badges/version/OOS)](https://CRAN.R-project.org/package=OOS)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov](https://codecov.io/gh/tylerJPike/OOS/branch/main/graph/badge.svg?token=AQ4PFWU3KS)](https://codecov.io/gh/tylerJPike/OOS)
[![Build Status](https://travis-ci.org/tylerJPike/OOS.svg?branch=main)](https://travis-ci.org/tylerJPike/OOS)
<!-- badges: end -->

Out-of-Sample time series forecasting is a common, important, and subtle task. The OOS package introduces a comprehensive and cohesive API for the out-of-sample forecasting workflow: data preparation, forecasting - including both traditional econometric time series models and modern machine learning techniques - forecast combination, model and error analysis, and forecast visualization. 

The key difference between OOS and the other time series forecasting packages is that it operates out-of-sample by construction. That is, OOS functions are designed to re-clean data and re-train models each forecast date and are careful not to introduce look-ahead bias into its information set via data cleaning or forecasts via model training. This framework is in contrast to other packages which tend to focus on cleaning data or fitting a forecast model once, leaving the user to construct the out-of-sample exercise on their own.

Available tools and techniques are summarized below and may be reviewed under the **Tools** tab. While vignettes and other extended documentation of the OOS package's capabilities may be found under the **Workflow** tab.

--- 

## Workflow and available Tools

### 1. Prepare Data

| Clean Outliers | Impute Missing Data (via [imputeTS](https://github.com/SteffenMoritz/imputeTS)) | Dimension Reduction | 
|----------------------|------------------------|-----------------------|
| Winsorize | Linear Interpolation | Principal Components |
| Trim | Kalman Filter | |
|  | Fill-Forward | |
|  | Average | |
|  | Moving Average | |
|  | Seasonal Decomposition | |


### 2. Forecast

| Univariate Forecasts (via [forecast](https://github.com/robjhyndman/forecast)) | Multivariate Forecasts (via [caret](https://github.com/topepo/caret)) | Forecast Combinations |
|----------------------|------------------------|-----------------------|
| Random Walk | Vector Autoregression | Mean|
| ARIMA | Linear Regression | Median |
| ETS | LASSO Regression | Trimmed (Winsorized) Mean |
| Spline | Ridge Regression | N-Best |
| Theta Method | Elastic Net | Linear Regression |
| TBATS | Principal Component Regression | LASSO Regression |
| STL | Partial Least Squares Regression | Ridge Regression |
| AR Perceptron | Random Forest | Partial Egalitarian LASSO |
|  | Tree-Based Gradient Boosting Machine | Principal Component Regression | 
|   |  Single Layered Neural Network  | Partial Least Squares Regression  |
|  | | Random Forest |
|  | | Tree-Based Gradient Boosting Machine |
|  | | Single Layered Neural Network  |


### 3. Analyze

| Accuracy | Compare | Visualize |
|----------------------|------------------------|-----------------------|
| Mean Square Error (MSE) | Forecast Error Ratios | Forecasts |
| Root Mean Square Error (RMSE) | Diebold-Mariano Test (for unnested models) | Errors |
| Mean Absolute Error (MAE) | Clark and West Test (for nested models) |  |
| Mean Absolute Percentage Error (MAPE) |  | |


---
## Contact
If you should have questions, concerns, or wish to collaborate, please contact [Tyler J. Pike](https://tylerjpike.github.io/)