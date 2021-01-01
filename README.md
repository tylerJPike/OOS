# Out-of-sample time series forecasting workbench

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/usethis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/usethis/actions)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

In many ways, this package is merely a wrapper for the excellent exant time series forecasting routines on 
CRAN -- including both traditional econometric time series models and modern machine learning techniques. 
However, this paradigm is meant to provide a structured approach to out-of-sample time series  forecasting, 
a common, important, and subtle task. This package further provides a modern and comprehensive set of forecast 
combination tools and forecast comparison metrics. 

## Workflow and Available Tools
### 1. Forecasting models 
Univariate 
1. Random Walk 
2. ARIMA
3. ETS  
4. Theta Method
5. TBATS
6. STL           
7. AR Perceptron 

Multivariate  
1. Vector Autoregression (VAR)  
2. Linear Regression  (OLS)
3. LASSO Regression  
4. Ridge Regression  
5. Elastic Net   
6. Random Forest
7. Tree-Based Gradient Boosting Machine 
8. Single Layered Neural Network  

### 2. Forecast combination techniques
1. Mean
2. Median
3. Trimmed (Winsorized) Mean 
4. N-best
5. Linear Regression
6. Ridge Regression 
7. Lasso Regression
8. Elastic Net
9. peLASSO
10. Random Forest
11. Tree-Based Gradient Boosting Machine 
12. Single Layered Neural Network  

### 3. Accuracy metrics and comparisons
Loss Functions
1. Mean Square Error (MSE)
2. Root Mean Square Error (RMSE)
3. Mean Absolute Error (MAE)
4. Mean Absolute Percentage Error (MAPE)

Forecast Comparison Methods
1. Forecast Error Ratios
2. Diebold-Mariano Test (for unnested models)
3. Clark and West Test (for nested models)

## Machine learning accessibility

Users may edit the ML training routines through accessing a list of caret arguments including: tuning grid, control grid, method, and accuracy metric. A brief example using the random forest to combine forecasts is shown:   

	# 1. create the central list of ML training arguments, forecast.combinations.ml.training  
	forecast.combinations.ml.training = instantiate.forecast.combinations.ml.training()  

	# 2. select an item to edit, for example the random forest tuning grid   
		# view default tuning grid  
		forecast.combinations.ml.training$tune.grid[['RF']]  
		# edit tuning grid   
		forecast.combinations.ml.training$tune.grid[['RF']] <- expand.grid(mtry = c(1:6))  
	
---

## Function List
1. forecast_univariate
2. forecast_multivariate
3. forecast_flatten
4. forecast_combine
5. forecast_accuracy
6. forecast_comparison

## Notes
univariate forecasting models 
1. STL models require seasonal ts objects
2. nnetar models do not estimate a standard error

## To-do
proper package
1. Create a vingette
2. Proper documentation
3. Proper warnings and errors

Second stage  
1. Add a basic genetic algorithm for forecast combinations  
2. Upgrade ML functionality   
	1. deep NN via Keras  
	2. xgboost, grf, quantile trees, ect.  
	3. univariate ts model error correction via NN  
3. Add dimension reduction routines (pca, pls, dfm)  
4. Add basic plotting functionality
5. Demonstrate create a new function for each piece of the framework
6. Multivariate automatic lag selection
7. Multivariate joint estimation via trees and NN
8. convert to tidymodels framework where possible