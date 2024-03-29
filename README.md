# Out-of-sample time series forecasting

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![CRAN status](https://www.r-pkg.org/badges/version/OOS)](https://CRAN.R-project.org/package=OOS)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov](https://codecov.io/gh/tylerJPike/OOS/branch/main/graph/badge.svg?token=AQ4PFWU3KS)](https://codecov.io/gh/tylerJPike/OOS)
[![Build Status](https://travis-ci.org/tylerJPike/OOS.svg?branch=main)](https://travis-ci.org/tylerJPike/OOS)
<!-- badges: end -->

Out-of-Sample time series forecasting is a common, important, and subtle task. The OOS package introduces a comprehensive and cohesive API for the out-of-sample forecasting workflow: data preparation, forecasting - including both traditional econometric time series models and modern machine learning techniques - forecast combination, model and error analysis, and forecast visualization. 

The key difference between OOS and the other time series forecasting packages is that it operates out-of-sample by construction. That is, it re-cleans data and re-trains models each forecast.date and is careful not to introduce look-ahead bias into its information set via data cleaning or forecasts via model training. Other packages tend to fit the model once, leaving the user to construct the out-of-sample data cleaning and forecast exercise on their own.

See the OOS package [website](https://tylerjpike.github.io/OOS/) for examples and documentation.

---
## Workflow and available Tools

### 1. Prepare Data

| Clean Outliers | Impute Missing Observations (via [imputeTS](https://github.com/SteffenMoritz/imputeTS)) | Dimension Reduction | 
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

## Model estimation flexibility and accessibility

Users may edit any model training routine through accessing a list of function arguments. For machine learning techniques, this entails editing [caret](https://github.com/topepo/caret) arguments including: tuning grid, control grid, method, and accuracy metric. For univariate time series forecasting, this entails passing arguments to [forecast](https://github.com/robjhyndman/forecast) package model functions. For imputing missing variables, this entails passing arguments to [imputeTS](https://github.com/SteffenMoritz/imputeTS) package functions.

A brief example using an `Arima` model to forecast univariate time series:   

	# 1. create the central list of univariate model training arguments, univariate.forecast.training  
	forecast_univariate.control_panel = instantiate.forecast_univariate.control_panel()  

	# 2. select an item to edit, for example the Arima order to create an ARMA(1,1)   
		# view default model arguments (there are none)  
		forecast_univariate.control_panel$arguments[['Arima']] 
		# add our own function arguments  
		forecast_univariate.control_panel$arguments[['Arima']]$order = c(1,0,1) 

A brief example using the `Random Forest` to combine forecasts:   

	# 1. create the central list of ML training arguments 
	forecast_combinations.control_panel = instantiate.forecast_combinations.control_panel()  

	# 2. select an item to edit, for example the random forest tuning grid   
		# view default tuning grid  
		forecast_combinations.control_panel$tuning.grids[['RF']]  
		# edit tuning grid   
		forecast_combinations.control_panel$tuning.grids[['RF']] = expand.grid(mtry = c(1:6))  
---
## Basic workflow
	#----------------------------------------
	### Forecasting Example
	#----------------------------------------
	# pull and prepare data from FRED
	quantmod::getSymbols.FRED(
		c('UNRATE','INDPRO','GS10'), 
		env = globalenv())
	Data = cbind(UNRATE, INDPRO, GS10)
	Data = data.frame(Data, date = zoo::index(Data)) %>%
		dplyr::filter(lubridate::year(date) >= 1990)

	# run univariate forecasts 
	forecast.uni = 
		forecast_univariate(
			Data = dplyr::select(Data, date, UNRATE),
			forecast.dates = tail(Data$date,15), 
			method = c('naive','auto.arima', 'ets'),      
			horizon = 1,                         
			recursive = FALSE,

			# information set       
			rolling.window = NA,    
			freq = 'month',                   
			
			# outlier cleaning
			outlier.clean = FALSE,
			outlier.variables = NULL,
			outlier.bounds = c(0.05, 0.95),
			outlier.trim = FALSE,
			outlier.cross_section = FALSE,
			
			# impute missing
			impute.missing = FALSE,
			impute.method = 'kalman',
			impute.variables = NULL,
			impute.verbose = FALSE) 

	# create multivariate forecasts
	forecast.multi = 
		forecast_multivariate(
			Data = Data,           
			forecast.date = tail(Data$date,15),
			target = 'UNRATE',
			horizon = 1,
			method = c('ols','lasso','ridge','elastic','GBM'),

			# information set       
			rolling.window = NA,    
			freq = 'month',                   
			
			# outlier cleaning
			outlier.clean = FALSE,
			outlier.variables = NULL,
			outlier.bounds = c(0.05, 0.95),
			outlier.trim = FALSE,
			outlier.cross_section = FALSE,
			
			# impute missing
			impute.missing = FALSE,
			impute.method = 'kalman',
			impute.variables = NULL,
			impute.verbose = FALSE,
			
			# dimension reduction
			reduce.data = FALSE,
			reduce.variables = NULL,
			reduce.ncomp = NULL,
			reduce.standardize = TRUE) 

	# combine forecasts and add in observed values
	forecasts = 
		dplyr::bind_rows(
			forecast.uni,
			forecast.multi) %>%
		dplyr::left_join( 
			dplyr::select(Data, date, observed = UNRATE))

	# forecast combinations 
	forecast.combo = 
		forecast_combine(
			forecasts, 
			method = c('uniform','median','trimmed.mean',
					   'n.best','lasso','peLasso','RF'), 
			burn.in = 5, 
			n.max = 2)

	# merge forecast combinations back into forecasts
	forecasts = 
		forecasts %>%
		dplyr::bind_rows(forecast.combo)

	# calculate forecast errors
	forecast.error = forecast_accuracy(forecasts)

	# view forecast errors from least to greatest 
	#   (best forecast to worst forecast method)
	forecast.error %>% 
		dplyr::mutate_at(vars(-model), round, 3) %>%
		dplyr::arrange(MSE)

	# compare forecasts to the baseline (a random walk)
	forecast_comparison(
		forecasts,
		baseline.forecast = 'naive',  
		test = 'ER',
		loss = 'MSE') %>% 
		arrange(error.ratio)

	# chart forecasts
	chart = 
		chart_forecast(
			forecasts,              
			Title = 'US Unemployment Rate',
			Ylab = 'Index',
			Freq = 'Monthly')

	chart

---
## Contact
If you should have questions, concerns, or wish to collaborate, please contact [Tyler J. Pike](https://tylerjpike.github.io/)
