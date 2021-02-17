# Out-of-sample time series forecasting workbench

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/usethis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/usethis/actions)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/tylerJPike/OOS/branch/main/graph/badge.svg?token=AQ4PFWU3KS)](https://codecov.io/gh/tylerJPike/OOS)
<!-- badges: end -->

This package creates a paradigm to provide a structured and automated approach to out-of-sample time series forecasting, a common, important, and subtle task. In many ways, this package is merely a wrapper for the excellent extant time series forecasting routines on CRAN - including both traditional econometric time series models and modern machine learning techniques. However, this package additionally provides a modern and comprehensive set of forecast combination tools and forecast comparison metrics. 

---
## Workflow and available Tools

### 0. Prepare Data

| Clean Outliers | Impute Missing Observations (via [imputeTS](https://github.com/SteffenMoritz/imputeTS)) | Dimension Reduction | 
|----------------------|------------------------|-----------------------|
| Winsorize | Linear Interpolation | Princepal Components |
| Trim | Kalman Filter | |
|  | Fill-Forward | |
|  | Average | |
|  | Moving Average | |
|  | Seasonal Decomposition | |


### 1. Forecast

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
| Root Mean Square Error (RMSE) | Diebold-Mariano Test (for unnested models) | Errors* |
| Mean Absolute Error (MAE) | Clark and West Test (for nested models) | SHAP decomposition* |
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
		forecast_combinations.control_panel$tune.grid[['RF']]  
		# edit tuning grid   
		forecast_combinations.control_panel$tune.grid[['RF']] = expand.grid(mtry = c(1:6))  
---
## Basic usage example

	#----------------------------------------
	### Forecasting Example
	#----------------------------------------
	# pull and prepare data from FRED
	quantmod::getSymbols.FRED(
		c('UNRATE','INDPRO','GS10'), 
		env = globalenv())
	Data = cbind(UNRATE, INDPRO, GS10)

	# a ts, xts, or zoo object may be used in OOS forecasting methods, 
	# but a data.frame with a `date` column may also be used, as shown below
	Data = data.frame(Data, date = zoo::index(Data)) %>%
		dplyr::filter(lubridate::year(date) >= 1990)

	# run univariate forecasts 
	forecast.uni = 
		forecast_univariate(
			Data = dplyr::select(Data, date, INDPRO),
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
			target = 'INDPRO',
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
			dplyr::select(Data, date, observed = INDPRO))

	# forecast combinations 
	combinations.indpro = 
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
		forecast_chart(
			forecasts,              
			Title = 'Industrial Production',
			Ylab = 'Index',
			Freq = 'Monthly')

	chart

---
## Contact
If you should have questions, concerns, or wish to collaborate, please contact [Tyler J. Pike](https://tylerjpike.github.io/)

---
## Future Extensions
High priority
1. Code
   1. ~~accept `ts` and `xts` objects~~
   2. ~~update ML default training parameters~~
   3. ~~when combo model has same name as forecast model, add .combo to its name~~
   4. error plot
2. Create vingettes
	1. ~~Basic example~~
	2. User defined functions
	3. Forecasting binary outputs - prob of recession example
	4. High dimensional forecasting
3. Create website
    1. ~~add navbar to workflow pages~~
	2. write workflow
	3. ~~change home page from README~~
	4. create sigil
	5. add google analytics
	6. add search bar (maybe)
	7. fix 'wall of text' feel

Medium Priority
1. Charting
	1. term-by-term decomposition time series (see Determinants as example)
	2. SHAP decomposition time series
2. smooth out user-defined functions

Low priority
1. Add a basic genetic algorithm for forecast combinations  
2. Upgrade ML functionality   
	1. Deep NN via Keras  
	2. Xgboost, grf, quantile trees, ect.  
	3. Univariate ts model error correction via NN 
	4. Multivariate joint estimation via trees and NN
3. Add density forecasting
4. Add real time regime detection
5. Automatically handle non-stationary series if desired
6. Add moving average of lags options for trees
