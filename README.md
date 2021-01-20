# Out-of-sample time series forecasting workbench

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/usethis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/usethis/actions)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/tylerJPike/OOS/branch/main/graph/badge.svg?token=AQ4PFWU3KS)](https://codecov.io/gh/tylerJPike/OOS)
<!-- badges: end -->

This package creates a paradigm to provide a structured and automatd approach to out-of-sample time series forecasting, a common, important, and subtle task. In many ways, this package is merely a wrapper for the excellent extant time series forecasting routines on CRAN - including both traditional econometric time series models and modern machine learning techniques. However, this package additionally provides a modern and comprehensive set of forecast combination tools and forecast comparison metrics. 

## Workflow and available Tools

### 0. Data preparation

Clean Outliers
1. Winsorize
2. Trim  

Impute Missing Observations 
1. Interpolation
2. Kalman Filter
3. Fill-Forward
4. Average
5. Moving Average
6. Seasonal Decomposition

Dimension Reduction
1. Principal Components
2. Partial Least Square Scores

### 1. Forecasting models 
Univariate 
1. Random Walk 
2. ARIMA
3. ETS  
4. Spline
5. Theta Method
6. TBATS
7. STL           
8. AR Perceptron 

Multivariate  
1. Vector Autoregression (VAR)  
2. Linear Regression  (OLS)
3. LASSO Regression  
4. Ridge Regression  
5. Elastic Net   
6. Principal Component Regression  
7. Partial Least Squares Regression  
8. Random Forest
9. Tree-Based Gradient Boosting Machine 
10. Single Layered Neural Network  

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

### 4. Chart forecasts

## Model estimation flexibility and accessibility

Users may edit any model training routine through accessing a list of function arguments. For machine learning techniques, this entails editing [caret](https://github.com/topepo/caret) arguments including: tuning grid, control grid, method, and accuracy metric. For univariate time series forecasting, this entails passing arguments to [forecast](https://github.com/robjhyndman/forecast) package model functions. For imputing missing variables, this entails passing arguments to [imputeTS](https://github.com/SteffenMoritz/imputeTS) package functions.

A brief example using an `Arima` model to forecast univariate time series:   

	# 1. create the central list of univariate model training arguments, univariate.forecast.training  
	univariate.forecast.training = instantiate.univariate.forecast.training()  

	# 2. select an item to edit, for example the Arima order to create an ARMA(1,1)   
		# view default model arguments (there are none)  
		univariate.forecast.training$arguments[['Arima']] 
		# add our own function arguments  
		univariate.forecast.training$arguments[['Arima']]$order = c(1,0,1) 

A brief example using the `Random Forest` to combine forecasts:   

	# 1. create the central list of ML training arguments, forecast.combinations.ml.training  
	forecast.combinations.ml.training = instantiate.forecast.combinations.ml.training()  

	# 2. select an item to edit, for example the random forest tuning grid   
		# view default tuning grid  
		forecast.combinations.ml.training$tune.grid[['RF']]  
		# edit tuning grid   
		forecast.combinations.ml.training$tune.grid[['RF']] <- expand.grid(mtry = c(1:6))  

## Basic usage example

	#----------------------------------------
	### Forecasting Example
	#----------------------------------------
	# set data
	quantmod::getSymbols.FRED(c('UNRATE','INDPRO','GS10'), 
							  env = globalenv())
	Data = cbind(UNRATE, INDPRO) %>% cbind(GS10)
	Data = data.frame(Data, date = zoo::index(Data)) %>%
		dplyr::filter(lubridate::year(date) >= 1990)
	
	# create OOS principal components
	# (will not be used)
	Data.factors =
		dimension_reduction(
			Data = Data,
			forecast.date = tail(Data$date),
			target = 'INDPRO',
			method = 'pc',
			ncomp = 2)

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
			impute.verbose = FALSE) 

	# combine forecasts and add in observed values
	forecasts = 
		dplyr::bind_rows(
			forecast.uni,
			forecast.mulit) %>%
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
			Title = 'Shiller/Case House Price Index',
			Ylab = 'Index',
			Freq = 'Monthly')

	chart



---

## Future Extensions
High priority
1. Forecast_combine fails when using only one method
2. Test updated forecast_combine winsorize
2. Add parallel processing ability
3. Add house pricing vingette
4. Add unit tests
5. Add dimension reduction in data cleaning step of forecast_multivariate
7. Fix CW forecast comparison test

Low priority
1. Add a basic genetic algorithm for forecast combinations  
2. Upgrade ML functionality   
	1. Deep NN via Keras  
	2. Xgboost, grf, quantile trees, ect.  
	3. Univariate ts model error correction via NN 
	4. Multivariate joint estimation via trees and NN
4. Demonstrate how to create user-define forecasting methods
