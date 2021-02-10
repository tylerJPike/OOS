
# Out-of-sample time series forecasting workbench

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/usethis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/usethis/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/tylerJPike/OOS/branch/main/graph/badge.svg?token=AQ4PFWU3KS)](https://codecov.io/gh/tylerJPike/OOS)
<!-- badges: end -->

This package creates a paradigm to provide a structured and automated approach to out-of-sample time series forecasting, a common, important, and subtle task. In many ways, this package is merely a wrapper for the excellent extant time series forecasting routines on CRAN - including both traditional econometric time series models and modern machine learning techniques. However, this package additionally provides a modern and comprehensive set of forecast combination tools and forecast comparison metrics. 

Available tools and techniques may be reviewed under the **Tools** tab. While vignettes and other extended documentation of the OOS package's capabilities may be found under the **Workflow** tab.

---
## Why use OOS

It is a simple fact that in-sample time series forecasting overstates model accuracy. Moreover, standard data cleaning routines and cross-validation techniques do not respect and avoid look-ahead bias, infecting the information set of study. In sum, practioners need to be careful when evaluating time series forecasts. 

As it stands (as of Feb. 2021), most would agree that the clostest package or collection of packages to a comprehensive and cohesive time series forecasting workflow in R is the maturing TidyModels. While TidyModels provides a flexible framework for data cleaning, model estimation, and prediction, it has not been built with out-of-sample time series forecasting in mind. That is, one still needs to write for-loop, apply function, or map routine to re-estimate thier desired forecasting workflow each time step, and then collect, clean, and deliver the mess of output that comes out of such a practice. 

In an answer to R's lack of out-of-sample time series forecasting frameworks, OOS provides a clean, comprehensive, and robust workflow for out-of-sample forecasting. 

In addition to systematizing and democratizing out-of-sample time series forecasting with traditional econometric tools and modern machine learning methods, OOS also:
1. plays nicely with the tidyverse - facilitating integration with existing codebases   
2. provides sensible off-the-shelf defaults for new forecasters, while giving advanced users full control over all model and trianing function parameters for maximum flexibility
3. gives users control over what pieces of information to return in easy to handle data.frames or lists  

---
## Installation

One may install OOS through the package's [GitHub](https://github.com/tylerJPike/OOS) page directly, or via either 

    devtools::install_github('tylerJPike/OOS')

or 

    remotes::install_github('tylerJPike/OOS')

---
	
## Basic workflow example
	#----------------------------------------
	### Forecasting Example
	#----------------------------------------
	# pull and prepare data from FRED
	quantmod::getSymbols.FRED(
		c('UNRATE','INDPRO','GS10'), 
		env = globalenv())
	Data = cbind(UNRATE, INDPRO) %>% cbind(GS10)
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

## Contact
If you should have questions, concerns, or wish to collaborate, please contact [Tyler J. Pike](https://tylerjpike.github.io/)