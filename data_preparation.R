#---------------------------------------------
# data cleaning helper functions
#---------------------------------------------
#' Standardize variables (mean 0, variance 1)
#'
#' @param X                   numeric: vector to be winsorized or trimmed
#' @param bounds              double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#' @param replacement         boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#'
#' @return  numeric vector of winsorized or trimmed values
#'
#' @export
standardize = function(X){return((X-mean(X, na.rm = T))/sd(X, na.rm = T))}

#' Winsorize or trim variables
#'
#' @param X                   numeric: vector to be winsorized or trimmed
#' @param bounds              double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#' @param replacement         boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#'
#' @return  numeric vector of winsorized or trimmed values
#'
#' @export
winsorize = function(X, bounds, replacement = TRUE){
  
    qq = quantile(X, probs = bounds, na.rm = TRUE)
  
    if(replacement == TRUE){
      X[X <= qq[2]] = qq[1]
      X[X >= qq[2]] = qq[2]
    }else{
      X[X <= qq[2]] = NA
      X[X >= qq[2]] = NA
    }
    
    return(X)
}


#' Create information set
#'
#' A function to subset data recurisvely or with a rolling window to create a valid information set. Is used as a data preparation 
#' helper function and is called internally by forecast_univariate, forecast_multivariate, and forecast_combine. 
#'
#' @param Data                  data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param forecast.date         date: upper bound of information set
#' @param rolling.window        int: size of rolling window, NA if expanding window is used
#' @param freq                  string: time series frequency; day, week, month, quarter, year; only needed for rolling window factors
#'
#' @return  data.frame bounded by the given date range
#'
#' @export
data_subset = function(
  Data,
  forecast.date,
  rolling.window,
  freq
){
  
  # 1. using expanding window
  if(is.na(rolling.window)){
    information.set =
      dplyr::filter(Data, date <= forecast.date) 
    
    # 2. using rolling window
  }else{
    rolling.window.start = forecast.date
    
    if(freq == 'day'){
      rolling.window.start = forecast.date - rolling.window
    }else if(freq == 'week'){
      lubridate::week(rolling.window.start) = lubridate::week(forecast.date) - rolling.window
    }else if(freq == 'month'){
      lubridate::month(rolling.window.start) = lubridate::month(forecast.date) - rolling.window
    }else if(freq == 'quarter'){
      lubridate::month(rolling.window.start) = lubridate::month(forecast.date) - rolling.window*3
    }else if(freq == 'year'){
      lubridate::year(rolling.window.start) = lubridate::year(forecast.date) - rolling.window
    }
    
    information.set =
      dplyr::filter(Data, rolling.window.start <= date & date <= forecast.date ) 
  }

  return(information.set)
}

#---------------------------------------------
# Clean outliers
#---------------------------------------------
#' Clean outliers
#'
#' A function to clean outliers. Is used as a data preparation helper function and is called internally
#'  by forecast_univariate, forecast_multivariate, and forecast_combine. 
#'
#' @param Data                  data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param variables             string: vector of variables to standardize, default is all but 'date' column
#' @param w.bounds              double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#' @param trim                  boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#' @param cross_section         boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export
data_outliers = function(
  Data,                           # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  variables,                      # string: vector of variables to standardize, default is all but 'date' column
  w.bounds = c(0.05, 0.95),       # double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
  trim = FALSE,                   # boolean: if TRUE then replace outliers with NA instead of winsorizing bound
  cross_section = FALSE,          # boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)
){

  # target variables must be numeric
  if(setdiff(variables, names(dplyr::select_if(Data, is.numeric))) != 0){
    print(errorCondition('Variables cleaned for outliers must be numeric.'))
  }
  
  # clean outliers (column wise)
  if(cross_section == FALSE){
    Data = Data %>%
      dplyr::mutate_at(vars(variables), winsorize, bounds = w.bounds, replacement = trim) %>%
      filter(date == cleaning.date)
    
  # clean outliers (row wise)
  }else{
    Data = Data %>%
      dplyr::rowwise() %>%
      dplyr::mutate_at(vars(variables), winsorize, bounds = w.bounds, replacement = trim) %>%
      filter(date == cleaning.date)
  }
  
  # return results
  return(Data)
}


#---------------------------------------------
# Impute missing
#---------------------------------------------
#' Instantiate impute.missing.routine
#'
#' A function to create the data imputation method
#' arguments list for user manipulation.
#'
#' @return instantiate.impute.missing.routine
#'
#' @export
instantiate.impute.missing.routine = function(){
  # methods
  methods = list(
    interpolation = imputeTS::na_interpolation,
    kalman = imputeTS::na_kalman, 
    locf = imputeTS::na_locf, 
    ma = imputeTS::na_ma,
    mean = imputeTS::na_mean,
    random = imputeTS::na_random,
    remove = imputeTS:na_remove,
    replace = imputeTS::na_replace,
    seadec = imputeTS::na_seadec,
    seasplit = imputeTS::na_seasplit
  )
  
  # arguments
  arguments = list(
    interpolation = NULL,
    kalman = NULL, 
    locf =  NULL,
    ma =  NULL,
    mean =  NULL,
    random =  NULL,
    remove =  NULL,
    replace =  NULL,
    seadec =  NULL,
    seasplit =  NULL
  )
  
  return(
    list(
      method = methods,
      arguments = arguments
    )
  )
  
}

#' Impute missing values
#'
#' A function to impute missing values. Is used as a data preparation helper function and is called internally
#'  by forecast_univariate, forecast_multivariate, and forecast_combine. 
#'
#' @param Data                  data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param method                string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
#' @param variables             string: vector of variables to standardize, default is all but 'date' column
#'
#' @return  data.frame with missing data imputed
#'
#' @export
data_standarize = function(
  Data,                           # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  method = 'kalman',              # string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
  variables = NA                  # string: vector of variables to impute missing values, default is all numeric columns
){

  # training parameter creation and warnings
  if(exists("impute.missing.routine")){
    print(warningCondition('impute.missing.routine exists and will be used to impute missing data in its present state.'))
  }else{
    impute.missing.routine = instantiate.impute.missing.routine()
    print(warningCondition('impute.missing.routine was instantiated and default values will be used for to impute missing data.'))
  }
  
  # set variables to all if default
  if(is.na(variables) == TRUE){
    variables = names(dplyr::select(Data, -date))
  }
  
  # target variables must be numeric
  if(setdiff(variables, names(dplyr::select_if(Data, is.numeric))) != 0){
    print(errorCondition('Variables must be numeric to impute'))
  }
  
  # clean outliers
  Data = Data %>%
    dplyr::mutate_at(vars(variables), 
                     instantiate.impute.missing.routine$method[[method]],
                     instantiate.impute.missing.routine$arguments[[method]]) %>%
    filter(date == cleaning.date)
  
  # return results
  return(Data)
}