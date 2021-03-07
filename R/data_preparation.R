#---------------------------------------------
# data cleaning helper functions
#---------------------------------------------
#' Standardize variables (mean 0, variance 1)
#'
#' @param X    numeric: vector to be standardized
#'
#' @return  numeric vector of standardized values
#'
#' @export
standardize = function(X){return((X-mean(X, na.rm = T))/sd(X, na.rm = T))}

#' Winsorize or trim variables
#'
#' @param X           numeric: vector to be winsorized or trimmed
#' @param trim        boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#' @param bounds      double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#'
#' @return  numeric vector of winsorized or trimmed values
#'
#' @export
winsorize = function(X, bounds, trim = FALSE){

    qq = quantile(X, probs = bounds, na.rm = TRUE)

    if(trim == FALSE){
      X[X <= qq[1]] = qq[1]
      X[X >= qq[2]] = qq[2]
    }else{
      X[X <= qq[1]] = NA
      X[X >= qq[2]] = NA
    }

    return(X)
}

#' Create information set
#'
#' A function to subset data recursively or with a rolling window to create a valid information set. Is used as a data preparation
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

#' Set forecasted date
#'
#' A function to subset data recurisvely or with a rolling window to create a valid information set. Is used as a data preparation
#' helper function and is called internally by forecast_univariate, forecast_multivariate, and forecast_combine.
#'
#' @param forecast.date         date: date forecast was made
#' @param horizon               int: periods ahead of forecast
#' @param freq                  string: time series frequency; day, week, month, quarter, year; only needed for rolling window factors
#'
#' @return  date vector
#'
#'
forecast_date = function(
  forecast.date,
  horizon,
  freq
){

  date = forecast.date

  if(freq == 'day'){
    date = forecast.date + horizon
  }else if(freq == 'week'){
    lubridate::week(date) = lubridate::week(date) + horizon
  }else if(freq == 'month'){
    lubridate::month(date) = lubridate::month(date) + horizon
  }else if(freq == 'quarter'){
    lubridate::month(date) = lubridate::month(date) + horizon*3
  }else if(freq == 'year'){
    lubridate::year(date) = lubridate::year(date) + horizon
  }

  return(date)
}

#' Create n lags
#'
#' A function to create 1 through n lags of a set of variables. Is used as a data preparation
#' helper function and is called internally by forecast_univariate, forecast_multivariate, and forecast_combine.
#'
#' @param Data        data.frame: data frame of variables to lag and a 'date' column
#' @param lags        int: number of lags to create
#' @param variables   string: vector of variable names to lag, default is all non-date variables
#'
#' @return  data.frame
#'
#' @export
n.lag = function(
  Data,                       # data.frame: data frame of variables to lag and a 'date' column
  lags,                       # int: number of lags to create
  variables = NULL            # string: vector of variable names to lag, default is all non-date variables
){

  if(is.null(variables)){
    variables = names(dplyr::select(Data, -date))
  }

  Data = c(0:lags) %>%
    purrr::map(
      .f = function(n){

        if(n == 0){return(Data)}

        X = Data %>%
          dplyr::mutate_at(variables, dplyr::lag, n)

        names(X)[names(X) != 'date'] = paste0(names(X)[names(X) != 'date'], '.l', n)

        return(X)
      }
    ) %>%
    purrr::reduce(dplyr::full_join, by = 'date')


  return(Data)
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
  variables = NULL,               # string: vector of variables to standardize, default is all but 'date' column
  w.bounds = c(0.05, 0.95),       # double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
  trim = FALSE,                   # boolean: if TRUE then replace outliers with NA instead of winsorizing bound
  cross_section = FALSE           # boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)
){


  # set variables to all if default
  if(is.null(variables) == TRUE){
    variables = names(dplyr::select_if(Data, is.numeric))
  }

  # target variables must be numeric
  if(length(setdiff(variables, names(dplyr::select_if(Data, is.numeric)))) != 0){
    print(errorCondition('Variables cleaned for outliers must be numeric.'))
  }

  # clean outliers (column wise)
  if(cross_section == FALSE){
    test = Data %>%
      dplyr::mutate_at(dplyr::vars(variables), winsorize, bounds = w.bounds, trim = trim)

  # clean outliers (row wise)
  }else{
    Data = Data %>%
      dplyr::rowwise() %>%
      dplyr::mutate_at(dplyr::vars(variables), winsorize, bounds = w.bounds, trim = trim)
  }

  # return results
  return(Data)
}


#---------------------------------------------
# Impute missing
#---------------------------------------------
#' Create interface to control `data_impute` model estimation
#'
#' A function to create the data imputation method
#' arguments list for user manipulation.
#'
#' @return data_impute.control_panel
#'
#' @export
instantiate.data_impute.control_panel = function(){

  # methods
  methods = list(
    interpolation = 'imputeTS::na_interpolation',
    kalman = imputeTS::na_kalman,
    locf = 'imputeTS::na_locf',
    ma = 'imputeTS::na_ma',
    mean = 'imputeTS::na_mean',
    random = 'imputeTS::na_random',
    remove = 'imputeTS:na_remove',
    replace = 'imputeTS::na_replace',
    seadec = 'imputeTS::na_seadec',
    seasplit = 'imputeTS::na_seasplit'
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
#' @param Data          data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param method        string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
#' @param variables     string: vector of variables to standardize, default is all but 'date' column
#' @param verbose       boolean: show start-up status of impute.missing.routine
#'
#' @return  data.frame with missing data imputed
#'
#' @export
data_impute = function(
  Data,                           # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  method = 'kalman',              # string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
  variables = NULL,               # string: vector of variables to impute missing values, default is all numeric columns
  verbose = FALSE                 # boolean: show start-up status of data_impute.control_panel
){

  # training parameter creation and warnings
  if(verbose == TRUE){
    if(exists("data_impute.control_panel")){
      print(warningCondition('data_impute.control_panel exists and will be used to impute missing data in its present state.'))
    }else{
      data_impute.control_panel = instantiate.data_impute.control_panel()
      print(warningCondition('data_impute.control_panel was instantiated and default values will be used for to impute missing data.'))
    }
  }else{
    if(!exists("data_impute.control_panel")){data_impute.control_panel = instantiate.data_impute.control_panel()}
  }

  # set variables to all if default
  if(is.null(variables) == TRUE){
    variables = names(dplyr::select_if(Data, is.numeric))
  }

  # target variables must be numeric
  if(length(setdiff(variables, names(dplyr::select_if(Data, is.numeric)))) != 0){
    print(errorCondition('Variables cleaned for outliers must be numeric.'))
  }

  # clean outliers
  for(v in variables){
    data_impute.control_panel$arguments[[method]]$x = Data[,c(v)]
    Data[,c(v)] =
      do.call(what = data_impute.control_panel$method[[method]],
              args = data_impute.control_panel$arguments[[method]])
  }

  # return results
  return(Data)
}


#---------------------------------------------
# Dimension reduction
#---------------------------------------------
#' Dimension reduction via principal components
#'
#' A function to estimate principal components.
#'
#' @param Data            data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param variables       string: vector of variables to standardize, default is all but 'date' column
#' @param ncomp           int: number of factors to create
#' @param standardize     boolean: normalize variables (mean zero, variance one) before estimating factors
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export

data_reduction = function(
  Data,                 # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  variables = NULL,     # string: vector of variables to impute missing values, default is all numeric columns
  ncomp,                # int: number of factors to create
  standardize = TRUE    # boolean: normalize variables (mean zero, variance one) before estimating factors
){

  # set variables to all if default
  if(is.null(variables) == TRUE){
    variables = names(dplyr::select_if(Data, is.numeric))
  }

  # target variables must be numeric
  if(length(setdiff(variables, names(dplyr::select_if(Data, is.numeric)))) != 0){
    print(errorCondition('Variables cleaned for outliers must be numeric.'))
  }

  # remove missing
  information.set = na.omit(Data)

  # standardize variables
  information.set = information.set %>%
    dplyr::mutate_at(dplyr::vars(variables), OOS::standardize)

  # estimate factors
  model.pc = stats::princomp(dplyr::select(information.set, -date))

  # select factors
  factors = as.matrix(dplyr::select(information.set, -date)) %*% model.pc$loadings[,1:ncomp]

  # take most recent factors
  colnames(factors) = paste0('pc.',c(1:ncomp))

  factors =
    data.frame(factors,
               date = information.set$date)

  # return results
  return(factors)
}
