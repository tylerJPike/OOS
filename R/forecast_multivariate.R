
# dependencies:
# dplyr
# zoo
# glmnet
# caret
# vars

#----------------------------------------------
# multivariate forecasting arguments - ML
#----------------------------------------------
#' instantiate.multivariate.forecast.ml.training
#'
#' A function to create the multivariate forecast methods
#' arguments list for user manipulation.
#'
#' @return multivariate.forecast.ml.training
#' @export

instantiate.multivariate.forecast.ml.training = function(){

  # caret names
  caret.engine = list(
    ols = 'lm',
    ridge = 'glmnet',
    lasso = 'glmnet',
    elastic = 'glmnet',
    RF = 'rf',
    GBM = 'gbm',
    NN = 'avNNet'
  )

  # tuning grids
  tuning.grids = list(

    ols = NULL,

    ridge = expand.grid(
      alpha = 0,
      lambda = 10^seq(-3, 3, length = 100)),

    lasso = expand.grid(
      alpha = 1,
      lambda = 10^seq(-3, 3, length = 100)),

    elastic = NULL,

    GBM =
      expand.grid(
        n.minobsinnode = c(1),
        shrinkage = c(.1,.01),
        n.trees = c(200,400,600),
        interaction.depth = c(1,2)),

    RF =
      expand.grid(
        mtry = c(1:4)),

    NN =
      expand.grid(
        size = seq(2,10,1),
        decay = c(.01,.001),
        bag = c(100, 250, 500))
  )

  # hyperparameter selection routine
  control =
    caret::trainControl(
      method = "cv",
      number = 5,
      allowParallel = TRUE,
      savePredictions = TRUE)

  # accuracy metric used in training
  accuracy = 'RMSE'

  # return training information
  return(
    list(
      caret.engine = caret.engine,
      tuning.grids = tuning.grids,
      control = control,
      accuracy = accuracy
    )
  )

}

#----------------------------------------------
# multivariate forecasting arguments - VAR
#----------------------------------------------
#' instantiate.multivariate.forecast.var.training
#'
#' A function to create the multivariate forecast methods
#' arguments list for user manipulation.
#'
#' @return multivariate.forecast.var.training
#'
#' @export

instantiate.multivariate.forecast.var.training = function(){

  return(
    list(
      p = NULL,
      lag.max = 12,
      ic = 'AIC',
      type = 'none',
      season = NULL,
      exogen = NULL
    )
  )

}

#---------------------------------------------
# Multivariate Forecast
#---------------------------------------------
#' forecast_multivariate
#'
#' A function to estimate multivariate forecasts out-of-sample. Methods available include:
#' vector auto-regression, linear regression, lasso regression, ridge regression, elastic net,
#' random forest, tree-based gradient boosting machine, and single-layer neural network.
#' The function will take in a data frame of the target variable, exogenous variables, and a 'date' column,
#' while outputting a data frame with a date column and one column per forecast method selected.
#'
#' @param Data             data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param forecast.dates   date: dates forecasts are created
#' @param target           string: column name in `Data` of variable to forecast
#' @param method           string or vector: methods to use; 'var', 'ols', 'ridge', 'lasso', 'elastic', 'RF', 'GBM', 'NN'
#' @param rolling.window   int: size of rolling window, NA if expanding window is used
#' @param freq             string: time series frequency; day, week, month, quarter, year
#' @param horizon          int: number of periods into the future to forecast
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @eport

forecast_multivariate = function(
  Data,                 # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  forecast.dates,       # date: dates forecasts are created
  target,               # string: column name in `Data` of variable to forecast
  method,               # string or vector: methods to use; 'var', 'ols', 'ridge', 'lasso', 'elastic', 'RF', 'GBM', 'NN'
  rolling.window = NA,  # int: size of rolling window, NA if expanding window is used
  freq,                 # string: time series frequency; day, week, month, quarter, year
  horizon               # int: number of periods into the future to forecast
){

  # results list
  results.list = list()

  # training parameter creation and warnings
  if(exists("multivariate.forecast.ml.training")){
    print(warningCondition('forecast.combinations.ml.training exists and will be used for ML forecast combination techniques in its present state.'))
  }else{
    multivariate.forecast.ml.training = instantiate.multivariate.forecast.ml.training()
    print(warningCondition('multivariate.forecast.ml.training was instantiated and default values will be used to train ML forecast combination techniques.'))
  }

  # VAR parameters and warnings
  if(exists("multivariate.forecast.var.training")){
    print(warningCondition('forecast.combinations.var.training exists and will be used for ML forecast combination techniques in its present state.'))
  }else{
    multivariate.forecast.var.training = instantiate.multivariate.forecast.var.training()
    print(warningCondition('multivariate.forecast.var.training was instantiated and default values will be used to train ML forecast combination techniques.'))
  }

  # Create forecasts
  forecasts = intersect(method, c('ols','ridge','lasso','elastic','GBM','RF','NN')) %>%
    purrr::map(
      .f = function(engine){

        forecast.dates%>%
          purrr::map(
            .f = function(forecast.date){

              # subset data
              # 1. using expanding window
              if(is.na(rolling.window)){
                information.set =
                  dplyr::filter(Data, date <= forecast.date) %>%
                  dplyr::select(-date)

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
                  dplyr::filter(Data, rolling.window.start <= date & date <= forecast.date ) %>%
                  dplyr::select(-date)
              }

              # set current data
              current.set = dplyr::filter(Data, forecast.date == date)

              # set target variable
              names(information.set)[names(information.set) == target] = 'target'

              # set horizon
              information.set = dplyr::mutate(information.set, target = dplyr::lead(target, horizon))

              # estimate ML model
              if(engine != 'var'){
                model =
                  caret::train(target~.,
                               data = information.set,
                               method    = multivariate.forecast.ml.training$caret.engine[[engine]],
                               trControl = multivariate.forecast.ml.training$control,
                               tuneGrid  = multivariate.forecast.ml.training$tuning.grids[[engine]],
                               metric    = multivariate.forecast.ml.training$accuracy,
                               na.action = na.omit)

                # calculate forecast
                ml = predict(model, newdata = current.set)
                results = data.frame(date = current.set$date, ml)
                colnames(results)[colnames(results) == 'ml'] = engine

              # estimate VAR
              }else{
                model =
                  vars::VAR(
                    y       = Data,
                    p       =  multivariate.forecast.var.training$p,
                    max.lag =  multivariate.forecast.var.training$max.lag,
                    ic      =  multivariate.forecast.var.training$ic,
                    season  =  multivariate.forecast.var.training$season,
                    exogen  =  multivariate.forecast.var.training$exogen
                  )

                # calculate forecast
                ml = predict(model, newdata = current.set)
                results = data.frame(date = current.set$date, ml)
                colnames(results)[colnames(results) == 'ml'] = engine
              }

              # return results
              return(results)

            }
          ) %>%
          purrr::reduce(dplyr::bind_rows)
      }
    ) %>%
    purrr::reduce(dplyr::full_join, by = 'date')

  # return results
  return(forecasts)
}
