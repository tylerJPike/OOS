
# dependencies:
# dplyr
# zoo
# glmnet
# caret
# vars

#----------------------------------------------
# multivariate forecasting arguments - ML
#----------------------------------------------
#' Instantiate multivariate.forecast.ml.training
#'
#' A function to create the multivariate forecast methods
#' arguments list for user manipulation.
#'
#' @return multivariate.forecast.ml.training
#'
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
    NN = 'avNNet',
    pls = 'pls',
    pcr = 'pcr'
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
        bag = c(100, 250, 500)),

    pls =
      expand.grid(
        ncomp = c(1:5)),

    pcr =
      expand.grid(
        ncomp = c(1:5))

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
#' Instantiate multivariate.forecast.var.training
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
      p = 1,
      lag.max = NULL,
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
#' Forecast with multivariate models
#'
#' A function to estimate multivariate forecasts out-of-sample. Methods available include:
#' vector auto-regression, linear regression, lasso regression, ridge regression, elastic net,
#' random forest, tree-based gradient boosting machine, and single-layer neural network.
#' The function will take in a data frame of the target variable, exogenous variables, and a 'date' column,
#' while outputting a data frame with a date column and one column per forecast method selected.
#'
#' @param Data             data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param forecast.dates   date: dates forecasts are created
#' @param target           string: column name in Data of variable to forecast
#' @param method           string or vector: methods to use; 'var', 'ols', 'ridge', 'lasso', 'elastic', 'RF', 'GBM', 'NN'
#' @param rolling.window   int: size of rolling window, NA if expanding window is used
#' @param freq             string: time series frequency; day, week, month, quarter, year
#' @param horizon          int: number of periods into the future to forecast
#' @param lag.variables    string: vector of variables to lag each time step, if lag.n is not null then the default is all non-date variables
#' @param lag.n            int: number of lags to create
#' @param outlier.clean         boolean: if TRUE then clean outliers
#' @param outlier.variables     string: vector of variables to standardize, default is all but 'date' column
#' @param outlier.bounds        double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#' @param outlier.trim          boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#' @param outlier.cross_section boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)
#' @param impute.missing        boolean: if TRUE then impute missing values
#' @param impute.method         string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
#' @param impute.variables      string: vector of variables to impute missing values, default is all numeric columns
#' @param impute.verbose        boolean: show start-up status of impute.missing.routine
#' @param reduce.data           boolean: if TRUE then reduce dimension
#' @param reduce.variables      string: vector of variables to impute missing values, default is all numeric columns
#' @param reduce.ncomp          int: number of factors to create
#' @param reduce.standardize    boolean: normalize variables (mean zero, variance one) before estimating factors
#' @param parallel.dates        int: the number of cores available for parallel estimation
#' @param return.models         boolean: if TRUE then return list of models estimated each forecast.date
#' @param return.data           boolean: if True then return list of information.set for each forecast.date
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export

forecast_multivariate = function(
  Data,                 # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  forecast.dates,       # date: dates forecasts are created
  target,               # string: column name in `Data` of variable to forecast
  horizon,              # int: number of periods into the future to forecast
  method,               # string or vector: methods to use; 'var', 'ols', 'ridge', 'lasso', 'elastic', 'RF', 'GBM', 'NN'

  # information set
  rolling.window = NA,  # int: size of rolling window, NA if expanding window is used
  freq,                 # string: time series frequency; day, week, month, quarter, year
  lag.variables = NULL, # string: vector of variables to lag each time step, if lag.n is not null then the default is all non-date variables
  lag.n = NULL,         # int: number of lags to create

  # outlier cleaning
  outlier.clean = FALSE,           # boolean: if TRUE then clean outliers
  outlier.variables = NULL,        # string: vector of variables to standardize, default is all but 'date' column
  outlier.bounds = c(0.05, 0.95),  # double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
  outlier.trim = FALSE,            # boolean: if TRUE then replace outliers with NA instead of winsorizing bound
  outlier.cross_section = FALSE,   # boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)

  # impute missing
  impute.missing = FALSE,          # boolean: if TRUE then impute missing values
  impute.method = 'kalman',        # string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
  impute.variables = NULL,         # string: vector of variables to impute missing values, default is all numeric columns
  impute.verbose = FALSE,          # boolean: show start-up status of impute.missing.routine

  # dimension reduction
  reduce.data = FALSE,             # boolean: if TRUE then reduce dimension
  reduce.variables = NULL,         # string: vector of variables to impute missing values, default is all numeric columns
  reduce.ncomp = NULL,             # int: number of factors to create
  reduce.standardize = TRUE,       # boolean: normalize variables (mean zero, variance one) before estimating factors

  # parallel processing
  parallel.dates = NULL,           # int: the number of cores available for parallel estimation

  # additional objects
  return.models = FALSE,           # boolean: if TRUE then return list of models estimated each forecast.date
  return.data = FALSE              # boolean: if True then return list of information.set for each forecast.date

){

  # results list
  results.list = list()

  # training parameter creation and warnings
  if(exists("multivariate.forecast.ml.training")){
    print(warningCondition('forecast.combinations.ml.training exists and will be used for ML model estimation in its present state.'))
  }else{
    multivariate.forecast.ml.training = instantiate.multivariate.forecast.ml.training()
    print(warningCondition('multivariate.forecast.ml.training was instantiated and default values will be used for ML model estimation.'))
  }

  # VAR parameters and warnings
  if(exists("multivariate.forecast.var.training")){
    print(warningCondition('forecast.combinations.var.training exists and will be used for VAR model estimation in its present state.'))
  }else{
    multivariate.forecast.var.training = instantiate.multivariate.forecast.var.training()
    print(warningCondition('multivariate.forecast.var.training was instantiated and default values will be used for VAR model estimation.'))
  }

  # create parallel back end
  if(!is.null(parallel.dates)){
    future::plan(strategy = 'multisession', workers = parallel.dates)
  }else{
    future::plan(strategy = 'sequential')
  }


  # Create forecasts
  forecasts = forecast.dates %>%
    furrr::future_map(
      .f = function(forecast.date){

          # subset data
          information.set =
            data_subset(
              Data = Data,
              forecast.date = forecast.date,
              rolling.window = rolling.window,
              freq = freq
            )

          # clean outliers
          if(outlier.clean){
            information.set =
              data_outliers(
                Data = information.set,
                variables = outlier.variables,
                w.bounds = outlier.bounds,
                trim = outlier.trim,
                cross_section = outlier.cross_section
              )
          }

          # impute missing values
          if(impute.missing){
            information.set =
              data_impute(
                Data = information.set,
                variables = impute.variables,
                method = impute.method,
                verbose = impute.verbose
              )
          }

          # dimension reduction
          if(reduce.data){
            information.set.reduce =
              data_reduction(
                Data = information.set,
                variables = reduce.variables,
                ncomp = reduce.ncomp,
                standardize = reduce.standardize
              )

            information.set =
              dplyr::full_join(
                dplyr::select(information.set, target, date),
                information.set.reduce,
                by = 'date')
          }

          # create variable lags
          if(!is.null(lag.n)){
            information.set =
              n.lag(
                Data = information.set,
                lags = lag.n,
                variables = lag.variables)
          }

          results = method %>%
            purrr::map(
              .f = function(engine){

              # set current data
              current.set = dplyr::filter(information.set, forecast.date == date)

              # estimate ML model
              if(engine != 'var'){

                # check for missing covariates in current data
                if(is.na(sum(dplyr::select(current.set, -date)))){
                  print(warningCondition(paste0('Missing covariate on: ', forecast.date)))
                  results = data.frame(date = current.set$date, ml = NA)
                  colnames(results)[colnames(results) == 'ml'] = engine
                  return(results)
                }

                # set target variable
                names(information.set)[names(information.set) == target] = 'target'

                # set horizon
                information.set =
                  dplyr::mutate(information.set, target = dplyr::lead(target, horizon)) %>%
                  na.omit()

                # estimate model
                model =
                  caret::train(target~.,
                               data = dplyr::select(information.set, -date),
                               method    = multivariate.forecast.ml.training$caret.engine[[engine]],
                               trControl = multivariate.forecast.ml.training$control,
                               tuneGrid  = multivariate.forecast.ml.training$tuning.grids[[engine]],
                               metric    = multivariate.forecast.ml.training$accuracy,
                               na.action = na.omit)

                # calculate forecast
                point = try(predict(model, newdata = current.set))

                if(!is.numeric(point)){
                  point = NA
                }

                # calculate standard error
                error =
                  try(
                    predict(model$finalModel, current.set, interval = "confidence", level = 0.95) %>%
                      data.frame(),
                    silent = TRUE
                    )

                error = try((error$upr - error$fit) / qnorm(0.95),
                            silent = TRUE)

                if(is.numeric(error) != TRUE | length(error) != 1){error = NA}

              # estimate VAR
              }else{

                model =
                  vars::VAR(
                    y       = na.omit(dplyr::select(information.set, -date)),
                    p       =  multivariate.forecast.var.training$p,
                    lag.max =  multivariate.forecast.var.training$max.lag,
                    ic      =  multivariate.forecast.var.training$ic,
                    season  =  multivariate.forecast.var.training$season,
                    type    =  multivariate.forecast.var.training$type
                  )

                # calculate forecast and standard error
                ml = predict(model, n.ahead = horizon)
                ml = ml$fcst[target] %>% data.frame()
                point = ml[horizon, 1]
                error = (ml[horizon, 3] - ml[horizon, 1]) / qnorm(0.95)

              }

              # set date
              date = forecast_date(
                forecast.date,
                horizon,
                freq)

              # set dates
              predictions = data.frame(
                date = date,
                forecast.date = forecast.date,
                model = engine, forecast = point, se = error)


              # return results
              return(
                list(
                  predictions = predictions,
                  model = model
                )
              )

            }
          )

        predictions =
          purrr::map(results, .f = function(X){return(X$predictions)}) %>%
          purrr::reduce(dplyr::bind_rows)

        rownames(predictions) = c(1:nrow(predictions))

        models =
          purrr::map(results, .f = function(X){return(X$model)})

        # store objects for return
        results =
          list(
            predictions = predictions,
            information.set = information.set,
            models = models
          )

        # return results
        return(results)

      }
    )

  # prepare forecasts
  predictions =
    purrr::map(forecasts, .f = function(X){return(X$predictions)}) %>%
    purrr::reduce(dplyr::bind_rows)

  # add model and information set lists to return object
  if(return.data == TRUE | return.models == TRUE){
    information = list(forecasts = predictions)
  }else{
    information = predictions
  }

  # prepare models
  if(return.models == TRUE){
    models = purrr::map(forecasts, .f = function(X){return(X$models)})
    names(models) = forecast.dates
    information[['models']] = models
  }

  # prepare information set
  if(return.data == TRUE){
    information.set = purrr::map(forecasts, .f = function(X){return(X$information.set)})
    names(information.set) = forecast.dates
    information[['information.set']] = information.set
  }

  # return results
  return(information)
}
