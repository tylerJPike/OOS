
# dependencies:
# magrittr
# lubridate
# dplry
# purrr
# forecast


#----------------------------------------------
# univariate forecasting arguments
#----------------------------------------------
#' Create interface to control `forecast_univariate` model estimation
#'
#' A function to create the univariate forecast method arguments list
#' for user manipulation.
#'
#' @return forecast_univariate.control_panel
#'
#' @export

instantiate.forecast_univariate.control_panel = function(){

  # methods
  methods = list(
    auto.arima = forecast::auto.arima,
    Arima = forecast::Arima,
    dshw = forecast::dshw,
    holt = forecast::holt,
    hw = forecast::hw,
    ses = forecast::ses,
    ets = forecast::ets,
    stlm = forecast::stlm,
    bats = forecast::bats,
    tbats = forecast::tbats,
    thetaf = forecast::thetaf,
    nnetar = forecast::nnetar,
    meanf = forecast::meanf,
    naive = forecast::naive,
    snaive = forecast::snaive,
    rwf = forecast::rwf,
    tslm = forecast::tslm,
    splinef = forecast::splinef
  )

  # arguments
  arguments = list(
   auto.arima = NULL,
   Arima   = NULL,
   dshw    = NULL,
   holt    = NULL,
   hw      = NULL,
   ses     = NULL,
   ets     = NULL,
   stlm    = NULL,
   bats    = NULL,
   tbats   = NULL,
   thetaf  = NULL,
   nnetar  = NULL,
   meanf   = NULL,
   naive   = NULL,
   snaive  = NULL,
   rwf     = NULL,
   splinef = NULL,
   tslm    = NULL
  )

  return(
    list(
      method = methods,
      arguments = arguments
    )
  )

}

#----------------------------------------------
# univariate time series forecasting function
#----------------------------------------------
#' Forecast with univariate models
#'
#' A function to estimate univariate forecasts out-of-sample. Methods available include all forecast
#' methods from the `forecast` package. See package website for most up-to-date list of available models.
#'
#' @param Data            data.frame: data frame of variable to forecast and a date column; may alternatively be a `ts`, `xts`, or `zoo` object to forecast
#' @param forecast.dates  date: dates forecasts are created
#' @param methods         string: models to estimate forecasts
#' @param horizon         int: number of periods to forecast
#' @param rolling.window  int: size of rolling window, NA if expanding window is used
#' @param freq            string: time series frequency; day, week, month, quarter, year
#' @param recursive       boolean: use sequential one-step-ahead forecast if TRUE, use direct projections if FALSE
#' @param outlier.clean         boolean: if TRUE then clean outliers
#' @param outlier.variables     string: vector of variables to purge of outliers, default is all but 'date' column
#' @param outlier.bounds        double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
#' @param outlier.trim          boolean: if TRUE then replace outliers with NA instead of winsorizing bound
#' @param outlier.cross_section boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)
#' @param impute.missing        boolean: if TRUE then impute missing values
#' @param impute.method         string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
#' @param impute.variables      string: vector of variables to impute missing values, default is all numeric columns
#' @param impute.verbose        boolean: show start-up status of impute.missing.routine
#' @param parallel.dates        int: the number of cores available for parallel estimation
#' @param return.models         boolean: if TRUE then return list of models estimated each forecast.date
#' @param return.data           boolean: if True then return list of information.set for each forecast.date
#'
#' @return  data.frame with a row for each forecast by model and forecasted date
#'
#' @examples
#' \donttest{
#'  # simple time series
#'  A = c(1:100) + rnorm(100)
#'  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
#'  Data = data.frame(date = date, A)
#'
#'  # estiamte univariate forecasts
#'  forecast.uni =
#'      forecast_univariate(
#'        Data = Data,
#'        forecast.dates = tail(Data$date,5),
#'        method = c('naive','auto.arima', 'ets'),
#'        horizon = 1,
#'        recursive = FALSE,
#'        # information set
#'        rolling.window = NA,
#'        freq = 'month',
#'        # data prep
#'        outlier.clean = TRUE,
#'        impute.missing = TRUE)
#' }
#'
#' @export

forecast_univariate = function(
  Data,                   # data.frame: data frame of variable to forecast and a date column; may alternatively be a `ts`, `xts`, or `zoo` object to forecast
  forecast.dates,         # date: dates forecasts are created
  methods,                # string or vector: models to estimate forecasts with; currently supports all and only functions from the `forecast` package
  horizon,                # int: number of periods to forecast
  recursive = TRUE,       # boolean: use sequential one-step-ahead forecast if TRUE, use direct projections if FALSE

  # information set
  rolling.window = NA,  # int: size of rolling window, NA if expanding window is used
  freq,                 # string: time series frequency; day, week, month, quarter, year

  # outlier cleaning
  outlier.clean = FALSE,           # boolean: if TRUE then clean outliers
  outlier.variables = NULL,               # string: vector of variables to standardize, default is all but 'date' column
  outlier.bounds = c(0.05, 0.95),  # double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)
  outlier.trim = FALSE,            # boolean: if TRUE then replace outliers with NA instead of winsorizing bound
  outlier.cross_section = FALSE,   # boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)

  # impute missing
  impute.missing = FALSE,          # boolean: if TRUE then impute missing values
  impute.method = 'kalman',        # string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'
  impute.variables = NULL,         # string: vector of variables to impute missing values, default is all numeric columns
  impute.verbose = FALSE,          # boolean: show start-up status of impute.missing.routine

  # parallel processing
  parallel.dates = NULL,           # int: the number of cores available for parallel estimation

  # additional objects
  return.models = FALSE,           # boolean: if TRUE then return list of models estimated each forecast.date
  return.data = FALSE              # boolean: if True then return list of information.set for each forecast.date

){

  # convert from ts, xts, or zoo object
  if(xts::is.xts(Data) | zoo::is.zoo(Data) | stats::is.ts(Data)){
    Data = data.frame(date = zoo::index(Data), Data)
  }

  # training parameter creation and warnings
  if(exists("forecast_univariate.control_panel")){
    message('forecast_univariate.control_panel exists and will be used for model estimation in its present state.')
  }else{
    forecast_univariate.control_panel = instantiate.forecast_univariate.control_panel()
    message('forecast_univariate.control_panel was instantiated and default values will be used for model estimation.')
  }

  # create parallel back end
  if(!is.null(parallel.dates)){
    future::plan(strategy = 'multisession', workers = parallel.dates)
  }else{
    future::plan(strategy = 'sequential')
  }

  # create lists to store information
  list.models = list(); i = 1
  list.data = list(); j = 1

  # forecast routine
  forecasts = forecast.dates %>%
    furrr::future_map(
      .f = function(forecast.date){

        #---------------------------
        # Create information set
        #---------------------------

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

        # set ts object
        information.set = information.set %>%
          dplyr::select(-date) %>%
          as.ts()

        #---------------------------
        # Create forecasts
        #---------------------------

        results =
            methods %>% purrr::map(
              .f = function(engine){

                # make predictions
                # 1. using direct projections
                if(recursive == FALSE){

                  # set data
                  forecast_univariate.control_panel$arguments[[engine]]$y = information.set

                  # estimate model
                  model =  do.call(what = forecast_univariate.control_panel$method[[engine]],
                                   args = forecast_univariate.control_panel$arguments[[engine]])

                  # create forecasts
                  predictions = forecast::forecast(model, h = horizon)

                  # create standard errors
                  calc.error = try(predictions$lower[1])

                  if(is.numeric(calc.error) == TRUE){
                    error = (predictions$upper[,1] - predictions$lower[,1]) /
                      (2 * qnorm(.5 + predictions$level[1] / 200))
                    error = as.numeric(error)
                  }else{
                    se = NA
                  }

                  predictions = data.frame(model = engine, forecast = predictions$mean, se = error)

                # 2. using recursive forecasts
                }else{

                  predictions = list()
                  forecast_univariate.control_panel$arguments[[engine]]$y = information.set

                  for(i in 1:horizon){

                    # estimate model
                    model =  do.call(what = forecast_univariate.control_panel$method[[engine]],
                                     args = forecast_univariate.control_panel$arguments[[engine]])

                    # create forecast
                    prediction = forecast::forecast(model, h = 1)

                    # create standard errors
                    calc.error = try(prediction$lower[1])

                    if(is.numeric(calc.error) == TRUE){
                      error = (prediction$upper[,1] - prediction$lower[,1]) /
                        (2 * qnorm(.5 + prediction$level[1] / 200))
                      error = as.numeric(error)
                    }else{
                      error = NA
                    }

                    predictions[[i]] = data.frame(model = engine, forecast = prediction$mean, se = error)

                    # update information set
                    information.set = rbind(information.set, prediction$mean[1]) %>% as.ts()
                    forecast_univariate.control_panel$arguments[[engine]]$y = information.set

                  }

                  # collapse results
                  predictions = purrr::reduce(predictions, dplyr::bind_rows) %>% data.frame()

                }

                # add forecast dates
                predictions$forecast.date = forecast.date
                predictions$date = seq.Date(from = forecast.date, by = freq, length.out = horizon+1)[2:(horizon+1)]

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
