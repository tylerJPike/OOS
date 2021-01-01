
# dependencies:
# magrittr
# lubridate
# dplry
# purrr
# forecast


#----------------------------------------------
# univariate forecasting arguments
#----------------------------------------------
#' instantiate.univariate.forecast.training
#'
#' A function to create the univariate forecast method arguments list
#' for user manipulation.
#'
#' @return univariate.forecast.training
#'
#' @export

instantiate.univariate.forecast.training = function(){

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
#' forecast_univariate
#'
#' A function to estimate univariate forecasts out-of-sample. Methods available include all forecast
#' methods from the `forecast` package. The function will take in a data frame of the target variable,
#' and a observation date column (named 'date'), while outputting a data frame with a date column and
#' one column per forecast method selected.
#'
#' @param Data            data.frame: data frame of variable to forecast and a date column
#' @param forecast.dates  date: dates forecasts are created
#' @param methods         string or vector: models to estimate forecasts with; currently supports all and only functions from the `forecast` package
#' @param periods         int: number of periods to forecast
#' @param rolling.window  int: size of rolling window, NA if expanding window is used
#' @param freq            string: time series frequency; day, week, month, quarter, year
#' @param recursive       boolean: use sequential one-step-ahead forecast if TRUE, use direct projections if FALSE
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export

forecast_univariate = function(
  Data,                   # data.frame: data frame of variable to forecast and a date column
  forecast.dates,         # date: dates forecasts are created
  methods,                # string or vector: models to estimate forecasts with; currently supports all and only functions from the `forecast` package
  periods,                # int: number of periods to forecast
  rolling.window = NA,    # int: size of rolling window, NA if expanding window is used
  freq,                   # string: time series frequency; day, week, month, quarter, year
  recursive = TRUE        # boolean: use sequential one-step-ahead forecast if TRUE, use direct projections if FALSE
){

  # training parameter creation and warnings
  if(exists("univariate.forecast.training")){
    print(warningCondition('univariate.forecast.training exists and will be used for model estimation in its present state.'))
  }else{
    univariate.forecast.training = instantiate.univariate.forecast.training()
    print(warningCondition('univariate.forecast.training was instantiated and default values will be used to model estimation.'))
  }

  # forecast routine
  forecasts = forecast.dates %>%
    purrr::map(
      .f = function(forecast.date){

        #---------------------------
        # Create information set
        #---------------------------

        # subset data
        # 1. using expanding window
        if(is.na(rolling.window)){
          information.set =
            dplyr::filter(Data, date <= forecast.date) %>%
            dplyr::select(-date) %>% as.ts()

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
            dplyr::select(-date) %>% as.ts()
        }

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
                univariate.forecast.training$arguments[[engine]]$y = information.set

                # estimate model
                model =  do.call(what = univariate.forecast.training$method[[engine]],
                                 args = univariate.forecast.training$arguments[[engine]])

                # create forecasts
                predictions = forecast::forecast(model, h = periods)

                # set results
                if(is.null(predictions$lower)){predictions$lower = predictions$upper = t(c(NA,NA))} # nnetar does not create se by default
                predictions = data.frame(predictions$mean, predictions$lower, predictions$upper)
                names(predictions) = c( 'forecast','lower.80','lower.95', 'upper.80', 'upper.95')

              # 2. using recursive forecasts
              }else{

                predictions = list()
                univariate.forecast.training$arguments[[engine]]$y = information.set

                for(i in 1:periods){

                  # estimate model
                  model =  do.call(what = univariate.forecast.training$method[[engine]],
                                   args = univariate.forecast.training$arguments[[engine]])

                  # create forecast
                  prediction = forecast::forecast(model, h = 1)
                  if(is.null(prediction$lower)){prediction$lower = prediction$upper = t(c(NA,NA))} # nnetar does not create se by default
                  predictions[[i]] = data.frame(forecast = prediction$mean, prediction$lower, prediction$upper)

                  # update information set
                  information.set = rbind(information.set, prediction$mean[1]) %>% as.ts()
                  univariate.forecast.training$arguments[[engine]]$y = information.set

                }

                # collapse results
                predictions = purrr::reduce(predictions, dplyr::bind_rows) %>% data.frame()
                names(predictions) = c( 'forecast','lower.80','lower.95', 'upper.80', 'upper.95')

              }

              # add forecast dates
              predictions$forecast.date = forecast.date
              predictions$date = seq.Date(from = forecast.date, by = freq, length.out = periods+1)[2:(periods+1)]

              # return results
              return(predictions)
            }
          )

        #---------------------------
        # Estimate forecasting model
        #---------------------------
        return(results)

      }
    )

  # set list item names
  names(forecasts) = forecast.dates

  return(forecasts)
}
