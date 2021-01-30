
# dependencies:
# dplyr
# zoo
# glmnet
# caret

#---------------------------------------------
# Forecast combination helper functions
#---------------------------------------------
#' Select N-best forecasts
#'
#' A function to subset the n-best forecasts;
#' assumes column named observed.
#'
#' @param forecasts data.frame: a data frame of forecasts to combine, assumes one column named "observed"
#' @param n.max     int: maximum number of forecasts to select
#' @param window    int: size of rolling window to evaluate forecast error over, use entire period if NA
#'
#' @return data.frame with n columns of the historically best forecasts
#'
#' @export

NBest = function(
  forecasts,    # data.frame: a data frame of forecasts to combine, assumes one column named "observed"
  n.max,        # int: maximum number of forecasts to select
  window = NA   # int: size of rolling window to evaluate forecast error over, use entire period if NA
){

  # calculate rolling forecast errors
  errors = abs(dplyr::select(forecasts, -observed) - forecasts$observed)
  rollRMSE = function(X){return(sqrt(mean((X)^2, na.rm = T)))}
  rollingErrors = zoo::rollapply(data = errors, width = seq_along(errors[,1]),
                                 FUN = rollRMSE, align = 'right', fill = NA)

  # create rolling N-best forecasts
  X = dplyr::select(forecasts, -observed) %>% as.matrix()
  nBest = matrix(nrow = nrow(X), ncol = n.max)
  for(row in 1:nrow(X)){
    for(column in 1:n.max){
      nBest[row,column] = mean(X[row,order(rollingErrors[row,])[1:column]])
    }
  }
  colnames(nBest) = paste0('N',c(1:n.max))

  # return results
  return(nBest)
}

#---------------------------------------------
# Forecast combination method arguments
#----------------------------------------------
#' Instantiate forecast.combinations.ml.training
#'
#' A function to create the forecast combination technique arguments list
#' for user manipulation.
#'
#' @return forecast.combinations.ml.training
#'
#' @export

instantiate.forecast.combinations.ml.training = function(){

  # caret names
  caret.engine = list(
    RF  = 'rf',
    GBM = 'gbm',
    NN  = 'avNNet',
    ols = 'lm',
    lasso   = 'glmnet',
    ridge   = 'glmnet',
    elastic = 'glmnet'
  )

  # tuning grids
  tuning.grids = list(
    GBM =
      expand.grid(n.minobsinnode = c(1),
                  shrinkage = c(.1,.01),
                  n.trees = c(200,400,600),
                  interaction.depth = c(1,2)),

    RF =
      expand.grid(mtry = c(1:4)),

    NN =
      expand.grid(size = seq(2,10,1),
                  decay = c(.01,.001),
                  bag = c(100, 250, 500)),

    ols = NULL,

    ridge = expand.grid(
      alpha = 0,
      lambda = 10^seq(-3, 3, length = 100)),

    lasso = expand.grid(
      alpha = 1,
      lambda = 10^seq(-3, 3, length = 100)),

    elastic = NULL
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

#---------------------------------------------
# Forecast combination methods
#---------------------------------------------
#' Combine forecasts
#'
#' A function to combine forecasts out-of-sample. Methods available include:
#' uniform weights, median forecast, trimmed (winsorized) mean, n-best,
#' ridge regression, lasso regression, elastic net, peLASSO,
#' random forest, tree-based gradient boosting machine, and single-layer neural network.
#' The function will take in a data frame of forecasts, observed values (named 'observed', when the method requires it),
#' and the observation date (i.e. the foretasted date, named 'date'), while outputting a data frame
#' with a date column and one column per combination method selected.
#'
#' @param forecasts       data.frame: data frame of forecasted values to combine, assumes 'date' and 'observed' columns, but `observed' is not necessary for all methods
#' @param method          string or vector: the method to use; 'uniform', 'median', 'trimmed.mean', 'n.best', 'peLasso', 'lasso', 'ridge', 'elastic', 'RF', 'GBM', 'NN'
#' @param n.max           int: maximum number of forecasts to select
#' @param window          int: size of rolling window to evaluate forecast error over, use entire period if NA
#' @param trim            numeric: a two element vector with the winsorizing bounds for the trimmed mean method; c(min, max)
#' @param burn.in         int: the number of periods to use in the first model estimation
#' @param parallel.dates  int: the number of cores available for parallel estimation
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export

# assumes a column named observed
forecast_combine = function(
  forecasts,              # data.frame: data frame of forecasted values to combine, assumes `date` and `observed` columns, but `observed' is not necessary for all methods
  method = 'unform',      # string or vector: the method to use; 'uniform', 'median', 'trimmed.mean', 'n.best', 'peLasso', 'lasso', 'ridge', 'elastic', 'RF', 'GBM', 'NN'
  n.max = NULL,           # int: maximum number of forecasts to select
  window = NA,            # int: size of rolling window to evaluate forecast error over, use entire period if NA
  trim = c(0.5, 0.95),    # numeric: a two element vector with the winsorizing bounds for the trimmed mean method; c(min, max)
  burn.in = 1,            # int: the number of periods to use in the first model estimation
  parallel.dates = NULL   # int: the number of cores available for parallel estimation
){

  # create parallel back end
  if(!is.null(parallel.dates)){
    future::plan(strategy = 'multisession', workers = parallel.dates)
  }else{
    future::plan(strategy = 'sequential')
  }

  # cast from long to wide
  forecasts = forecasts %>%
    dplyr::select(-se, -forecast.date) %>%
    tidyr::pivot_wider(names_from = model, values_from = forecast)

  results.list = list()

  # uniform weights
  if('uniform' %in% method){
    forecasts.raw = dplyr::select(forecasts, -dplyr::contains('date'), -dplyr::contains('observed'))
    combination = apply(forecasts.raw, MARGIN = 1, FUN = mean, na.rm = T)
    results.list[['unform']] = data.frame(date = forecasts$date, forecast = combination, model = 'uniform')
  }

  # median forecast
  if('median' %in% method){
    forecasts.raw = dplyr::select(forecasts, -dplyr::contains('date'), -dplyr::contains('observed'))
    combination = apply(forecasts.raw, MARGIN = 1, FUN = median, na.rm = T)
    results.list[['median']] = data.frame(date = forecasts$date, forecast = combination, model = 'median')
  }

  # trimmed (winsorized) mean
  if('trimmed.mean' %in% method){
    forecasts.raw = dplyr::select(forecasts, -dplyr::contains('date'), -dplyr::contains('observed'))
    combination = apply(forecasts.raw, MARGIN = 1, FUN = winsorize, bounds = trim, trim = FALSE)
    combination = apply(forecasts.raw, MARGIN = 1, FUN = mean, na.rm = T)
    results.list[['trimmed']] =  data.frame(date = forecasts$date, forecast = combination, model = 'trimmed.mean')
  }

  # N-best method
  if('n.best' %in% method){

    # warnings and errors
    if(!is.null(n.max)){
      errorCondition('Set n.max before using the n-best combination method')
    }
    if(!is.null(window)){
      warningCondition('The n-best method will default to using the entire forecast history')
    }

    # create n-best forecast combinations
    combination.nbest = NBest(dplyr::select(forecasts, -dplyr::contains('date')), n.max, window)
    combination.mean = apply(combination.nbest, MARGIN = 1, FUN = mean, na.rm = T)
    combination = data.frame(date = forecasts$date, combination.mean, combination.nbest) %>%
      dplyr::rename(N.best = combination.mean)
    combination = tidyr::pivot_longer(combination,
                                      cols = names(dplyr::select(combination, -date)),
                                      names_to = 'model',
                                      values_to = 'forecast')

    results.list[['nbest']] = combination
  }

  # peLASSO
  if('peLasso' %in% method){
    combination =
      forecasts$date[burn.in : nrow(forecasts)] %>%
      furrr::future_map(
        .f = function(forecast.date){

          # set data
          information.set = dplyr::filter(forecasts, forecast.date > date)
          current.forecasts = dplyr::filter(forecasts, forecast.date == date)

          # calculate peLasso method
          # stage 1, shrink to 0,
          # y-f -> eLasso to select subset of regressors
          x = as.matrix(dplyr::select(information.set , -observed, -date))
          y = information.set$observed - rowMeans(x)
          model = glmnet::cv.glmnet(x, y, alpha = 1, intercept = F, parallel = T)
          covariates = colnames(x)[which(as.vector(coef(model, s = 'lambda.min')) != 0)-1]

          # stage 2, shrink to 1/k,
          # y-f -> eRidge to shrink subset of regressors to uniform weights
          if(length(covariates) > 1){
            model = glmnet::cv.glmnet(x[,covariates], y, alpha = 0, intercept = F)
          }else{
            covariates = colnames(x)
          }

          # calculate forecast
          peLasso = predict(model, newx = as.matrix(current.forecasts[,covariates]), s = 'lambda.min') +
            rowMeans(dplyr::select(current.forecasts , -observed, -date))
          results = data.frame(date = current.forecasts$date, peLasso, model = 'peLasso')
          colnames(results)[colnames(results) == 'X1'] = 'forecast'
          return(results)

        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)

    results.list[['peLasso']] = combination
  }

  # ML algorithms via caret
  if(length(intersect(c('GBM','RF','NN','ols','lasso','ridge','elastic'), method)) > 0){

    # training parameter creation and warnings
    if(exists("forecast.combinations.ml.training")){
      print(warningCondition('forecast.combinations.ml.training exists and will be used for ML forecast combination techniques in its present state.'))
    }else{
      forecast.combinations.ml.training = instantiate.forecast.combinations.ml.training()
      print(warningCondition('forecast.combinations.ml.training was instantiated and default values will be used to train ML forecast combination techniques.'))
    }

    combination = intersect(c('GBM','RF','NN','ols','lasso','ridge','elastic'), method) %>%
      purrr::map(
        .f = function(engine){

          forecasts$date[burn.in : nrow(forecasts)] %>%
            furrr::future_map(
              .f = function(forecast.date){

                # set data
                information.set = dplyr::filter(forecasts, forecast.date > date)
                current.forecasts = dplyr::filter(forecasts, forecast.date == date)

                # estimate model
                model =
                  caret::train(observed~.,
                                data = dplyr::select(information.set, -date),
                                method    = forecast.combinations.ml.training$caret.engine[[engine]],
                                trControl = forecast.combinations.ml.training$control,
                                tuneGrid  = forecast.combinations.ml.training$tuning.grids[[engine]],
                                metric    = forecast.combinations.ml.training$accuracy,
                                na.action = na.omit)

                # calculate forecast
                point = predict(model, newdata = current.forecasts)

                # calculate standard error
                error =
                  try(
                    predict(model$finalModel, current.forecasts, interval = "confidence", level = 0.95) %>%
                      data.frame(),
                    silent = TRUE
                  )

                if('upr' %in% names(error) == TRUE){
                  error = (error$upr - error$fit) / qnorm(0.95)
                  error = as.numeric(error)
                }else{
                  error = NA
                }

                # set dates
                results = data.frame(date = current.forecasts$date,
                                     model = engine, forecast = point, se = error)
              }
            ) %>%
            purrr::reduce(dplyr::bind_rows)
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)

    results.list[['ML']] = combination
  }

  # return results
  results = purrr::reduce(results.list, dplyr::bind_rows)
  rownames(results) = c(1:nrow(results))
  return(results)
}
