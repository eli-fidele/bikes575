

########################################################################

########################################################################

LOOCV <- function(predictors, response, train_data, summarize = T){
  is <- 1:nrow(train_data)
  residuals <- purrr::map_dbl(is, function(i){.LOOCV_i(i, predictors, response, train_data)})
  res_table <- data.frame(i = is, residual = residuals)
  if(summarize){ return(sqrt(mean(res_table$residual^2))) }
  return(res_table)
}

.LOOCV_i <- function(i, predictors, response, train_data){
  # Choose observation i as our test data
  test_data <- train_data[i,]
  # Omit it from the training data
  train_data <- train_data %>% anti_join(test_data)
  # Parse the predictor string vector into a formula then fit the model
  model <- lm(formula = .parseFormula(predictors, response), data = train_data)
  # Evaluate the residual of the omitted observation
  yhat <- predict(model, test_data)
  y <- test_data[[response]]
  epsilon <- (yhat - y)
  # Return the residual
  return(epsilon)
}

.parseFormula <- function(predictors, response = "cnt"){
  f <- as.formula(
    paste(response, 
          paste(predictors, collapse = " + "), 
          sep = " ~ "))
  return(f)
}

########################################################################

########################################################################


get_rmse <- function(y, y_hat, name='none'){
  rmse <- sqrt(mean((y - y_hat)^2))
  normalized_rmse <- sqrt(mean((y - y_hat)^2)) / sd(y)
  percentage_error <- mean(abs(y - y_hat) / y) * 100
  
  return(data.frame('name' = name, 'rmse' = rmse, 'normalized_rmse' = normalized_rmse, 'percentage_error' = percentage_error))
}

get_mod_eval <- function(mod.current.cas, mod.current.reg, data2011, data2012, scale_2012=TRUE){
  if (scale_2012){
    return(rbind(get_rmse(data2011$casual,     predict(mod.current.cas, data2011), '2011 casual'),
                 get_rmse(data2011$registered, predict(mod.current.reg, data2011), '2011 registered'),
                 get_rmse(data2011$cnt,        predict(mod.current.cas, data2011) + predict(mod.current.reg, data2011), '2011 total'),
                 get_rmse(data2012$casual,     predict(mod.current.cas, data2012)/0.608, '2012 casual'),
                 get_rmse(data2012$registered, predict(mod.current.reg, data2012)/0.608, '2012 registered'),
                 get_rmse(data2012$cnt,        predict(mod.current.cas, data2012)/0.608 + predict(mod.current.reg, data2012)/0.608, '2012 total')))
  }else{
    return(
      rbind(
        get_rmse(data2011$casual,     predict(mod.current.cas, data2011), '2011 casual'), 
        get_rmse(data2011$registered, predict(mod.current.reg, data2011), '2011 registered'), 
        get_rmse(data2011$cnt,        predict(mod.current.cas, data2011) + predict(mod.current.reg, data2011), '2011 total'), 
        get_rmse(data2012$casual,     predict(mod.current.cas, data2012), '2012 casual'), 
        get_rmse(data2012$registered, predict(mod.current.reg, data2012), '2012 registered'), 
        get_rmse(data2012$cnt,        predict(mod.current.cas, data2012) + predict(mod.current.reg, data2012), '2012 total'))
    )
  }
}

get_mod_eval_tot <-function(mod.current.tot, data2011, data2012, scale_2012=TRUE){
  if (scale_2012){
    return(rbind(get_rmse(data2011$cnt,        predict(mod.current.tot, data2011), '2011 total'),
                 get_rmse(data2012$cnt,        predict(mod.current.tot, data2012)/0.608, '2012 total')))
  }else{
    return(
      rbind(get_rmse(data2011$cnt,        predict(mod.current.tot, data2011), '2011 total'),
            get_rmse(data2012$cnt,        predict(mod.current.tot, data2012), '2012 total')))
  }
}
