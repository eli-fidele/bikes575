


########################################################################

########################################################################

LOOCV <- function(predictors, response, train_data = data2011, summarize = T){
  is <- 1:nrow(train_data)
  residuals <- purrr::map_dbl(is, function(i){.LOOCV_i(i, predictors, response, train_data)})
  res_table <- data.frame(i = is, residual = residuals)
  if(summarize){ return(sqrt(mean(res_table$residual^2, na.rm = TRUE))) }
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
  rmse <- sqrt(mean((y - y_hat)^2, na.rm = TRUE))
  normalized_rmse <- sqrt(mean(((y - y_hat)^2), na.rm = TRUE)) / sd(y)
  percentage_error <- mean(abs(y - y_hat) / y, na.rm = TRUE) * 100
  # Round the numbers
  digits <- 2
  rmse <- round(rmse, digits)
  normalized_rmse <- round(normalized_rmse, digits)
  percentage_error <- round(percentage_error, digits)
  # Return the dataframe row
  return(data.frame('name' = name, 'rmse' = rmse, 'nrmse' = normalized_rmse, 'prc_err' = percentage_error))
}

get_loocv_rmse = function(model) {
  loocv_rmse <- sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
  # Round the numbers
  digits <- 2
  loocv_rmse <- round(loocv_rmse, digits)
  # Return the dataframe row
  return(data.frame('CV_rmse' = loocv_rmse))
}

get_loocv_rmse_tot = function(cas, reg) {
  res_cas <- resid(cas) / (1 - hatvalues(cas))
  res_reg <- resid(reg) / (1 - hatvalues(reg))
  loocv_rmse <- sqrt(mean((res_cas + res_reg) ^ 2))
  # Round the numbers
  digits <- 2
  loocv_rmse <- round(loocv_rmse, digits)
  # Return the dataframe row
  return(data.frame('CV_rmse' = loocv_rmse))
}

########################################################################

########################################################################

get_mod_eval_2011 <- function(cas, reg){
  ret1 <- rbind(get_rmse(data2011$casual,     predict(cas, data2011), '2011 cas'),
               get_rmse(data2011$registered, predict(reg, data2011), '2011 reg'),
               get_rmse(data2011$cnt,        predict(cas, data2011) + predict(reg, data2011), '2011 tot'))
  
  ret2 <- rbind(
    get_loocv_rmse(cas),
    get_loocv_rmse(reg),
    get_loocv_rmse_tot(cas, reg)
  )
  
  return(
    cbind(ret1, ret2)
  )
}

get_mod_eval <- function(cas, reg, data2011, data2012, scale_2012 = TRUE, include_2011 = F){
  if(scale_2012){G_FACTOR <- 0.608} else{G_FACTOR <- 1}
  
  if(include_2011){
    return(
      rbind(get_rmse(data2011$casual,     predict(cas, data2011), '2011 cas'),
            get_rmse(data2011$registered, predict(reg, data2011), '2011 reg'),
            get_rmse(data2011$cnt,        predict(cas, data2011) + predict(reg, data2011), '2011 tot'),
            get_rmse(data2012$casual,     predict(cas, data2012)/G_FACTOR, '2012 cas'),
            get_rmse(data2012$registered, predict(reg, data2012)/G_FACTOR, '2012 reg'),
            get_rmse(data2012$cnt,        predict(cas, data2012)/G_FACTOR + predict(reg, data2012)/G_FACTOR, '2012 tot')
      )
    )
  }else{
    return(
      rbind(
            get_rmse(data2012$casual,     predict(cas, data2012)/G_FACTOR, '2012 cas'),
            get_rmse(data2012$registered, predict(reg, data2012)/G_FACTOR, '2012 reg'),
            get_rmse(data2012$cnt,        predict(cas, data2012)/G_FACTOR + predict(reg, data2012)/G_FACTOR, '2012 tot')
           ))
  }
}

get_mod_eval_tot_2011 <-function(tot){
  ret1 <- rbind(get_rmse(data2011$cnt, predict(tot, data2011), '2011 tot'))
  
  ret2 <- rbind(
    get_loocv_rmse(tot)
  )
  
  return(
    cbind(ret1, ret2)
  )
}
  
#   {
#   return(rbind(get_rmse(data2011$cnt, predict(tot, data2011), '2011 total'),
#                get_loocv_rmse(tot)))
# }

get_mod_eval_tot <-function(tot, data2011, data2012, scale_2012 = TRUE){
  if(scale_2012){G_FACTOR <- 0.608} else{G_FACTOR <- 1}
    return(rbind(get_rmse(data2011$cnt, predict(tot, data2011), '2011 tot'),
                 get_rmse(data2012$cnt, predict(tot, data2012)/G_FACTOR, '2012 tot'), 
                 get_loocv_rmse(tot)))
}
