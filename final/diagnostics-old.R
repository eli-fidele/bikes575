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
