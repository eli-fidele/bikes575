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


# Reference:

# get_bp_decision = function(model, alpha) {
#   decide = unname(bptest(model)$p.value < alpha)
#   ifelse(decide, "Reject", "Fail to Reject")
# }
# 
# get_bp_pval = function(model) {
#   bptest(model)$p.value
# }
# 
# get_sw_decision = function(model, alpha) {
#   decide = unname(shapiro.test(resid(model))$p.value < alpha)
#   ifelse(decide, "Reject", "Fail to Reject")
# }
# 
# get_sw_pval = function(model) {
#   shapiro.test(resid(model))$p.value
# }
# 
# get_num_params = function(model) {
#   length(coef(model))
# }
# 
# get_loocv_rmse = function(model, is_log) {
#   ifelse(
#     is_log, 
#     sqrt(mean(na.omit(((dat_trn$price - exp(fitted(model))) / (1 - hatvalues(model))) ^ 2))),
#     sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
#   )
# }
# 
# get_adj_r2 = function(model) {
#   summary(model)$adj.r.squared
# }
# 
# test_mod = function(model, is_log = FALSE){
#   c(loocv_rmse = get_loocv_rmse(model, is_log), 
#     adj_r2 = get_adj_r2(model), 
#     bp_pval = get_bp_pval(model), 
#     sw_pval = get_sw_pval(model), 
#     num_params = get_num_params(model))
# }
# 
# diagnostics = function(model, pcol = "grey", lcol = "dodgerblue", alpha = 0.05, plotit = TRUE, testit = TRUE){
#   if (plotit){
#     par(mfrow = c(1, 2), pty="s")
#     
#     plot(fitted(model), resid(model), col = "grey", pch = 20, 
#          xlab = "Fitted", ylab = "Residual", 
#          main = "Fitted versus Residuals")
#     abline(h = 0, col = "darkorange", lwd = 2)
#     
#     qqnorm(resid(model), col = pcol)
#     qqline(resid(model), col = lcol, lwd = 2)
#   }
#   if (testit){
#     list(p_val = shapiro.test(resid(model))$p, 
#          decision = ifelse(test = shapiro.test(resid(model))$p < alpha, 
#                            yes = "Reject", no = "Fail to Reject"))
#   }
# }
# 
# get_test_rmse = function(model) {
#   sqrt(mean((dat_tst$price - predict(model, newdata = dat_tst))^ 2))
# }
# 
# get_perc_err = function(model) {
#   actual = dat_tst$price
#   predicted = predict(model, dat_tst)
#   100 * mean((abs(actual - predicted)) / actual)
# }