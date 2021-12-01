
#========================#
#        OLS Models      #
#========================#
# Linear
m.ols.1 <- lm(cnt ~ temp, data = data)
# Quadratic
m.ols.2 <- lm(cnt ~ I(temp^2) + temp, data = data)
# Cubic
m.ols.3 <- lm(cnt ~ I(temp^3) + I(temp^2) + temp, data = data)
# Quartic
m.ols.4 <- lm(cnt ~ I(temp^4) + I(temp^3) + I(temp^2) + temp, data = data)

#===============================#
#        OLS Fitted Values      #
#===============================#
# Obtain the fitted values from each model
fitted_vals1 <- predict(m.ols.1)
fitted_vals2 <- predict(m.ols.2)
fitted_vals3 <- predict(m.ols.3)
fitted_vals4 <- predict(m.ols.4)