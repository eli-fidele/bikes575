
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
