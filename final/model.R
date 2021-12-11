mod.cas.1.0.5.5 <- lm(casual ~ 
                        wavg_cas + season:weekday + season:I(atemp^2) + workingday, data = data2011) # changed I(weekday^2) to atemp^2 since wasnt working..