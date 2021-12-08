mod.cas.0.0.5 <- lm(casual ~ 
                      wavg_cas, data = data2011AUG)
mod.reg.0.0.5 <- lm(registered ~ 
                      wavg_reg, data = data2011AUG)
mod.cas.0.5.5 <- lm(casual ~ 
                      season:wavg_cas, data = data2011AUG)
mod.reg.0.5.5 <- lm(registered ~ 
                      season:wavg_reg, data = data2011AUG)

mod.cas.1.5.5 <- lm(casual ~ 
                      workingday + weathersit + atemp + I(atemp^2) + season + wavg_cas, data = data2011AUG)
mod.reg.1.5.5 <- lm(registered ~ 
                      workingday + weathersit + atemp + I(atemp^2) + season + wavg_reg, data = data2011AUG)

mod.cas.2.7.5 <- lm(casual ~
                      workingday:atemp + weathersit:season + holiday + wavg_cas, 
                    data = data2011AUG) #current best
mod.cas.3.5.5 <- lm(casual ~
                      season:workingday:atemp + weathersit:season + holiday + wavg_cas, 
                    data = data2011AUG) #overfitting but leads to best total
mod.reg.4.0.5 <- lm(registered ~ 
                      season:workingday:atemp + season:workingday:I(atemp^2) + weathersit + holiday + wavg_reg, 
                    data = data2011AUG) #current best

mod.cas.0.0.5.5 <- lm(casual ~ 
                        wavg_cas + weekday, data = data2011AUG)
mod.reg.0.0.5.5 <- lm(registered ~ 
                        wavg_reg + weekday, data = data2011AUG)
mod.cas.0.5.5.5 <- lm(casual ~ 
                        season:wavg_cas + weekday, data = data2011AUG) #season not helping
mod.reg.0.5.5.5 <- lm(registered ~ 
                        season:wavg_reg + weekday, data = data2011AUG) #season not helping
mod.cas.1.0.5.5 <- lm(casual ~ 
                        wavg_cas + season:weekday + season:I(weekday^2) + workingday, data = data2011AUG)
mod.reg.1.0.5.5 <- lm(registered ~ 
                        wavg_reg + season:weekday + season:I(weekday^2) + workingday, data = data2011AUG)

mod.cas.1.5.5.5 <- lm(casual ~ 
                        workingday + weathersit + atemp + I(atemp^2) + season + wavg_cas + weekday, data = data2011AUG)
mod.reg.1.5.5.5 <- lm(registered ~ 
                        workingday + weathersit + atemp + I(atemp^2) + season + wavg_reg + weekday, data = data2011AUG)
mod.cas.2.7.5.5 <- lm(casual ~
                        workingday:atemp + weathersit:season + holiday + wavg_cas + weekday, 
                      data = data2011AUG) #current best
mod.cas.3.5.5.5 <- lm(casual ~
                        season:workingday:atemp + weathersit:season + holiday + wavg_cas + weekday, 
                      data = data2011AUG) #overfitting but leads to best total
mod.reg.4.0.5.5 <- lm(registered ~ 
                        season:workingday:atemp + season:workingday:I(atemp^2) + weathersit + holiday + wavg_reg + weekday, 
                      data = data2011AUG) #current best


mls_week <- ls(mod.cas.0.0.5, mod.reg.0.0.5, mod.cas.0.5.5, mod.reg.0.5.5,  mod.cas.1.5.5, mod.reg.1.5.5, 
               mod.cas.2.7.5, mod.cas.3.5.5, mod.reg.4.0.5, mod.cas.0.0.5.5, mod.reg.0.0.5.5, mod.cas.0.5.5.5, 
               mod.reg.0.5.5.5, mod.cas.1.0.5.5, mod.reg.1.0.5.5, mod.cas.1.5.5.5, mod.reg.1.5.5.5, mod.cas.2.7.5.5, 
               mod.cas.3.5.5.5, mod.reg.4.0.5.5)

# mls_week <- ls(mod.cas.0.0.5, mod.reg.0.0.5, # 1, 2
#                mod.cas.0.5.5, mod.reg.0.5.5, # 3, 4
#                mod.cas.1.5.5, mod.reg.1.5.5, # 5, 6 
#                mod.cas.2.7.5, mod.cas.3.5.5, # 7, 8
#                mod.reg.4.0.5, # 9
#                mod.cas.0.0.5.5, mod.reg.0.0.5.5, # 10, 11
#                mod.cas.0.5.5.5, mod.reg.0.5.5.5, # 12, 13
#                mod.cas.1.0.5.5, mod.reg.1.0.5.5, # 14, 15
#                mod.cas.1.5.5.5, mod.reg.1.5.5.5, # 16, 17
#                mod.cas.2.7.5.5, mod.cas.3.5.5.5, # 18, 19
#                mod.reg.4.0.5.5) # 20
