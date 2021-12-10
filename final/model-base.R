
mod.cas.1 <- lm(casual ~ 
                  workingday + weathersit + atemp + I(atemp^2), data = data2011)
mod.reg.1 <- lm(registered ~ 
                  workingday + weathersit + atemp + I(atemp^2), data = data2011)

# Model 1.5: adding season

mod.cas.1.5 <- lm(casual ~ 
                    workingday + weathersit + atemp + I(atemp^2) + season, data = data2011)
mod.reg.1.5 <- lm(registered ~ 
                    workingday + weathersit + atemp + I(atemp^2) + season, data = data2011)

# Model 2: an intermediate model, slightly more complicated

mod.cas.2 <- lm(casual ~ 
                  weathersit:workingday:atemp + season, data = data2011)
mod.reg.2 <- lm(registered ~ 
                  weathersit:workingday:atemp + I(atemp^2) + weathersit + season, data = data2011)

# Model 2.5: having season as a interception or an interaction?

mod.cas.2.5 <- lm(casual ~
                    workingday:atemp + weathersit:season, data = data2011)
mod.reg.2.5 <- lm(registered ~
                    workingday:atemp + I(atemp^2) + weathersit + season, data = data2011) 

# adding the holiday variable for cas
mod.cas.2.7 <- lm(casual ~
                    workingday:atemp + weathersit:season + holiday, data = data2011) #current best

# Model 3: having season as an interaction term is better
mod.cas.3 <- lm(casual ~
                  season:workingday:atemp + weathersit + holiday, data = data2011)  #overfitting
mod.reg.3 <- lm(registered ~
                  season:workingday:atemp + season:I(atemp^2) + weathersit, data = data2011)

mod.cas.3.5 <- lm(casual ~
                    season:workingday:atemp + weathersit:season + holiday, data = data2011) #overfitting but leads to best total

# adding the holiday variable for reg
mod.reg.3.7 <- lm(registered ~
                    season:workingday:atemp + season:I(atemp^2) + weathersit + holiday, data = data2011) 


# Model 4: then try a more complex model - but it starts to overfit #alright now no longer overfitting
mod.reg.4 <- lm(registered ~ 
                  season:workingday:atemp + season:workingday:I(atemp^2) + weathersit + holiday, data = data2011) #current best
mod.reg.4.5 <- lm(registered ~ 
                    season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + holiday, data = data2011) #overfitting


# current best models
mod.cas.best <- mod.cas.2.7
mod.reg.best <- mod.reg.4

# try removing the y-space outlier
mod.cas.best. <- lm(casual ~
                      season:workingday:atemp + weathersit:season + holiday, 
                    data = filter(data2011, dteday != '2011-08-29'))
mod.reg.best. <- lm(registered ~
                      season:workingday:atemp + season:workingday:I(atemp^2) + weathersit + holiday, 
                    data = filter(data2011, dteday != '2011-08-29'))