# guess what, let's try directly modeling the total count! 
mod.tot.1   <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2), data = data2011)
mod.tot.1.5 <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2) + holiday + season, data = data2011)
mod.tot.2   <- lm(cnt ~ weathersit:workingday:atemp + I(atemp^2) + weathersit + season, data = data2011)
mod.tot.3   <- lm(cnt ~ season:workingday:atemp + season:I(atemp^2) + weathersit + holiday, data = data2011)
mod.tot.4   <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season, data = data2011)
mod.tot.4.5 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + holiday, data = data2011) #cas.2.7 + reg.4
mod.tot.4.7 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + holiday, data = data2011) #cas.3.5 + reg.4...oh it's the same...


mod.tot.1.0.5 <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2) + wavg_cnt, data = data2011)
mod.tot.1.5.5 <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2) + holiday + season + wavg_cnt, data = data2011)
mod.tot.2.0.5 <- lm(cnt ~ weathersit:workingday:atemp + I(atemp^2) + weathersit + season + wavg_cnt, data = data2011)
mod.tot.3.0.5 <- lm(cnt ~ season:workingday:atemp + season:I(atemp^2) + weathersit + holiday + wavg_cnt, data = data2011)
mod.tot.4.0.5 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + wavg_cnt, data = data2011)
mod.tot.4.5.5 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + holiday + wavg_cnt, data = data2011) #cas.2.7 + reg.4

mod.tot.1.0.5.5 <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2) + wavg_cnt + weekday, data = data2011)
mod.tot.1.5.5.5 <- lm(cnt ~ workingday + weathersit + atemp + I(atemp^2) + holiday + season + wavg_cnt + weekday, data = data2011)
mod.tot.2.0.5.5 <- lm(cnt ~ weathersit:workingday:atemp + I(atemp^2) + weathersit + season + wavg_cnt + weekday, data = data2011)
mod.tot.3.0.5.5 <- lm(cnt ~ season:workingday:atemp + season:I(atemp^2) + weathersit + holiday + wavg_cnt + weekday, data = data2011)
mod.tot.4.0.5.5 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + wavg_cnt + weekday, data = data2011)
mod.tot.4.5.5.5 <- lm(cnt ~ season:workingday:atemp + season:workingday:I(atemp^2) + weathersit:season + holiday + wavg_cnt + weekday, data = data2011) #cas.2.7 + reg.4
