
# Initial wrangling
wrangle_init <- function(data){
  # Boolean variables (from int to logical type)
  data$holiday <- as.logical(data$holiday)          #0 or 1
  data$workingday <- as.logical(data$workingday)    #0 or 1
  
  # Other categorical variables (from int to factor type)
  data$season <- as.factor(data$season)             #1 to 4
  data$yr <- as.factor(data$yr)                     #0 to 1
  data$mnth <- as.factor(data$mnth)                 #1 to 12
  data$weekday <- as.factor(data$weekday)           #0 to 6
  data$weathersit <- as.factor(data$weathersit)     #1 to 4
  
  # Return the wrangled dataset
  data
}