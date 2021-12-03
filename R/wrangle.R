
#========================#
#         Wrangling      #
#========================#
# Initial wrangling
wrangle_init <- function(data, omit_NA = TRUE, omit_idx = TRUE){
  # Boolean variables (from int to logical type)
  data$holiday <- as.logical(data$holiday)          # 0 or 1
  data$workingday <- as.logical(data$workingday)    # 0 or 1
  # Other categorical variables (from int to factor type)
  data$season <- as.factor(data$season)             # 1 to 4
  data$yr <- as.factor(data$yr)                     # 0 to 1
  data$mnth <- as.factor(data$mnth)                 # 1 to 12
  data$weekday <- as.factor(data$weekday)           # 0 to 6
  data$weathersit <- as.factor(data$weathersit)     # 1 to 4
  # Re-scale the normalized measurements
  data$temp <- data$temp * 41
  data$atemp <- data$atemp * 50
  data$hum <- data$hum * 100
  data$windspeed <- data$windspeed * 67
  # Change type of Dates (from char to Date type)
  data$dteday <- as.Date(data$dteday)
  # Remove NAs (if prompted) default value is TRUE
  if(omit_NA) { data <- na.omit(data) }
  # Remove instance column (if prompted) default value is TRUE
  if(omit_idx) { data <- data %>% select(-c("instant")) }
  # Observe christmas
  data$holiday[359] <- T; data$holiday[725] <- T
  # Add the scaled count variable (adjusting for growth in bikesharing)
  data$cnt_adj <- data$cnt
  r <- 0.6081
  data[which(data$yr == 1), "cnt_adj"] <- r * data[which(data$yr == 1), "cnt_adj"] 
  # Return the wrangled dataset
  return(data)
}

#========================#
#        Subsetting      #
#========================#
# Filter for the 2011 data
in_2011 <- function(data){ return(data[(data$dteday >= "2011-01-01" & data$dteday <= "2011-12-31"),]) }
# Filter for the 2012 data
in_2012 <- function(data){ return(data[(data$dteday >= "2012-01-01" & data$dteday <= "2012-12-31"),]) }