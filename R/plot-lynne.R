plot_pairs <- function(data, group=1, y_name='cnt', var_list_new=c()){
  if (group==1){var_list <- c('dteday','weathersit','temp','atemp','windspeed','hum', y_name);}
  if (group==2){var_list <- c('dteday','season','yr','mnth','holiday','weekday','workingday', y_name)}
  if (group==0){var_list <- c(var_list_new, y_name)}
  
  ggpairs(data[var_list], 
          progress = F, 
          lower = list(continuous = wrap("points", alpha = 0.3, size=0.1)), 
          upper = list(continuous = wrap("cor", size=2))) + 
    
    # Text settings
    labs(x = "", y = "", 
         title = paste(paste("Pairs Plots (Group",group),")"), 
         subtitle = paste("Response Variable (see last row & columnn): ",y_name)) + 
    theme_grey(base_size = 10) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5), 
          axis.text = element_text(size = 4))
}


plot_pairs_colored <- function(data, group=1, y_name='cnt', color_name, var_list_new=c()){
  if (group==1){var_list <- c('dteday','weathersit','temp','atemp','windspeed','hum', y_name)}
  if (group==2){var_list <- c('dteday','season','yr','mnth','holiday','weekday','workingday', y_name)}
  if (group==0){var_list <- c(var_list_new, y_name)}
  
  ggpairs(data[var_list], 
          progress = F, 
          lower = list(continuous = wrap("points", alpha = 0.3, size=0.1)), 
          upper = list(continuous = wrap("cor", size=2)), 
          aes(colour=data[,color_name])) + 
    
    # Text settings
    labs(x = "", y = "", 
         title = paste(paste("Pairs Plots (Group",group),")"),
         subtitle = paste(paste(paste("Colored by:",color_name),"\n Response Variable (see last row & columnn): "),y_name)) + 
    theme_grey(base_size = 10) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5), 
          axis.text = element_text(size = 4))
}
