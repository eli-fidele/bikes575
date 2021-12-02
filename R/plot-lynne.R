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


plot_percentage_error <- function(model,data2011,data2012,is_casual=FALSE){
  p_error <- ggplot() 
  
  if (is_casual){
    p_error <- p_error + 
      geom_line(data2012, 
                mapping = aes(dteday, (casual - predict(model, data2012)/0.608)/casual, color = '2012 errors'), 
                size = 0.5, alpha = 0.4) + 
      geom_line(data2011, 
                mapping = aes(dteday, (casual - predict(model, data2011))/casual, color = '2011 errors'), 
                size = 0.5, alpha = 0.4)
    
    
    
    p_error <- p_error + 
      geom_point(data2012[!(data$workingday), ],
                 mapping = aes(dteday, (casual - predict(model, data2012[!(data$workingday), ])/0.608)/casual, color = 'on non-working days'),
                 size = 1.5, pch = 16, alpha = 1) +
      geom_point(data2011[!(data$workingday), ],
                 mapping = aes(dteday, (casual - predict(model, data2011[!(data$workingday), ]))/casual, color = 'on non-working days'),
                 size = 1.5, pch = 16, alpha = 1)
    
    
    p_error <- p_error + 
      geom_point(data2012[data$holiday, ],
                 mapping = aes(dteday, (casual - predict(model, data2012[data$holiday, ])/0.608)/casual, color = 'on holidays'),
                 size = 1.5, pch = 16, alpha = 1) +
      geom_point(data2011[data$holiday, ],
                 mapping = aes(dteday, (casual - predict(model, data2011[data$holiday, ]))/casual, color = 'on holidays'),
                 size = 1.5, pch = 16, alpha = 1) +
      scale_color_manual(values = cols[-c(4,5)],
                         labels = names(cols[-c(4,5)]),
                         name = 'Percentage Prediction Errors')
    
    p_error <- p_error + 
      labs(x = 'Date', y = 'Percenrage Error', 
           title = 'Percenrage Error (Trained on 2011, Tested on 2012)', 
           subtitle = 'Casual Counts')
  }else{
    p_error <- p_error + 
      geom_line(data2012, 
                mapping = aes(dteday, (registered - predict(model, data2012)/0.608)/registered, color = '2012 errors'), 
                size = 0.5, alpha = 0.4) + 
      geom_line(data2011, 
                mapping = aes(dteday, (registered - predict(model, data2011))/registered, color = '2011 errors'), 
                size = 0.5, alpha = 0.4)
    
    
    
    p_error <- p_error + 
      geom_point(data2012[!(data$workingday), ],
                 mapping = aes(dteday, (registered - predict(model, data2012[!(data$workingday), ])/0.608)/registered, color = 'on non-working days'),
                 size = 1.5, pch = 16, alpha = 1) +
      geom_point(data2011[!(data$workingday), ],
                 mapping = aes(dteday, (registered - predict(model, data2011[!(data$workingday), ]))/registered, color = 'on non-working days'),
                 size = 1.5, pch = 16, alpha = 1)
    
    
    p_error <- p_error + 
      geom_point(data2012[data$holiday, ],
                 mapping = aes(dteday, (registered - predict(model, data2012[data$holiday, ])/0.608)/registered, color = 'on holidays'),
                 size = 1.5, pch = 16, alpha = 1) +
      geom_point(data2011[data$holiday, ],
                 mapping = aes(dteday, (registered - predict(model, data2011[data$holiday, ]))/registered, color = 'on holidays'),
                 size = 1.5, pch = 16, alpha = 1) +
      scale_color_manual(values = cols[-c(4,5)],
                         labels = names(cols[-c(4,5)]),
                         name = 'Percentage Prediction Errors')
    
    p_error <- p_error + 
      labs(x = 'Date', y = 'Percenrage Error', 
           title = 'Percenrage Error (Trained on 2011, Tested on 2012)', 
           subtitle = 'Registered Counts')
  }
  
  
  # Text settings
  p_error <- p_error +
    ylim(-0.5, 0.5) + 
    geom_abline(slope = 0) + 
    theme_grey(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5))
  
  p_error
}