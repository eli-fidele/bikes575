
# Returns a plot of the OLS model fit to temperature
OLS_temp_plot <- function(data, i){
  ggplot(data = data, aes(x = temp, y = cnt)) + 
    geom_point(size = 0.5, color = "deepskyblue4", alpha = 0.4) + 
    # Labels
    labs(x = "Temperature (ºC)", y = "Daily Count", 
         title = paste("Model",i)) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) + 
    # Line of fit
    geom_line(data = data, mapping = aes(x = temp, y = LS_fitvals[[i]], col = names(col_vec)[i]), lwd = 1) +
    scale_color_manual(values = col_vec[i], 
                       labels = names(col_vec)[i], 
                       name = 'Prediction')
}