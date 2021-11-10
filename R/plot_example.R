
#======================================#
#         Plot Wrapper Functions       #
#======================================#

## Declare any global variables
# Ordered color vector indexed by model
col_vec <- c('Linear' = "cyan", 'Quadratic' = "blue", 'Cubic' = "red", 'Quartic' = 'black')
# Create a list (vector of anything, even vectors!) of all the fitted values indexed by model
LS_fitvals <- list(fitted_vals1, fitted_vals2, fitted_vals3, fitted_vals4) 

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

#===============================#
#        Individual Plots       #
#===============================#
# NOTE: We can also just write out individual plot code, and import the plot to our main file. This way, there is no nasty plot code!
# Plot the predicted values
all_model_plot <- 
  ggplot(data = data, aes(x = temp, y = cnt)) + 
  geom_point(size = 2, color = "deepskyblue4", alpha = 0.4) + 
  # Text options
  labs(x = "Temperature (ºC)", y = "Daily Count") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) + 
  # Lines of fit
  geom_line(data = data, mapping = aes(x = temp, y = fitted_vals1, col = names(col_vec)[1]), lwd = 1, alpha = 0.8) +
  geom_line(data = data, mapping = aes(x = temp, y = fitted_vals2, col = names(col_vec)[2]), lwd = 1, alpha = 0.8) +
  geom_line(data = data, mapping = aes(x = temp, y = fitted_vals3, col = names(col_vec)[3]), lwd = 1, alpha = 1) + 
  geom_line(data = data, mapping = aes(x = temp, y = fitted_vals4, col = names(col_vec)[4]), lwd = 1, alpha = 0.6) + 
  scale_color_manual(values = col_vec, 
                     labels = names(col_vec), 
                     name = 'Prediction')