
#=============================#
#      Global Parameters      #
#=============================#

cols <- c('all day types' = "darkgrey", 
          'on non-working days' = "darkgreen", 
          'on holidays' = "red", 
          'trends' = 'black', 
          'fitted' = 'purple')

#========================================#
#      Count Users Plots: Functions      #
#========================================#

plot_user_counts <- function(data, user, compare = FALSE){
  ##########################
  ## Graphical parameters ##
  ##########################
  size0 <- 1.5; size1 <- 0.3 
  pch0 <- 16; pch1 <- 20 
  fsize0 <- 13; fsize1 <- 10; fsize2 <- 11 # Font sizes
  ###############
  ## Main plot ##
  ###############
  main_plot <- 
    data %>%
    ggplot(aes(x = dteday, y = .data[[user]], color = 'all day types')) + 
    geom_point(size = size0, pch = pch1, alpha = 0.8) + 
    # General plot settings
    labs(x = 'Date', y = 'Daily Registered Count', title = .title_helper(user)) + 
    theme_grey(base_size = 5.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = fsize0, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = fsize1),
          plot.caption = element_text(hjust = 0.5, size = fsize1),
          axis.text.x = element_text(size = fsize2),
          axis.text.y = element_text(size = fsize2),
          axis.title.x = element_text(size = fsize2),
          axis.title.y = element_text(size = fsize2),
          legend.title=element_text(size = fsize0), 
          legend.text=element_text(size = fsize1),
          legend.position = "bottom")+
    # Geoms
    geom_line(mapping = aes(x = dteday, .data[[user]], color = 'trends'), 
              size = size1, alpha = 0.8) + 
    geom_point(data %>% filter(!workingday), 
               mapping = aes(x = dteday, y = .data[[user]], color = 'on non-working days'), 
               size = size0, pch = pch0, alpha = 1) + 
    geom_point(data %>% filter(holiday), 
               mapping = aes(x = dteday, y = .data[[user]], color = 'on holidays'),
               size = size0, pch = pch0, alpha = 1) + 
    # Color settings
    scale_color_manual(values = cols, 
                       labels = names(cols), 
                       name = 'Daily Count')
  ################
  ## Comparsion ##
  ################
  # If other user type data series is not desired, return the main plot
  if(!compare){ return(main_plot) }
  # Otherwise, append the other user data before returning (at a lower alpha)
  main_plot +
    geom_line(data = data, 
              mapping = aes(x = dteday, y = .data[[.switch_user(user)]], color = 'trends'), 
              size = size1, alpha = 0.2) + 
    geom_point(data %>% filter(!workingday), 
               mapping = aes(x = dteday, y = .data[[.switch_user(user)]], color = 'on non-working days'), 
               size = size0, pch = pch0, alpha = 0.2) + 
    geom_point(data %>% filter(holiday), 
               mapping = aes(x = dteday, y = .data[[.switch_user(user)]], color = 'on holidays'), 
               size = size0, pch = pch0, alpha = 0.2)
}

.title_helper <- function(user){
  user_title <- str_to_title(user)
  paste("Daily User Count for ",user_title," Users", sep = "")
}

# Dummy helper function that returns a string of the other user type
.switch_user <- function(user){
  if(user == "registered"){ return("casual") }
  if(user == "casual"){ return("registered") }
  else{ warning("User type not found")}
}

#=============================#
#      Count Users Plots      #
#=============================#

p1 <- data2011 %>% 
  plot_user_counts(user = "registered", compare = F)
p1_compare <- data2011 %>% 
  plot_user_counts(user = "registered", compare = T)


p2 <- data2011 %>% 
  plot_user_counts(user = "casual", compare = F)
p2_compare <- data2011 %>% 
  plot_user_counts(user = "casual", compare = T)

p3 <- data2011 %>% 
  plot_user_counts(user = "registered", compare = F)
p3_compare <- data2012 %>% 
  plot_user_counts(user = "registered", compare = T)


p4 <- data2011 %>% 
  plot_user_counts(user = "casual", compare = F)
p4_compare <- data2012 %>% 
  plot_user_counts(user = "casual", compare = T)

#=============================#
#      Weekly Line Plots      #
#=============================#

pw1 <- 
  ggplot() + 
  geom_line(data2011, 
            mapping = aes(dteday, data2011$wavg_cnt, color = '1'), 
            size = 0.5, alpha = 0.7) + 
  geom_line(data2011, 
            mapping = aes(dteday, data2012$wavg_cnt[-359], color = '2'), 
            size = 0.5, alpha = 0.7)
pw2 <- ggplot() + 
  geom_line(data2011,
            mapping = aes(dteday, data2011$wavg_cnt / data2012$wavg_cnt[-359], color = '3'), 
            size = 0.5, alpha = 0.7)

#========================================#
#      Predicition Plots: Functions      #
#========================================#

predict2011reg_plot <- function(model){
  fitted_vals <- predict(model, data2011)
  p1_compare + geom_line(data2011, mapping = aes(dteday, fitted_vals, color = 'fitted'), size = 0.5, alpha = 0.7)
}

predict2011cas_plot <- function(model){
  fitted_vals <- predict(model, data2011)
  p2_compare + geom_line(data2011, mapping = aes(dteday, fitted_vals, color = 'fitted'), size = 0.5, alpha = 0.7)
}

predict2012reg_plot <- function(model, scale_2012 = F){
  if(scale_2012){G_FACTOR <- 0.608} else{G_FACTOR <- 1}
  fitted_vals <- predict(model, data2012) / G_FACTOR
  p3_compare + geom_line(data2012, mapping = aes(dteday, fitted_vals, color = 'fitted'), size = 0.5, alpha = 0.7)
}

predict2012cas_plot <- function(model, scale_2012 = F){
  if(scale_2012){G_FACTOR <- 0.608} else{G_FACTOR <- 1}
  fitted_vals <- predict(model, data2012) / G_FACTOR
  p4_compare + geom_line(data2012, mapping = aes(dteday, fitted_vals, color = 'fitted'), size = 0.5, alpha = 0.7)
}

#=============================#
#      Predicition Plots      #
#=============================#

# p1_preds <- predict2011reg_plot(mod.cas.1.0.5.5)
# p2_preds <- predict2011cas_plot(mod.cas.1.0.5.5)
# p3_preds <- predict2012reg_plot(mod.cas.1.0.5.5)
# p4_preds <- predict2012cas_plot(mod.cas.1.0.5.5)

#=========================#
#      Miscellaneous      #
#=========================#

.plot_temp_cnt <- function(data = data2011){
  data %>% 
    ggplot() +
    geom_point(aes(x = dteday, color = atemp, y = cnt)) +
    labs(title = "Bike User Count in 2011 by Temperature over Time") +
    scale_color_gradient(low="deepskyblue1", high = "tomato2")
}
