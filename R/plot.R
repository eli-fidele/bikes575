

plot_usertype_cnt <- function(data, user, compare = FALSE){
  ##########################
  ## Graphical parameters ##
  ##########################
  size0 <- 1.5; size1 <- 0.3
  pch0 <- 16; pch1 <- 20
  
  ###############
  ## Main plot ##
  ###############
  main_plot <- 
    data %>%
    ggplot(aes(x = dteday, y = .data[[user]], color = 'all day types')) + 
    geom_point(size = size0, pch = pch1, alpha = 0.8) + 
    # General plot settings
    labs(x = 'Date', y = 'Daily Registered Count') + 
    theme_grey(base_size = 5.5) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
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

# Dummy helper function that returns a string of the other user type
.switch_user <- function(user){
  if(user == "registered"){ return("casual") }
  if(user == "casual"){ return("registered") }
  else{ warning("User type not found")}
}

.plot_temp_cnt <- function(data){
  data %>% 
    ggplot() +
    geom_point(aes(x = instant, color = atemp, y = cnt)) +
    labs(title = "Bike User Count in 2011 by Temperature over Time") +
    scale_color_gradient(low="deepskyblue1", high = "tomato2")
}