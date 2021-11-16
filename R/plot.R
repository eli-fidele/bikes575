

.plot_temp_cnt <- function(data){
  data %>% 
    ggplot() +
    geom_point(aes(x = instant, color = atemp, y = cnt)) +
    labs(title = "Bike User Count in 2011 by Temperature")
}