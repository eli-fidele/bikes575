
cols <- c('all day types' = "darkgrey", 
          'on non-working days' = "darkgreen", 
          'on holidays' = "red", 
          'trends' = 'black', 
          'fitted' = 'purple')

p1 <- 
  ggplot(data2011, aes(dteday, registered, color = 'all day types')) + 
  geom_point(size = 1.5, pch = 20, alpha = 0.8) + 
  labs(x = 'Date', y = 'Daily Registered Count') + 
  
  theme_grey(base_size = 5.6) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  
  geom_line(data2011, 
            mapping = aes(dteday, registered, color = 'trends'), 
            size = 0.3, alpha = 0.8) + 
  
  geom_point(data2011[!(data2011$workingday), ], 
             mapping = aes(dteday, registered, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 1) + 
  geom_point(data2011[data2011$holiday, ], 
             mapping = aes(dteday, registered, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 1) + 
  scale_color_manual(values = cols, 
                     labels = names(cols), 
                     name = 'Daily Count')

p1 <- 
  p1 + geom_line(data2011, 
                 mapping = aes(dteday, casual, color = 'trends'), 
                 size = 0.3, alpha = 0.2) + 
  
  geom_point(data2011[!(data2011$workingday), ], 
             mapping = aes(dteday, casual, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 0.2) + 
  geom_point(data2011[data2011$holiday, ], 
             mapping = aes(dteday, casual, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 0.2)

p2 <-
  ggplot(data2011, aes(dteday, casual, color = 'all day types')) + 
  geom_point(size = 1.5, pch = 20, alpha = 0.8) + 
  labs(x = 'Date', y = 'Daily Registered Count') + 
  
  theme_grey(base_size = 5.6) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  
  geom_line(data2011, 
            mapping = aes(dteday, casual, color = 'trends'), 
            size = 0.3, alpha = 0.8) + 
  
  geom_point(data2011[!(data2011$workingday), ], 
             mapping = aes(dteday, casual, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 1) + 
  geom_point(data2011[data2011$holiday, ], 
             mapping = aes(dteday, casual, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 1) + 
  scale_color_manual(values = cols, 
                     labels = names(cols), 
                     name = 'Daily Count')

p2 <- p2 + 
  geom_line(data2011, 
            mapping = aes(dteday, registered, color = 'trends'), 
            size = 0.3, alpha = 0.2) + 
  
  geom_point(data2011[!(data2011$workingday), ], 
             mapping = aes(dteday, registered, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 0.2) + 
  geom_point(data2011[data2011$holiday, ], 
             mapping = aes(dteday, registered, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 0.2)

p3 <- 
  ggplot(data2012, aes(dteday, registered, color = 'all day types')) + 
  geom_point(size = 1.5, pch = 20, alpha = 0.8) + 
  labs(x = 'Date', y = 'Daily Registered Count') + 
  
  theme_grey(base_size = 5.6) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  
  geom_line(data2012, 
            mapping = aes(dteday, registered, color = 'trends'), 
            size = 0.3, alpha = 0.8) + 
  
  geom_point(data2012[!(data2012$workingday), ], 
             mapping = aes(dteday, registered, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 1) + 
  geom_point(data2012[data2012$holiday, ], 
             mapping = aes(dteday, registered, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 1) + 
  scale_color_manual(values = cols, 
                     labels = names(cols), 
                     name = 'Daily Count')
p3 <- 
  p3 + geom_line(data2012, 
                 mapping = aes(dteday, casual, color = 'trends'), 
                 size = 0.3, alpha = 0.2) + 
  
  geom_point(data2012[!(data2012$workingday), ], 
             mapping = aes(dteday, casual, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 0.2) + 
  geom_point(data2012[data2012$holiday, ], 
             mapping = aes(dteday, casual, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 0.2)

p4 <- 
  ggplot(data2012, aes(dteday, casual, color = 'all day types')) + 
  geom_point(size = 1.5, pch = 20, alpha = 0.8) + 
  labs(x = 'Date', y = 'Daily Registered Count') + 
  
  theme_grey(base_size = 5.6) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  
  geom_line(data2012, 
            mapping = aes(dteday, casual, color = 'trends'), 
            size = 0.3, alpha = 0.8) + 
  
  geom_point(data2012[!(data2012$workingday), ], 
             mapping = aes(dteday, casual, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 1) + 
  geom_point(data2012[data2012$holiday, ], 
             mapping = aes(dteday, casual, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 1) + 
  scale_color_manual(values = cols, 
                     labels = names(cols), 
                     name = 'Daily Count')

p4 <- p4 + 
  geom_line(data2012, 
            mapping = aes(dteday, registered, color = 'trends'), 
            size = 0.3, alpha = 0.2) + 
  
  geom_point(data2012[!(data2012$workingday), ], 
             mapping = aes(dteday, registered, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 0.2) + 
  geom_point(data2012[data2012$holiday, ], 
             mapping = aes(dteday, registered, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 0.2)

### Weekly stuff

p2l <- p2 + geom_line(data2011AUG, 
                      mapping = aes(dteday, predict(mod.cas.1.0.5.5, data2011AUG), color = 'fitted'), 
                      size = 0.5, alpha = 0.7)

p1l <- p1 + geom_line(data2011AUG, 
                      mapping = aes(dteday, predict(mod.reg.1.0.5.5, data2011AUG), color = 'fitted'), 
                      size = 0.5, alpha = 0.7)

p4l <- p4 + geom_line(data2012AUG, 
                      mapping = aes(dteday, predict(mod.cas.1.0.5.5, data2012AUG), color = 'fitted'), 
                      size = 0.5, alpha = 0.7)

p3l <- p3 + geom_line(data2012AUG, 
                      mapping = aes(dteday, predict(mod.reg.1.0.5.5, data2012AUG), color = 'fitted'), 
                      size = 0.5, alpha = 0.7)