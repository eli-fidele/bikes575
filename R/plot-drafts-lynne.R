
cols <- c('all day types' = "darkgrey", 'on non-working days' = "darkgreen", 'on holidays' = "red", 'trends' = 'black')

data2011 = data[data$dteday < '2012-01-01', ]

ggplot(data2011, aes(dteday, cnt, color = 'all day types')) + 
  geom_point(size = 1.5, pch = 20, alpha = 0.8) + 
  labs(x = 'Date', y = 'Daily Count') + 
  
  theme_grey(base_size = 5.6) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  
  geom_line(data2011, 
            mapping = aes(dteday, cnt, color = 'trends'), 
            size = 0.3, alpha = 0.8) + 
  
  geom_point(data2011[!(data2011$workingday), ], 
             mapping = aes(dteday, cnt, color = 'on non-working days'), 
             size = 1.5, pch = 16, alpha = 1) + 
  geom_point(data2011[data2011$holiday, ], 
             mapping = aes(dteday, cnt, color='on holidays'), 
             size = 1.5, pch = 16, alpha = 1) + 
  scale_color_manual(values = cols, 
                     labels = names(cols), 
                     name = 'Daily Count')