
mod.cas.1.final <- lm.cas(c("workingday", "weathersit", "atemp"))
mod.reg.1.final <- lm.reg(c("workingday", "weathersit", "atemp", "I(atemp^2)"))
mod.tot.1.final <- lm.tot(c("workingday", "weathersit", "atemp", "I(atemp^2)"))

mod.cas.2.final <- lm.cas(c("holiday", "season:weathersit", "season:workingday:atemp"))
mod.reg.2.final <- lm.reg(c("holiday", "season:weathersit", "season:workingday:atemp", "season:workingday:I(atemp^2)"))




residual_QQ <- function(model, title_str = ""){
  # Plot parameters 
  fsize0 <- 13; fsize1 <- 13; fsize2 <- 13
  # Plot
  df_res <- data.frame(ep = model$residuals)
  df_res %>% 
    ggplot(aes(sample = ep)) +
    geom_qq(alpha = 0.8, color = "palegreen3") +
    stat_qq_line(color = "seagreen") +
    labs(x = "Theoretical Quantile", y = "Sample Quantiles", title = paste("Normal Q-Q Plot", title_str, sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5, size = fsize0, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = fsize1),
          plot.caption = element_text(hjust = 0.5, size = fsize1),
          axis.text.x = element_text(size = fsize2),
          axis.text.y = element_text(size = fsize2),
          axis.title.x = element_text(size = fsize2),
          axis.title.y = element_text(size = fsize2),
          legend.title=element_text(size = fsize0), 
          legend.text=element_text(size = fsize1))
}

residual_fitted <- function(model, title_str = ""){
  # Plot parameters 
  fsize0 <- 13; fsize1 <- 13; fsize2 <- 13
  # Plot
  df_res <- data.frame(fitted_value = model$fitted.values, residual = model$residuals)
  df_res %>%
    ggplot(aes(x = fitted_value, y = residual)) +
    geom_point(alpha = 0.7, color = "palegreen3") +
    stat_smooth(se = T, color = "forestgreen") +
    labs(x = "Fitted Value", y = "Residual", title = paste("Fitted vs Residual",title_str, sep = ""))+ 
    theme(plot.title = element_text(hjust = 0.5, size = fsize0, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = fsize1),
          plot.caption = element_text(hjust = 0.5, size = fsize1),
          axis.text.x = element_text(size = fsize2),
          axis.text.y = element_text(size = fsize2),
          axis.title.x = element_text(size = fsize2),
          axis.title.y = element_text(size = fsize2),
          legend.title=element_text(size = fsize0), 
          legend.text=element_text(size = fsize1))
}

residual_histogram <- function(model, title_str = ""){
  # Plot parameters 
  fsize0 <- 13; fsize1 <- 13; fsize2 <- 13
  # Plot
  epsilons <- unname(model$residuals)
  df_res <- data.frame(epsilon = epsilons)
  df_res %>%
    ggplot(aes(x = epsilon)) +
    geom_histogram(fill = "forestgreen", bins = 60) +
    labs(x = "Residual", title = paste("Residual Distribution",title_str, sep = ""))+ 
    theme(plot.title = element_text(hjust = 0.5, size = fsize0, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = fsize1),
          plot.caption = element_text(hjust = 0.5, size = fsize1),
          axis.text.x = element_text(size = fsize2),
          axis.text.y = element_text(size = fsize2),
          axis.title.x = element_text(size = fsize2),
          axis.title.y = element_text(size = fsize2),
          legend.title=element_text(size = fsize0), 
          legend.text=element_text(size = fsize1))
}

residual_plots <- function(model, title_str){
  p1 <- model %>% residual_QQ(title_str)
  p2 <- model %>% residual_histogram(title_str)
  p3 <- model %>% residual_fitted(title_str)
  (p1 + p2 + p3)
}