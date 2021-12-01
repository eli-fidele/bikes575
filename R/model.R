
MSE <- function(fits, trues){
  ep <- fits - trues
  SSE <- sum(ep^2)
  n <- length(fits)
  MSE <- (1/n) * SSE
  MSE
}

#MSE_model <- function(preds){ }