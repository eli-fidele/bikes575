.parseFormula <- function(predictors, response = "cnt"){
  f <- as.formula(
    paste(response, 
          paste(predictors, collapse = " + "), 
          sep = " ~ "))
  return(f)
}

lm.tot <- function(varset, train_data = data2011){
  model <- lm(formula = .parseFormula(predictors = varset, response = "cnt"), data = train_data)
  model
}
lm.cas <- function(varset, train_data = data2011){
  model <- lm(formula = .parseFormula(predictors = varset, response = "casual"), data = train_data)
  model
}
lm.reg <- function(varset, train_data = data2011){
  model <- lm(formula = .parseFormula(predictors = varset, response = "registered"), data = train_data)
  model
}
evaluate_model <- function(cas, reg){
  get_mod_eval(cas, reg, data2011, data2012 %>% filter(dteday != '2012-10-29'), scale_2012 = FALSE)
}