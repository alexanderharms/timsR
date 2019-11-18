# Exponentional smoothing modellen ------------------------------------------------------------------

# Algemene Exponentional Smoothing functie
train_exp_smooth <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  modelexp_smooth <- ets(x, model="ZZZ", damped=NULL, 
                         alpha=NULL, beta=NULL, gamma=NULL, phi=NULL, 
                         additive.only=FALSE, lambda=NULL, 
                         lower=c(rep(0.0001,3), 0.8), 
                         upper=c(rep(0.9999,3),0.98), 
                         opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3, 
                         bounds=c("both","usual","admissible"), 
                         ic=c("aicc","aic","bic"),
                         restrict=TRUE, 
                         allow.multiplicative.trend=FALSE, 
                         use.initial.values=FALSE)
  print(summary(modelexp_smooth))
  
  return(modelexp_smooth)
}

pred_exp_smooth <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Model 0: automatisch exponential smoothing model
train_exp_smooth1 <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  modelexp_smooth1 <- ets(x)
  print(summary(modelexp_smooth1))
  
  return(modelexp_smooth1)
}

pred_exp_smooth1 <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}


# Model 1: Holt - Winters - additief - met HoltWinters() functie 
train_holtwinters <- function(doelreeks) {
  x <- na.trim.ts(doelreeks)
  
  model_hw <- HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
                          seasonal = "additive",
                          start.periods = 2)
  print(summary(model_hw))
  return(model_hw)
}

pred_holtwinters <- function(model, h) {
  prediction <- model %>% 
    predict(n.ahead = h, prediction.interval = TRUE,
            level = 0.95)
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  
  return(return_df)
}

# Model 2 Holt - Winters - multiplicatief - met HoltWinters() functie
train_holtwinters2 <- function(doelreeks) {
  x <- na.trim.ts(doelreeks)
  
  model_hw2 <- HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
                          seasonal = c("mult"),
                          start.periods = 2, 
                          l.start = NULL, b.start = NULL,
                          s.start = NULL,
                          optim.start = c(alpha = 0.3, 
                                          beta = 0.1, 
                                          gamma = 0.1),
                          optim.control = list())
  print(summary(model_hw2))
  return(model_hw2)
}

pred_holtwinters2 <- function(model, h) {
  prediction <- model %>% 
    predict(n.ahead = h, prediction.interval = TRUE,
            level = 0.95)
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  
  return(return_df)
}

# Model 3: Holt - Winters - additief - met ets() functie
train_holtwinters3 <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  model_hw3 <- ets(x, model="AAA")
  print(summary(model_hw3))
  
  return(model_hw3)
}

pred_holtwinters3 <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Model 4: Holt - Winters - multiplicatief - met ets() functie
train_holtwinters4 <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  model_hw4 <- ets(x, model="MAM")
  print(summary(model_hw4))
  
  return(model_hw4)
}

pred_holtwinters4 <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Model 5: Simple Exponentional Smoothing - Alleen het niveau van de reeks 
# wordt geschat
train_s_exp_smooth <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  models_exp_smooth <- ets(x, model="ANN")
  print(summary(models_exp_smooth))
  
  return(models_exp_smooth)
}

pred_s_exp_smooth <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}
 
# Model 6: Holt - Niveau en trend worden geschat
train_holt <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  modelholt <- ets(x, model="AAN")
  print(summary(modelholt))
  
  return(modelholt)
}

pred_holt <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Model 7: Holt Winters Additief zonder trend
train_holtwinters5 <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  modelholtwinters5 <- ets(x, model="ANA")
  print(summary(modelholtwinters5))
  
  return(modelholtwinters5)
}

pred_holtwinters5 <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Model 8: Holt Winters multiplicatief zonder trend
train_holtwinters6 <- function(doelreeks){
  x <- na.trim.ts(doelreeks)
  
  modelholtwinters6 <- ets(x, model="MNM")
  print(summary(modelholtwinters6))
  
  return(modelholtwinters6)
}

pred_holtwinters6 <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}