# ARIMA -----------------------------------------------------------------------
train_arima <- function(target_series, regressors=NULL){
  x <- target_series
  
  if (is.null(regressor)) {
    # modelarima <- stats::arima(x, order = c(1L, 1L, 1L))
    modelarima <- arima(x, order = c(1L, 1L, 1L),
                        seasonal = list(order = c(1L, 0L, 1L), period = NA),
                        xreg = NULL, 
                        include.mean = TRUE,
                        transform.pars = TRUE,
                        fixed = NULL, init = NULL,
                        method = c("CSS-ML", "ML", "CSS"), 
                        SSinit = c("Gardner1980", "Rossignol2011"),
                        optim.method = "BFGS",
                        optim.control = list(), kappa = 1e6)
  } else {
    # modelarima <- arima(x, order = c(1L, 1L, 1L))
    modelarima <- arima(x, order = c(1L, 1L, 1L),
                        seasonal = list(order = c(1L, 0L, 1L), period = NA),
                        xreg = regressors[1:(length(x)),], 
                        include.mean = TRUE,
                        transform.pars = TRUE,
                        fixed = NULL, init = NULL,
                        method = c("CSS-ML", "ML", "CSS"), 
                        SSinit = c("Gardner1980", "Rossignol2011"),
                        optim.method = "BFGS",
                        optim.control = list(), kappa = 1e6)
  }
  
  
  print(summary(modelarima))
  return(modelarima)
}

pred_arima <- function(model, h) {
  # prediction <- model %>% forecast(h=h) %>% .$mean
  prediction <- model %>% 
    predict(n.ahead=h, se.fit = TRUE) 
  
  return_df <- data.frame("prediction" = prediction$pred,
                          "lower_conf" = prediction$pred - 1.96*prediction$se,
                          "upper_conf" = prediction$pred + 1.96*prediction$se)
  
  return(return_df)
}

# Holt - Winters --------------------------------------------------------------
train_holtwinters <- function(target_series) {
  x <- target_series
  
  model_hw <- HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
                          seasonal = c("additive", "multiplicative"),
                          start.periods = 2, 
                          l.start = NULL, b.start = NULL,
                          s.start = NULL,
                          optim.start = c(alpha = 0.3, 
                                          beta = 0.1, 
                                          gamma = 0.1),
                          optim.control = list())
  print(summary(model_hw))
  return(model_hw)
}

pred_holtwinters <- function(model, h) {
  prediction <- model %>% 
    predict(n.ahead = h, prediction.interval = TRUE,
            level = 0.95)
  
  return_df <- data.frame("prediction" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  
  return(return_df)
}

# Naiëf model -----------------------------------------------------------
train_naief <- function(target_series){
  x <- target_series
  return(x)
  
}

pred_naief <- function(x, h) {
  prediction <- naive(x, h=h, level=c(80, 95), fan=FALSE, lambda=NULL)
  print(summary(prediction))
  
  return_df <- data.frame("prediction" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Seizoens naief model (snaief) -----------------------------------------

train_snaief <- function(target_series){
  x <- target_series
  return(x)
}

pred_snaief <- function(x, h) {
  prediction <- snaive(x, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  print(summary(prediction))
  
  return_df <- data.frame("prediction" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

# Exponentional Smoothing -------------------------------------------------
train_exp_smooth <- function(target_series){
  x <- target_series
  
  model_exp_smooth <- ets(x, model="ZZZ", damped=NULL, 
                    alpha=NULL, beta=NULL, gamma=NULL, phi=NULL, 
                    additive.only=FALSE, lambda=NULL, 
                    lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98), 
                    opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3, 
                    bounds=c("both","usual","admissible"), 
                    ic=c("aicc","aic","bic"),
                    restrict=TRUE, 
                    allow.multiplicative.trend=FALSE, 
                    use.initial.values=FALSE)
  print(summary(model_exp_smooth))
  
  return(model_exp_smooth)
}

pred_exp_smooth <- function(model, h) {
  prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, lambda=NULL)
  return_df <- data.frame("prediction" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  return(return_df)
}

