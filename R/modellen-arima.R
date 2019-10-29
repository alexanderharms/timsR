# Alle ARIMA modellen --------------------------------------------------------
train_arima <- function(doelreeks, regressor = NULL){
  x <- na.trim.ts(doelreeks)
  
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
                        xreg = regressor[1:(length(x)),], 
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
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  prediction <- model %>% 
    predict(n.ahead=h, se.fit = TRUE) 
  
  return_df <- data.frame("voorspelling" = prediction$pred,
                          "lower_conf" = prediction$pred - 1.96*prediction$se,
                          "upper_conf" = prediction$pred + 1.96*prediction$se)
  
  return(return_df)
}

# Arima model 1: non seasonal auto arima
train_arima1 <- function(doelreeks, regressoren = NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)) {
    modelarima1 <- auto.arima(x, seasonal = FALSE, allowdrift = TRUE) 
  } else {
    modelarima1 <- auto.arima(x, seasonal = FALSE, allowdrift = TRUE, 
                              xreg = regressoren)
  }
  
  print(summary(modelarima1))
  return(modelarima1)
}


pred_arima1 <- function(model, h, regressoren=NULL) {
# voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){ 
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Arima model 2: seasonal auto arima
train_arima2 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima2 <- auto.arima(x, seasonal=TRUE, allowdrift=TRUE)
  } else {
    modelarima2 <- auto.arima(x, seasonal=TRUE, allowdrift=TRUE, 
                              xreg=regressoren)
  } 
  
  print(summary(modelarima2))
  return(modelarima2)
}


pred_arima2 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){ 
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Arima model 3: seasonal model handmatig gespecificeerd
train_arima3 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  
  if (is.null(regressoren)){
    modelarima3 <- Arima(x, order = c(2, 1, 2),
                         seasonal = list(order = c(1, 1, 0), period = 12),
                         xreg = NULL, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  } else { 
    modelarima3 <- Arima(x, order = c(2, 1, 2),
                         seasonal = list(order = c(1, 1, 0), period = 12),
                         xreg = regressoren, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  }

  print(summary(modelarima3))
  return(modelarima3)
}

pred_arima3 <- function(model, h, regressoren = NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 5: handmatig gespecificeerd model met Arima() i.p.v. arima() om 
# verschil te bekijken
train_arima5 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima5 <- Arima(x, order = c(2, 1, 2),
                         seasonal = list(order = c(2, 1, 0), period = 12),
                         xreg = NULL, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS")
  } else {
    modelarima5 <- Arima(x, order = c(2, 1, 2),
                         seasonal = list(order = c(2, 1, 0), period = 12),
                         xreg = regressoren, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS")
  }
  print(summary(modelarima5))
  return(modelarima5)
}

pred_arima5 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 6: non-seasonal, handmatig gespecificeerd 
train_arima6 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima6 <- Arima(x, order = c(4, 0, 1),
                         xreg = NULL, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  } else {
    modelarima6 <- Arima(x, order = c(4, 0, 1),
                         xreg = regressoren, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  }
  print(summary(modelarima6))
  return(modelarima6)
}

pred_arima6 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 7: seasonal model, handmatig gespecificeerd 
train_arima7 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima7 <- Arima(x, order = c(3, 0, 0),
                         seasonal = list(order = c(2, 1, 0), period = 12),
                         xreg = NULL, 
                         include.mean = TRUE,
                         include.drift = TRUE,
                         method = "CSS-ML")
  } else {
    modelarima7 <- Arima(x, order = c(3, 0, 0),
                         seasonal = list(order = c(2, 1, 0), period = 12),
                         xreg = regressoren, 
                         include.mean = TRUE,
                         include.drift = TRUE,
                         method = "CSS-ML")
  }
  print(summary(modelarima7))
  return(modelarima7)
}

pred_arima7 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Seasonal model zonder drift, handmatig gespecificeerd 
train_arima8 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima8 <- Arima(x, order = c(1, 1, 3),
                         seasonal = list(order = c(0, 1, 0), period = 12),
                         xreg = NULL, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  } else {
    modelarima8 <- Arima(x, order = c(1, 1, 3),
                         seasonal = list(order = c(0, 1, 0), period = 12),
                         xreg = regressoren, 
                         include.mean = TRUE,
                         include.drift = FALSE,
                         method = "CSS-ML")
  }
  print(summary(modelarima8))
  return(modelarima8)
}

pred_arima8 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 9: seasonal model zonder drift, handmatig gespecificeerd 
train_arima9 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima9<- Arima(x, order = c(2, 1, 2),
                        seasonal = list(order = c(0, 1, 0), period = 12),
                        xreg = NULL, 
                        include.mean = TRUE,
                        include.drift = FALSE,
                        method = "CSS-ML")
  } else {
    modelarima9<- Arima(x, order = c(2, 1, 2),
                        seasonal = list(order = c(0, 1, 0), period = 12),
                        xreg = regressoren, 
                        include.mean = TRUE,
                        include.drift = FALSE,
                        method = "CSS-ML")
  }
  print(summary(modelarima9))
  return(modelarima9)
}

pred_arima9 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }  
    
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 10: seasonal model zonder drift, handmatig gespecificeerd
train_arima10 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima10 <- Arima(x, order = c(2, 1, 2),
                          seasonal = list(order = c(2, 1, 1), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  } else {
    modelarima10 <- Arima(x, order = c(2, 1, 2),
                          seasonal = list(order = c(2, 1, 1), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  }
  print(summary(modelarima10))
  return(modelarima10)
}

pred_arima10 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }  
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 11: seasonal model zonder drift, handmatig gespecificeerd 
train_arima11 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima11 <- Arima(x, order = c(2, 1, 1),
                          seasonal = list(order = c(2, 1, 1), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  } else {
    modelarima11 <- Arima(x, order = c(2, 1, 1),
                          seasonal = list(order = c(2, 1, 1), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  }
  print(summary(modelarima11))
  return(modelarima11)
}

pred_arima11 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 12: seasonal model zonder drift, handmatig gespecificeerd 
train_arima12 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  
  if (is.null(regressoren)){
    modelarima12 <- Arima(x, order = c(2, 1, 1),
                          seasonal = list(order = c(2, 1, 0), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  } else {
    modelarima12 <- Arima(x, order = c(2, 1, 1),
                          seasonal = list(order = c(2, 1, 0), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  }
  print(summary(modelarima12))
  return(modelarima12)
}

pred_arima12 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 13: Seasonal model met drift, handmatig gespecificeerd
train_arima13 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima13 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  } else {
    modelarima13 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  }
  print(summary(modelarima13))
  return(modelarima13)
}

pred_arima13 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 14: seasonal model met drift, handmatig gespecificeerd
train_arima14 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima14 <- Arima(x, order = c(2, 0, 2),
                          seasonal = list(order = c(1, 1, 2), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  } else {
    modelarima14 <- Arima(x, order = c(2, 0, 2),
                          seasonal = list(order = c(1, 1, 2), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  }
  print(summary(modelarima14))
  return(modelarima14)
}

pred_arima14 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }  
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}


# Model 15: seasonal model met drift, handmatig gespecificeerd
train_arima15 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima15 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(1, 1, 2), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  } else {
    modelarima15 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(1, 1, 2), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  }  
  print(summary(modelarima15))
  return(modelarima15)
}

pred_arima15 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }  
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 16: seasonal model met drift, handmatig gespecificeerd
train_arima16 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima16 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  } else {
    modelarima16 <- Arima(x, order = c(1, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  }
  print(summary(modelarima16))
  return(modelarima16)
}

pred_arima16 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# Model 17: seasonal model met drift, handmatig gespecificeerd
train_arima17 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima17 <- Arima(x, order = c(2, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  } else {
    modelarima17 <- Arima(x, order = c(2, 0, 1),
                          seasonal = list(order = c(0, 1, 2), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = TRUE,
                          method = "CSS-ML")
  }
  
  print(summary(modelarima17))
  return(modelarima17)
}

pred_arima17 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

train_arima18 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima18 <- Arima(x, order = c(1, 1, 1),
                          seasonal = list(order = c(0, 1, 1), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  } else {
    modelarima18 <- Arima(x, order = c(1, 1, 1),
                          seasonal = list(order = c(0, 1, 1), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  }
  print(summary(modelarima18))
  return(modelarima18)
}

pred_arima18 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

# 
train_arima19 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima19 <- Arima(x, order = c(2, 0, 1),
                          seasonal = list(order = c(0, 1, 1), period = 12),
                          xreg = NULL, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  } else {
    modelarima19 <- Arima(x, order = c(2, 0, 1),
                          seasonal = list(order = c(0, 1, 1), period = 12),
                          xreg = regressoren, 
                          include.mean = TRUE,
                          include.drift = FALSE,
                          method = "CSS-ML")
  }
  print(summary(modelarima19))
  return(modelarima19)
}

pred_arima19 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}

train_arima20 <- function(doelreeks, regressoren=NULL){
  x <- na.trim.ts(doelreeks)
  if (is.null(regressoren)){
    modelarima20 <- Arima(x, order = c(4, 1, 3),
                          xreg = NULL, 
                          include.mean = FALSE,
                          include.drift = TRUE,
                          method = "ML")
  } else {
    modelarima20 <- Arima(x, order = c(4, 1, 3),
                          xreg = regressoren, 
                          include.mean = FALSE,
                          include.drift = TRUE,
                          method = "ML")
  }
  print(summary(modelarima20))
  return(modelarima20)
}

pred_arima20 <- function(model, h, regressoren=NULL) {
  # voorspelling <- model %>% forecast(h=h) %>% .$mean
  if (is.null(regressoren)){
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL)
  } else {
    prediction <- forecast(model, h=h, level=c(80,95), fan = FALSE, 
                           lambda=NULL, xreg=regressoren)
  }  
  
  return_df <- data.frame("voorspelling" = prediction$mean,
                          "lower_conf" = prediction$lower[,2],
                          "upper_conf" = prediction$upper[,2])
  
  return(return_df)
}
