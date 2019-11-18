
###############################################
# y_t   = Z_t*alpha_t + epsilon_t      met  epsilon_t ~ N(0, H_t)
# alpha_t+1= T_t*alpha_t + R_t*eta_t   met  eta_t ~ N(0, Q_t)
###############################################

# Level -----------------------------------------------------------------------
train_STM_dlevel <- function(doelreeks, regressoren = NULL) {
  # Vergelijkingen:
  # y_t = level_t + ruis_1_t
  # level_t+1 = level_t
  
  initial_values <- c(0.3)
  #modelname <- "Deterministic level model"
  modelname <- "det.  level"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=0), H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=0) + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_dlevel <- function(model, h, regressoren = NULL) {
  
 if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                        -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                   a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel <- function(doelreeks, regressoren = NULL) {
  # Vergelijkingen:
  # y_t = level_t + ruis_1_t
  # level_t+1 = level_t + ruis_2_t
  
  initial_values <- c(1.0, 0.3)
  modelname <- "stoch. level"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA))), H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA))) + regressoren, 
                     H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_slevel <- function(model, h, regressoren = NULL) {
  
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                        -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                   a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

# Level + Slope ---------------------------------------------------------------
train_STM_dlevel_dslope <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + slope_t + ruis_1_t
  # level_t+1 = level_t 
  # slope_t+1 = slope_t
  
  initial_values <- c(0.3)
  modelname <- "det. level + det. slope"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0)), H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0)) + regressoren, 
                     H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_dlevel_dslope <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  } 
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_dlevel_sslope <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + slope_t + ruis_1_t
  # level_t+1 = level_t 
  # slope_t+1 = slope_t + ruis_2_t
  
  initial_values <- c(0.02, 0.3)
  modelname <- "det. level + stoch. slope"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA))), 
                   H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA))) 
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_dlevel_sslope <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_dslope <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + slope_t + ruis_1_t
  # level_t+1 = level_t + ruis_2_t
  # slope_t+1 = slope_t
  
  initial_values <- c(1.0, 0.3)
  modelname <- "stoch. level + det. slope"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0)), 
                   H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0)) 
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_slevel_dslope <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_sslope <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + slope_t + ruis_1_t
  # level_t+1 = level_t + ruis_2_t
  # slope_t+1 = slope_t + ruis_3_t
  
  initial_values <- c(1.0, 0.02, 0.3)
  modelname <- "stoch. level + stoch. slope"
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA))),
                   H=matrix(NA))
  
  if (is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA))) 
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)
  return(model$model)
}

pred_STM_slevel_sslope <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

# Level + Seasonal ------------------------------------------------------------
train_STM_dlevel_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t 
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(0))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(0))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + ruis_2_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA)))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA)))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_dlevel_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t 
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_2_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(0))
                   + SSMseasonal(periodicity, Q=matrix(NA), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(0))
                     + SSMseasonal(periodicity, Q=matrix(NA), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_sseas <- function(model, h, regressoren = NULL) {
  
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                        -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                   a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + ruis_2_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_3_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA)))
                   + SSMseasonal(periodicity, Q=matrix(NA), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(1, Q=list(matrix(NA)))
                     + SSMseasonal(periodicity, Q=matrix(NA), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_sseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

# Level + Det. Slope + Seasonal -----------------------------------------------
train_STM_dlevel_dslope_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t
  # slope_t+1 = slope_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + det. slope + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_dslope_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_dslope_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t + ruis_2_t
  # slope_t+1 = slope_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + + det. slope + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_dslope_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_dlevel_dslope_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t
  # slope_t+1 = slope_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_2_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + det. slope + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  # initial_values <- c(0.5, 1.0)
  initial_values <- c(1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0))
                   + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, 0))
                     + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_dslope_sseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_dslope_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t + ruis_2_t
  # slope_t+1 = slope_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_3_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + det. slope + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0))
                   + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), 0))
                     + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_dslope_sseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

# Level + Stoch. Slope + Seasonal ---------------------------------------------
train_STM_dlevel_sslope_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t
  # slope_t+1 = slope_t + ruis_2_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + stoch. slope + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(0.02, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA)))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA)))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_sslope_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_sslope_dseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t + ruis_2_t
  # slope_t+1 = slope_t + ruis_3_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + + stoch. slope + det. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 0.02, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA)))
                   + SSMseasonal(periodicity, Q=diag(0, nrow = 1), 
                                 sea.type="dummy"), 
                   H=matrix(NA))
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA)))
                     + SSMseasonal(periodicity, diag(0, nrow = 1), 
                                   sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_sslope_dseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_dlevel_sslope_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t
  # slope_t+1 = slope_t + ruis_2_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_3_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "det.  level + stoch. slope + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA)))
                   + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(0, matrix(NA)))
                     + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_dlevel_sslope_sseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}

train_STM_slevel_sslope_sseas <- function(doelreeks, regressoren=NULL) {
  # Vergelijkingen:
  # y_t = level_t + seasonal_1_t + ruis_1_t
  # level_t+1 = level_t + slope_t + ruis_2_t
  # slope_t+1 = slope_t + ruis_4_t
  # seasonal_1_t+1 = -seasonal_1_t+1 ... - seasonal_11_t+1 + ruis_3_t
  # seasonal_2_t+1 = seasonal_1_t+1
  # ...
  # seasonal_11_t+1 = seasonal_10_t+1
  modelname <- "stoch. level + stoch. slope + stoch. seasonal"
  periodicity <- tsp(doelreeks)[3]
  
  # Voor initial_values: eerst Q dan H
  initial_values <- c(1.0, 0.02, 1.0, 0.3)
  doelreeks <- na.omit(doelreeks)
  
  model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA)))
                   + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy"), 
                   H=matrix(NA))
  
  if(is.null(regressoren) == FALSE) {
    model <- SSModel(doelreeks ~ SSMtrend(2, Q=list(matrix(NA), matrix(NA)))
                     + SSMseasonal(periodicity, Q=matrix(NA), sea.type="dummy")
                     + regressoren, H=matrix(NA))
  }
  
  model <- estimate_parameters(model, initial_values)

  return(model$model)
}

pred_STM_slevel_sslope_sseas <- function(model, h, regressoren = NULL) {
  if (is.null(regressoren)) {
    prediction <- model %>% 
      predict(n.ahead = h, filtered=FALSE, interval="confidence", level=0.95) 
  } else {
    # Voor de voorspelling moet een nieuw model gebouwd worden.
    # Dit wordt gedaan naar voorbeeld van het getrainde model; hiervoor
    # moeten wel alle items met de naam 'regressoren' uit de matrices 
    # verwijderd worden. 
    Z <- model$Z %>% .[, , dim(.)[3]] %>% 
      .[-which(names(.) == "regressoren")] %>%
      matrix() %>% t()
    Tmat <- model$T %>% .[-which(dimnames(.)[[1]] == "regressoren"), 
                          -which(dimnames(.)[[2]] == "regressoren"), 1] 
    R <- model$R %>% .[-which(dimnames(.)[[1]] == "regressoren"), , 1]
    a1 <- model$a1 %>% .[-which(dimnames(.)[[1]] == "regressoren"), ]
    P1 <- model$P1 %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                         -which(dimnames(.)[[2]] == "regressoren")]
    P1inf <- model$P1inf %>% .[-which(dimnames(.)[[1]] == "regressoren"),
                               -which(dimnames(.)[[2]] == "regressoren")]
    
    newdata <- SSModel(rep(NA, length(regressoren)) ~ 
                         -1 + SSMcustom(Z = Z, T = Tmat, R = R, Q = model$Q,
                                        a1 = a1, P1 = P1, P1inf = P1inf) 
                       +  regressoren)
    prediction <- model %>%
      predict(newdata = newdata, filtered=FALSE, interval="confidence",
              level = 0.95)
  }
  
  return_df <- data.frame("voorspelling" = prediction[, "fit"],
                          "lower_conf" = prediction[, "lwr"],
                          "upper_conf" = prediction[, "upr"])
  return(return_df)
}
