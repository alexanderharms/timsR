rol_hor_model_loop <- function(doelreeks, MODEL_VECTOR, STARTTEST, 
                               N_TEST, H, 
                               regressoren = NULL,
                               ramingen = NULL,
                               conjunctuur = FALSE) {
  
  # Initialiseer de data frames voor de metrieken
  metrieken_maand <- data.frame("Modelnaam" = c(), 
                                "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                                "MPE" = c(), "MAPE" = c())
  metrieken_jaar <- data.frame("Modelnaam" = c(), 
                               "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                               "MPE" = c(), "MAPE" = c())
  error_modellen <- c()
  
  # Loop over de modellen in de MODEL_VECTOR
  for (model_naam in MODEL_VECTOR) {
    print(paste0("Model: ", model_naam))
    
    # Voor ieder model moet een train- en een predictie-functie 
    # gedefinieerd zijn. Deze beginnen respectievelijk met 'train_' en 'pred_'.
    train_fun <- paste0("train_", model_naam)
    pred_fun  <- paste0("pred_", model_naam)
    
    metrieken_list <- NULL
    
    # Als modellen niet convergeren tijdens het trainen, dan worden ze
    # toegevoegd aan de vector error_modellen. De loop kan dan doorgaan.
    metrieken_list <- tryCatch({
      # Voer de rolling horizon uit.
      # In rol_hor_list zitten de metrieken_maand, metrieken_jaar, plot_data
      # en modellen.
      if (is.null(regressoren)) {
        rol_hor_list <- rol_hor_geenreg(doelreeks, FUN = train_fun, 
                                        FUN_PRED = pred_fun,
                                        start_eval = STARTTEST, 
                                        eval_periods = N_TEST,
                                        h = H)
      } else {
        if (conjunctuur) {
          rol_hor_list <- rol_hor_conj(doelreeks, FUN = train_fun, 
                                       FUN_PRED = pred_fun,
                                       start_eval = STARTTEST, 
                                       eval_periods = N_TEST,
                                       h = H, regressoren = regressoren)
        } else {
          rol_hor_list <- rol_hor_reg(doelreeks, FUN = train_fun, 
                                      FUN_PRED = pred_fun,
                                      start_eval = STARTTEST, 
                                      eval_periods = N_TEST,
                                      h = H, regressoren = regressoren,
                                      ramingen = ramingen)
        }
      }
      
      plot_rol_hor_list(rol_hor_list, model_naam, zoom = TRUE, 
                        doelreeks = doelreeks)
      
      # De metrieken worden als factors opgeslagen in de data frames.
      # Om te kunnen sorteren moeten deze eerst naar character omgezet worden
      # en daarna naar numeric.
      metrieken_maand <- rbind(metrieken_maand, 
                               cbind(model_naam, 
                                     rol_hor_list$metrieken_maand),
                               make.row.names = FALSE) %>%
        mutate(ME = ME %>% as.character() %>% as.numeric(),
               RMSE = RMSE %>% as.character() %>% as.numeric(),
               MAE = MAE %>% as.character() %>% as.numeric(),
               MPE = MPE %>% as.character() %>% as.numeric(),
               MAPE = MAPE %>% as.character() %>% as.numeric())
      
      metrieken_jaar <- rbind(metrieken_jaar, 
                              cbind(model_naam, 
                                    rol_hor_list$metrieken_jaar),
                              make.row.names = FALSE) %>%
        mutate(ME = ME %>% as.character() %>% as.numeric(),
               RMSE = RMSE %>% as.character() %>% as.numeric(),
               MAE = MAE %>% as.character() %>% as.numeric(),
               MPE = MPE %>% as.character() %>% as.numeric(),
               MAPE = MAPE %>% as.character() %>% as.numeric())
      
      metrieken_list <- list("metrieken_maand" = metrieken_maand,
                             "metrieken_jaar" = metrieken_jaar,
                             "error_modellen" = error_modellen)
    },
    # Als de rolling horizon een error oplevert, voeg dan de
    # modelnaam toe aan de vector error_modellen.
    error = function(cond) {
      error_modellen <- c(error_modellen, model_naam)
      print(cond) # Print de error
      return(list("metrieken_maand" = metrieken_maand,
                  "metrieken_jaar" = metrieken_jaar,
                  "error_modellen" = error_modellen))
    })
    
    if (!is.null(metrieken_list)) {
      metrieken_maand <- metrieken_list$metrieken_maand
      metrieken_jaar  <- metrieken_list$metrieken_jaar
      error_modellen  <- metrieken_list$error_modellen
    }
    
  }
  
  return(list("metrieken_jaar"  = metrieken_jaar,
              "metrieken_maand" = metrieken_maand,
              "error_modellen"  = error_modellen))
}

#' Rolling horizon, geen regressoren
#' 
#' Test tijdreeksmodellen door middel van een rolling horizon methode. 
#' 
#'  @param reeks Tijdreeks van de doelvariabele.
#'  @param FUN Functie waarin het tijdreeksmodel getrained wordt. Deze functie 
#'    hoort een model-object uit te voeren.
#'  @param FUN_PRED Functie die op basis van het getrainde tijdreeksmodel een
#'    voorspelling kan doen.
#'  @param start_eval Vector van de vorm c(jaar, periode) die het laatste punt 
#'    van de eerste rolling horizon vormt.
#'  @param eval_periods Aantal rolling horizons. De 
#'    standaardwaarde is 4.
#'  @param h Horizon lengte. Standaardwaarde is 14.   
rol_hor_geenreg <- function(reeks, FUN, FUN_PRED, start_eval, 
                            eval_periods = 4, h = 14) {
  # Als FUN en FUN_PRED strings zijn, dan worden de bijbehorende 
  # train en predictie-functie erbij gezocht.
  if (is.character(FUN)) FUN <- get(FUN)
  if (is.character(FUN_PRED)) FUN_PRED <- get(FUN_PRED)
  
  nowcasts_month <- data.frame("actual" = c(), 
                               "voorspelling" = c())
  
  nowcasts_year <- data.frame("actual_year" = c(),
                              "voorspelling_year" = c())
  
  plot_data <- list()
  getrainde_modellen <- list()
  for (i in 0:(eval_periods - 1)) {
    # Start_eval[2] - 1 + i zorgt ervoor dat de start_eval-datum als eerste
    # meetwaarde meegenomen wordt als i = 0.
    # tsp(reeks)[3] is de frequentie van de reeks
    enddate <- start_eval[1] + (start_eval[2] - 1 + i)/tsp(reeks)[3]
    reeks_nieuw <- window(reeks, end = enddate)
    actual <- reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)]
    reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)] <- NA
    
    # Train het model.
    getraind_model <- FUN(reeks_nieuw)
    # Het getrainde model wordt opgeslagen in een list om ze later te kunnen
    # onderzoeken.
    getrainde_modellen[[i+1]] <- getraind_model
    
    # FUN_PRED geeft een data frame terug met de voorspelling en de 95 procent
    # betrouwbaarheidsintervallen.
    return_df <- getraind_model %>% FUN_PRED(h)
    voorspelling <- return_df$voorspelling
    
    # De voorspellingen gaan per maand.
    month_actual_pred <- cbind(actual, voorspelling)
    # print(month_actual_pred)
    
    # De laatste 12 maanden van de voorspelling worden opgeteld tot een 
    # jaarcijfer.
    actual_year <- sum(tail(actual, 12))
    voorspelling_year <- sum(tail(voorspelling, 12))
    year_actual_pred <- cbind(actual_year, voorspelling_year)
    # print(year_actual_pred)
    
    nowcasts_month <- rbind(nowcasts_month, month_actual_pred)
    nowcasts_year <- rbind(nowcasts_year, year_actual_pred)
    
    # Geef plot_data tijdsinformatie mee om later als tijdreeks te plotten
    datum_vector <- enddate +
      seq(-(h - 1), 0, length.out = h)/tsp(reeks)[3]
    plot_data[[i+1]] <- cbind(actual, return_df, datum_vector)
  }
  
  # Bereken de metrieken op basis van de echte waarde en de voorspelling.
  metrieken_maand <- accuracy(nowcasts_month[, 'actual'], 
                              nowcasts_month[, 'voorspelling'])
  metrieken_jaar <- accuracy(nowcasts_year[, 'actual_year'], 
                             nowcasts_year[,'voorspelling_year'])
  
  return(list("metrieken_maand" = metrieken_maand,
              "metrieken_jaar"= metrieken_jaar,
              "plot_data" = plot_data,
              "modellen" = getrainde_modellen))
}

#' Rolling horizon, met regressoren
#' 
#' Test tijdreeksmodellen door middel van een rolling horizon methode. 
#' 
#'  @param reeks Tijdreeks van de doelvariabele.
#'  @param FUN Functie waarin het tijdreeksmodel getrained wordt. Deze functie 
#'    hoort een model-object uit te voeren.
#'  @param FUN_PRED Functie die op basis van het getrainde tijdreeksmodel een
#'    voorspelling kan doen.
#'  @param start_eval Vector van de vorm c(jaar, periode) die het laatste punt 
#'    van de eerste rolling horizon vormt.
#'  @param eval_periods Aantal rolling horizons. De 
#'    standaardwaarde is 4.
#'  @param h Horizon lengte. Standaardwaarde is 14. 
#'  @param regressoren Tijdreeks van de hulpvariabelen.
#'  @param ramingen Dataframe vanuit het ramingbestand.
#'  
rol_hor_reg <- function(reeks, FUN, FUN_PRED, start_eval, 
                        eval_periods = 4, h = 14, regressoren = NULL,
                        ramingen = NULL) {
  # Als FUN en FUN_PRED strings zijn, dan worden de bijbehorende 
  # train en predictie-functie erbij gezocht.
  if (is.character(FUN)) FUN <- get(FUN)
  if (is.character(FUN_PRED)) FUN_PRED <- get(FUN_PRED)
  
  nowcasts_month <- data.frame("actual" = c(), 
                               "voorspelling" = c())
  
  nowcasts_year <- data.frame("actual_year" = c(),
                              "voorspelling_year" = c())
  
  plot_data <- list()
  getrainde_modellen <- list()
  for (i in 0:(eval_periods - 1)) {
    # Start_eval[2] - 1 + i zorgt ervoor dat de start_eval-datum als eerste
    # meetwaarde meegenomen wordt als i = 0.
    # tsp(reeks)[3] is de frequentie van de reeks
    enddate <- start_eval[1] + (start_eval[2] - 1 + i)/tsp(reeks)[3]
    reeks_nieuw <- window(reeks, end = enddate)
    actual <- reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)]
    reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)] <- NA
    
    reg_train <- window(regressoren, end = (enddate - h/tsp(reeks)[3]))
    
    if (is.null(ramingen)) {
      reg_pred <- window(regressoren, start = (enddate - (h - 1)/tsp(reeks)[3]),
                          end = enddate)
    } else {
      start_pred <- enddate - (h - 1)/tsp(reeks)[3]
      end_pred <- enddate
      reg_pred <- haal_ramingen_op(ramingen, start_pred, end_pred, tsp(reeks)[3])
                                   # index = reg_train[length(reg_train)])
    }

    # Train het model.
    getraind_model <- FUN(reeks_nieuw, regressoren = reg_train)
    # Het getrainde model wordt opgeslagen in een list om ze later te kunnen
    # onderzoeken.
    getrainde_modellen[[i+1]] <- getraind_model
    
    # FUN_PRED geeft een data frame terug met de voorspelling en de 95 procent
    # betrouwbaarheidsintervallen.
    return_df <- getraind_model %>% FUN_PRED(h, regressoren = reg_pred)
    voorspelling <- return_df$voorspelling
    
    # De voorspellingen gaan per maand.
    month_actual_pred <- cbind(actual, voorspelling)
    # print(month_actual_pred)
    
    # De laatste 12 maanden van de voorspelling worden opgeteld tot een 
    # jaarcijfer.
    actual_year <- sum(tail(actual, 12))
    voorspelling_year <- sum(tail(voorspelling, 12))
    year_actual_pred <- cbind(actual_year, voorspelling_year)
    # print(year_actual_pred)
    
    nowcasts_month <- rbind(nowcasts_month, month_actual_pred)
    nowcasts_year <- rbind(nowcasts_year, year_actual_pred)
    
    # Geef plot_data tijdsinformatie mee om later als tijdreeks te plotten
    datum_vector <- enddate +
      seq(-(h - 1), 0, length.out = h)/tsp(reeks)[3]
    plot_data[[i+1]] <- cbind(actual, return_df, datum_vector)
  }
  
  # Bereken de metrieken op basis van de echte waarde en de voorspelling.
  metrieken_maand <- accuracy(nowcasts_month[, 'actual'], 
                              nowcasts_month[, 'voorspelling'])
  metrieken_jaar <- accuracy(nowcasts_year[, 'actual_year'], 
                             nowcasts_year[,'voorspelling_year'])
  
  return(list("metrieken_maand" = metrieken_maand,
              "metrieken_jaar"= metrieken_jaar,
              "plot_data" = plot_data,
              "modellen" = getrainde_modellen))
}


#' Rolling horizon, met regressoren i.c.m. de terug-naar-het-conjunctuurgem.
#' 
#' Test tijdreeksmodellen door middel van een rolling horizon methode. 
#' 
#'  @param reeks Tijdreeks van de doelvariabele.
#'  @param FUN Functie waarin het tijdreeksmodel getrained wordt. Deze functie 
#'    hoort een model-object uit te voeren.
#'  @param FUN_PRED Functie die op basis van het getrainde tijdreeksmodel een
#'    voorspelling kan doen.
#'  @param start_eval Vector van de vorm c(jaar, periode) die het laatste punt 
#'    van de eerste rolling horizon vormt.
#'  @param eval_periods Aantal rolling horizons. De 
#'    standaardwaarde is 4.
#'  @param h Horizon lengte. Standaardwaarde is 14. 
#'  @param regressoren Tijdreeks van de hulpvariabelen.
#'  
rol_hor_conj <- function(reeks, FUN, FUN_PRED, start_eval, 
                         eval_periods = 4, h = 14, regressoren = NULL) {
  # Als FUN en FUN_PRED strings zijn, dan worden de bijbehorende 
  # train en predictie-functie erbij gezocht.
  if (is.character(FUN)) FUN <- get(FUN)
  if (is.character(FUN_PRED)) FUN_PRED <- get(FUN_PRED)
  
  nowcasts_month <- data.frame("actual" = c(), 
                               "voorspelling" = c())
  
  nowcasts_year <- data.frame("actual_year" = c(),
                              "voorspelling_year" = c())
  
  plot_data <- list()
  getrainde_modellen <- list()
  for (i in 0:(eval_periods - 1)) {
    # Start_eval[2] - 1 + i zorgt ervoor dat de start_eval-datum als eerste
    # meetwaarde meegenomen wordt als i = 0.
    # tsp(reeks)[3] is de frequentie van de reeks
    enddate <- start_eval[1] + (start_eval[2] - 1 + i)/tsp(reeks)[3]
    reeks_nieuw <- window(reeks, end=enddate)
    actual <- reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)]
    reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)] <- NA
    
    reg_train <- window(regressoren, end = (enddate - h/tsp(reeks)[3]))
    reg_pred <- terug_naar_conjunctuur(reg_train, h)
    
    # Train het model.
    getraind_model <- FUN(reeks_nieuw, regressoren = reg_train)
    # Het getrainde model wordt opgeslagen in een list om ze later te kunnen
    # onderzoeken.
    getrainde_modellen[[i+1]] <- getraind_model
    
    # FUN_PRED geeft een data frame terug met de voorspelling en de 95 procent
    # betrouwbaarheidsintervallen.
    return_df <- getraind_model %>% FUN_PRED(h, regressoren = reg_pred)
    voorspelling <- return_df$voorspelling
    
    # De voorspellingen gaan per maand.
    month_actual_pred <- cbind(actual, voorspelling)
    # print(month_actual_pred)
    
    # De laatste 12 maanden van de voorspelling worden opgeteld tot een 
    # jaarcijfer.
    actual_year <- sum(tail(actual, 12))
    voorspelling_year <- sum(tail(voorspelling, 12))
    year_actual_pred <- cbind(actual_year, voorspelling_year)
    # print(year_actual_pred)
    
    nowcasts_month <- rbind(nowcasts_month, month_actual_pred)
    nowcasts_year <- rbind(nowcasts_year, year_actual_pred)
    
    # Geef plot_data tijdsinformatie mee om later als tijdreeks te plotten
    datum_vector <- enddate +
      seq(-(h - 1), 0, length.out = h)/tsp(reeks)[3]
    plot_data[[i+1]] <- cbind(actual, return_df, datum_vector)
  }
  
  # Bereken de metrieken op basis van de echte waarde en de voorspelling.
  metrieken_maand <- accuracy(nowcasts_month[, 'actual'], 
                              nowcasts_month[, 'voorspelling'])
  metrieken_jaar <- accuracy(nowcasts_year[, 'actual_year'], 
                             nowcasts_year[,'voorspelling_year'])
  
  return(list("metrieken_maand" = metrieken_maand,
              "metrieken_jaar"= metrieken_jaar,
              "plot_data" = plot_data,
              "modellen" = getrainde_modellen))
}


# Migratiesaldo ---------------------------------------------------------------
rol_hor_model_loop_mig_saldo <- function(doelreeks, extra_doelreeks, 
                                         MODEL_VECTOR, STARTTEST, N_TEST, H) { 
  # Initialiseer de data frames voor de metrieken
  metrieken_maand <- data.frame("Modelnaam" = c(), 
                                "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                                "MPE" = c(), "MAPE" = c())
  metrieken_jaar <- data.frame("Modelnaam" = c(), 
                               "ME" = c(), "RMSE" = c(), "MAE" = c(), 
                               "MPE" = c(), "MAPE" = c())
  error_modellen <- c()
  
  # Loop over de modellen in de MODEL_VECTOR
  for (model_naam in MODEL_VECTOR) {
    print(paste0("Model: ", model_naam))
     
    # Voor ieder model moet een train- en een predictie-functie 
    # gedefinieerd zijn. Deze beginnen respectievelijk met 'train_' en 'pred_'.
    train_fun <- paste0("train_", model_naam)
    pred_fun  <- paste0("pred_", model_naam)
    
    metrieken_list <- NULL
    # Sommige modellen geven errors tijdens het fitten op de data. Om toch de 
    # andere tests door te kunnen laten gaan is hier een tryCatch statement 
    # ingebouwd.
    metrieken_list <- tryCatch({
      # Voer de rolling horizon uit.
      # In rol_hor_list zitten de metrieken_maand, metrieken_jaar, plot_data
      # en modellen.
      rol_hor_list <- rol_hor_geenreg_migratiesaldo(doelreeks, 
                                                    extra_doelreeks, 
                                                    FUN = train_fun, 
                                                    FUN_PRED = pred_fun,
                                                    start_eval = STARTTEST, 
                                                    eval_periods = N_TEST,
                                                    h = H)
      
      # De metrieken worden als factors opgeslagen in de data frames.
      # Om te kunnen sorteren moeten deze eerst naar character omgezet worden
      # en daarna naar numeric.
      metrieken_maand <- rbind(metrieken_maand, 
                               cbind(model_naam, 
                                     rol_hor_list$metrieken_maand),
                               make.row.names = FALSE) %>%
        mutate(ME = ME %>% as.character() %>% as.numeric(),
               RMSE = RMSE %>% as.character() %>% as.numeric(),
               MAE = MAE %>% as.character() %>% as.numeric(),
               MPE = MPE %>% as.character() %>% as.numeric(),
               MAPE = MAPE %>% as.character() %>% as.numeric())
      
      metrieken_jaar <- rbind(metrieken_jaar, 
                              cbind(model_naam, 
                                    rol_hor_list$metrieken_jaar),
                              make.row.names = FALSE) %>%
        mutate(ME = ME %>% as.character() %>% as.numeric(),
               RMSE = RMSE %>% as.character() %>% as.numeric(),
               MAE = MAE %>% as.character() %>% as.numeric(),
               MPE = MPE %>% as.character() %>% as.numeric(),
               MAPE = MAPE %>% as.character() %>% as.numeric())
      
      metrieken_list <- list("metrieken_maand" = metrieken_maand,
                             "metrieken_jaar" = metrieken_jaar,
                             "error_modellen" = error_modellen)
    },
    # Als de rolling horizon een error oplevert, voeg dan de
    # modelnaam toe aan de vector error_modellen.
      error = function(cond) {
        error_modellen <- c(error_modellen, model_naam)
        print(cond) # Print de error
        return(list("metrieken_maand" = metrieken_maand,
                    "metrieken_jaar" = metrieken_jaar,
                    "error_modellen" = error_modellen))
    })
    
    if (!is.null(metrieken_list)) {
      metrieken_maand <- metrieken_list$metrieken_maand
      metrieken_jaar  <- metrieken_list$metrieken_jaar
      error_modellen  <- metrieken_list$error_modellen
    }
  }
  
  return(list("metrieken_jaar"  = metrieken_jaar,
              "metrieken_maand" = metrieken_maand,
              "error_modellen"  = error_modellen))
}

rol_hor_geenreg_migratiesaldo <- function(reeks, extra_reeks, FUN, FUN_PRED,
                                          start_eval, eval_periods = 4, 
                                          h = 14) {
  # Als FUN en FUN_PRED strings zijn, dan worden de bijbehorende 
  # train en predictie-functie erbij gezocht.
  if (is.character(FUN)) FUN <- get(FUN)
  if (is.character(FUN_PRED)) FUN_PRED <- get(FUN_PRED)
  
  nowcasts_month <- data.frame("actual" = c(), 
                               "voorspelling" = c())
  
  nowcasts_year <- data.frame("actual_year" = c(),
                              "voorspelling_year" = c())
  
  plot_data <- list()
  getrainde_modellen <- list()
  for (i in 0:(eval_periods - 1)) {
    # Start_eval[2] - 1 + i zorgt ervoor dat de start_eval-datum als eerste
    # meetwaarde meegenomen wordt als i = 0.
    # tsp(reeks)[3] is de frequentie van de reeks
    enddate <- start_eval[1] + (start_eval[2] - 1 + i)/tsp(reeks)[3]
    reeks_nieuw <- window(reeks, end = enddate)
    actual <- reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)]
    reeks_nieuw[(length(reeks_nieuw) - h + 1):length(reeks_nieuw)] <- NA
    
    extra_reeks_nieuw <- window(extra_reeks, end = enddate)
    extra_actual <- extra_reeks_nieuw[(length(extra_reeks_nieuw) - h + 1):
                                        length(extra_reeks_nieuw)]
    extra_reeks_nieuw[(length(extra_reeks_nieuw) - h + 1):
                        length(extra_reeks_nieuw)] <- NA
    
    # Train het model.
    getraind_model <- FUN(reeks_nieuw)
    extra_getraind_model <- FUN(extra_reeks_nieuw)
    # Het getrainde model wordt opgeslagen in een list om ze later te kunnen
    # onderzoeken.
    getrainde_modellen[[i+1]] <- getraind_model
    
    # FUN_PRED geeft een data frame terug met de voorspelling en de 95 procent
    # betrouwbaarheidsintervallen.
    return_df <- getraind_model %>% FUN_PRED(h)
    extra_return_df <- extra_getraind_model %>% FUN_PRED(h)
    voorspelling <- return_df$voorspelling
    extra_voorspelling <- extra_return_df$voorspelling
    
    # Haal voorspelling (immigratie) en extra_voorspelling (emigratie) van 
    # elkaar af om het migratiesaldo te krijgen
    actual <- actual - extra_actual
    voorspelling <- voorspelling - extra_voorspelling
    
    # De voorspellingen gaan per maand.
    month_actual_pred <- cbind(actual, voorspelling)
    # print(month_actual_pred)
    
    # De laatste 12 maanden van de voorspelling worden opgeteld tot een 
    # jaarcijfer.
    actual_year <- sum(tail(actual, 12))
    voorspelling_year <- sum(tail(voorspelling, 12))
    year_actual_pred <- cbind(actual_year, voorspelling_year)
    # print(year_actual_pred)
    
    nowcasts_month <- rbind(nowcasts_month, month_actual_pred)
    nowcasts_year <- rbind(nowcasts_year, year_actual_pred)
    
    # Geef plot_data tijdsinformatie mee om later als tijdreeks te plotten
    datum_vector <- enddate +
      seq(-(h - 1), 0, length.out = h)/tsp(reeks)[3]
    plot_data[[i+1]] <- cbind(actual, return_df, datum_vector)
  }
  
  # Bereken de metrieken op basis van de echte waarde en de voorspelling.
  metrieken_maand <- accuracy(nowcasts_month[, 'actual'], 
                              nowcasts_month[, 'voorspelling'])
  metrieken_jaar <- accuracy(nowcasts_year[, 'actual_year'], 
                             nowcasts_year[,'voorspelling_year'])
  
  return(list("metrieken_maand" = metrieken_maand,
              "metrieken_jaar"= metrieken_jaar,
              "plot_data" = plot_data,
              "modellen" = getrainde_modellen))
}