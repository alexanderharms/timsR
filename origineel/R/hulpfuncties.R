# Inlezen data   --------------------------------------------------------------
#' Inlezen data CSV
#' 
#' Leest tijdreeksen in van CSV; geeft een dataframe terug met als kolommen
#' de doelvariabele, de hulpvariabelen en eventueel kolommen met een
#' tijdsindicatie.

inlezen_data_csv <- function(DATAPAD, DATABESTAND, csv_sep = ';') {
  if (csv_sep == ',') {
    input_data <- read.csv(paste(DATAPAD, DATABESTAND, sep = ""),
                           stringsAsFactors = FALSE)
  } else {
    input_data <- read.csv2(paste(DATAPAD, DATABESTAND, sep = ""),
                            stringsAsFactors = FALSE)
  }
  return(input_data)
}

# Prepareer tijdreeksen -------------------------------------------------------
#' Prepareer tijdreeksen
#' 
#' Leest een dataframe in met als kolommen de doelvariabele, hulpvariabelen
#' en eventueel een tijdsindicatie; geeft een list terug met een tijdreeks voor 
#' de doelvariabele genaamd 'doelreeks' en geeft een tijdreeks terug voor voor
#' de hulpvariabelen genaamd 'regressoren'.
prepareer_tijdreeksen <- function(data, STARTDATA, STARTMODEL, FREQ,
                                  DOELREEKS_VAR = NULL,
                                  REGKOLOMMEN = NULL,
                                  plot_tijdreeks = FALSE) {
  # Trimt de doelreeks en de regressoren tot een gelijk beginpunt.
  tijdreeks <- ts(data, start = STARTDATA, frequency = FREQ,
                  names = names(data)) %>%
    window(start = STARTMODEL)
  
  if (is.null(DOELREEKS_VAR)) {
    doelreeks <- NULL
    extra_doelreeksen <- NULL
  } else {
    # Het eerste item in de vector DOELREEKS_VAR wordt doelreeks, 
    # de andere items worden in extra_doelreeksen ondergebracht.
    doelreeks <- tijdreeks[, DOELREEKS_VAR[1]]
    if (length(DOELREEKS_VAR) > 1) {
      extra_doelreeksen <- tijdreeks[, DOELREEKS_VAR[2:length(DOELREEKS_VAR)]]
    } else {
      extra_doelreeksen <- NULL
    }
  }
  
  if (is.null(REGKOLOMMEN)) {
    regressoren <- NULL
  } else {
    regressoren <- tijdreeks[, REGKOLOMMEN]
  }
  
  if (plot_tijdreeks) {
    ts.plot(doelreeks, gpars = list(main = "Doelreeks"))
    
    if (!is.null(regressoren)) {
      ts.plot(regressoren, gpars = list(main = "Regressoren"))
    }
  }
  
  return(list("doelreeks" = doelreeks,
              "regressoren" = regressoren,
              "extra_doelreeksen" = extra_doelreeksen))
}

haal_ramingen_op <- function(ramingen, start_pred, eind_pred, freq, 
                             index = NULL) {
  # Vindt de laatste publicatiedatum voor start_pred
  # Alle jaren tussen start_pred en eind_pred die geraamd zijn 
  # moeten ingevuld worden met deze raming.
  # Alle jaren daarna zijn hetzelfde als het laatste jaar van de raming.
  
  # Vindt de eerste publicatiedatum voor de voorspelperiode.
  publicatie_datum <- ramingen[['Publicatiejaar']] + 
    ramingen[['Publicatiemaand']]/freq
  publicatie_datum <- publicatie_datum[max(which(publicatie_datum < start_pred))]
  publicatie_jaar <- floor(publicatie_datum)
  
  # Genereer vector van de jaren in de voorspelperiode.
  pred_raming <- seq(from = start_pred, to = eind_pred, by = 1/freq) %>%
    floor()

  # Selecteer de ramingcijfers uit de publicatie die uitgebracht is voor de 
  # voorspelperiode.
  pub_raming <- ramingen %>% filter(Publicatiejaar == publicatie_jaar)
  
  # # Voorheen hadden we procentuele veranderingen tot index-cijfers omgezet.
  # # Dit stukje code zorgt ervoor dat geraamde procentuele veranderingen 
  # # ook omgezet konden worden tot index-cijfers.
  # if (!is.null(index)) {
  #   cumprod_raming <- cumprod(1 + pub_raming$Raming/100)
  #   for (k in 1:nrow(pub_raming)) {
  #     pub_raming$Raming[k] <- index * cumprod_raming[k]
  #   }
  # }
  
  # Neem het laatste jaar waar er in het publicatiejaar een raming van is.
  max_jaar <- pub_raming$Jaar %>% max()
  
  # Ieder jaar in de voorspelperiode dat later is dan max_jaar, krijgt dezelfde
  # waarde als max_jaar, en hiermee ook dezelfde ramingswaarde als max_jaar
  pred_raming[pred_raming > max_jaar] <- max_jaar
  
  # Maak een vector aan ramingswaardes van de jaren in de voorspelperiode.
  raming <- sapply(pred_raming, function(x) {
    pub_raming %>% filter(Jaar == x) %>% pull(Raming)})
  return(raming)
}

plot_rol_hor_list <- function(rol_hor_list, plot_title,
                              zoom = TRUE, doelreeks = NULL){
  if (zoom == FALSE & is.null(doelreeks)) {
    stop("Bij zoom = FALSE moet een doelreeks gegeven worden.")
  }
  
  for (item in rol_hor_list$plot_data) {
    item_ts <- ts(item, start = item[, 'datum_vector'][1],
                  end = item[, 'datum_vector'][nrow(item)],
                  frequency = 12)
    
    if (zoom == FALSE) {
      reeks <- doelreeks
    } else {
      reeks <- item_ts[, 'actual']
    } 
    
    ts.plot(reeks, item_ts[, 'voorspelling'], 
            item_ts[, 'lower_conf'], item_ts[, 'upper_conf'], 
            gpars=list(main = plot_title,
                       col = c('black', 'red', 'blue', 'blue')))
  }
}

# Verkenfunctie voor ARIMA ----------
verken_doelreeks <- function(doelreeks, lagmax) {
  dec <- decompose(doelreeks)
  
  print(adf.test(doelreeks, alternative="stationary", k=12))
  
  acf_plot <- acf(doelreeks, lag.max=lagmax, main="ACF plot for doelreeks")
  pacf_plot <- pacf(doelreeks, lag.max=lagmax, main="PACF plot for doelreeks")
  
  return(list("decompose" = dec, "acf_plot" = acf_plot, "pacf_plot" = pacf_plot))
}

inlezen_data_cbs <- function(DATAPAD, DATABESTAND, csv_sep = ';') {
  if (csv_sep == ',') {
    input_data <- read.csv(paste(DATAPAD, DATABESTAND, sep = ""),
                           stringsAsFactors = FALSE) %>%
      cbs_add_date_column(date_type = c("Date"))
  } else {
    input_data <- read.csv2(paste(DATAPAD, DATABESTAND, sep = ""),
                            stringsAsFactors = FALSE) %>%
      select(-c(ID)) %>%
      cbs_add_date_column(date_type = c("Date"))
  }
  return(input_data)
}

terug_naar_conjunctuur <- function(reeks, h) {
  # Fit een STM op de hulpvariabele met een level, slope en cycle. 
  # Construeer een SSMcustom zonder de cycle maar met de ander componenten.
  # Voorspel de hulpvariabele in de voorspelperiode met deze SSMcustom.
  
  initial_values <- c(0.02, 0.02, 0.02, 0.02, 0.3)
  reeks <- na.omit(reeks)
  
  # Fit een STM op de hulpvariabele.
  model <- SSModel(reeks ~ SSMtrend(2, Q = list(matrix(NA), matrix(NA))) 
                   + SSMcycle(7 * 12, Q = matrix(NA)), 
                   H=matrix(NA))
  
  model <- estimate_parameters(model, initial_values)$model
  
  # Verwijder de cycle-termen uit de STM-matrices
  Z <- model$Z %>% 
    .[, , dim(.)[3]] %>% 
    .[-which(names(.) %in% c("cycle", "cycle*"))] %>%
    matrix() %>% 
    t()
  Tmat <- model$T %>% 
    .[-which(dimnames(.)[[1]] %in% c("cycle", "cycle*")), 
      -which(dimnames(.)[[2]] %in% c("cycle", "cycle*")), 1] 
  R <- model$R %>% 
    .[-which(dimnames(.)[[1]] %in% c("cycle", "cycle*")), 
      -which(dimnames(.)[[1]] %in% c("cycle", "cycle*")), 1]
  a1 <- model$a1 %>% 
    .[-which(dimnames(.)[[1]] %in% c("cycle", "cycle*")), ]
  P1 <- model$P1 %>% 
    .[-which(dimnames(.)[[1]] %in% c("cycle", "cycle*")),
      -which(dimnames(.)[[2]] %in% c("cycle", "cycle*"))]
  P1inf <- model$P1inf %>% 
    .[-which(dimnames(.)[[1]] %in% c("cycle", "cycle*")),
      -which(dimnames(.)[[2]] %in% c("cycle", "cycle*"))]
  Q <- model$Q[1:2, 1:2, 1]
  # Construeer een nieuw model met dezelfde matrices.
  model <- SSModel(reeks ~ -1 + 
                     SSMcustom(Z = Z, T = Tmat, R = R, Q = Q,
                               a1 = a1, P1 = P1, P1inf = P1inf))
  
  # Voorspel de hulpvariabele in de voorspelperiode.
  prediction <- model %>% 
    predict(n.ahead = h, filtered = FALSE, interval = "confidence", 
            level = 0.95) %>%
    .[, "fit"]
  
  return(prediction)
}