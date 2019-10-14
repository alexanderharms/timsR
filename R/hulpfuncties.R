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

