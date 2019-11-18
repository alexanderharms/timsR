### Korte termijns-prognose van demografische gegevens voor 14 maanden
# Auteurs: Alexander Harms, Pauline Sluijpers

# Voor het uitvoeren van de experimenten moeten de instellingen ingesteld 
# worden. Dit bestand kan met een source-functie worden ingegeladen.

source("./settings.R")

library(dplyr)
library(zoo) 
library(timeDate)  
library(forecast)  
library(magrittr)
library(tseries)   
#library(KFAS)

source("./R/hulpfuncties.R")
source("./R/rol_hor_functies.R")
#source("./R/hulpfuncties-STM.R")
source("./R/modellen.R")
source("./R/modellen-STM.R")
source("./R/modellen-arima.R")
source("./R/modellen-expsmooth.R")

if (!exists("CONJUNCTUUR")) CONJUNCTUUR <- FALSE

if (!exists("REGKOLOMMEN")) REGKOLOMMEN <- NULL

if (!exists("RAMINGBESTAND")) RAMINGBESTAND <- NULL

if (!exists("INCLUDE_REV")) INCLUDE_REV <- FALSE

if (!exists("STARTMODEL")) STARTMODEL <- STARTDATA

if (!exists("LOGBESTAND")) LOGBESTAND <- "./log.txt"

# Schrijf de settings weg naar een logbestand 
sink(LOGBESTAND) 
print("Data-pad en -bestand:")
paste0(DATAPAD, DATABESTAND) %>% print()
print("Doelreeks:")
print(DOELREEKS_VAR)
print("Regressoren:")
print(REGKOLOMMEN)
# print("Tijdkolommen:")
# print(TIJDKOLOMMEN)
print("Start van data:")
print(STARTDATA)
print("Start van model:")
print(STARTMODEL)
print("Start van test:")
print(STARTTEST)
print("H:")
print(H)
print("N_TEST:")
print(N_TEST)
print("Model vector:")
print(MODEL_VECTOR) 
print("Ook omgekeerde tijdreeks:")
print(INCLUDE_REV)
print("Terug-naar-het-conjunctuurgemiddelde:")
print(CONJUNCTUUR)
sink()

# Inlezen en prepareren -------------------------------------------------------
# Lees de dataset in en maak van de dataset timeseries met de doelreeks,
# evt. extra_doelreeksen en regressoren.
tijdreeks_list <- inlezen_data_csv(DATAPAD, DATABESTAND, csv_sep = ';') %>% 
  prepareer_tijdreeksen(STARTDATA, STARTMODEL, FREQ, DOELREEKS_VAR, 
                        REGKOLOMMEN = REGKOLOMMEN)

doelreeks <- tijdreeks_list$doelreeks
extra_doelreeksen <- tijdreeks_list$extra_doelreeksen
regressoren <- tijdreeks_list$regressoren

if (!is.null(RAMINGBESTAND)) {
  ramingen <- inlezen_data_csv(DATAPAD, RAMINGBESTAND, csv_sep = ';')
} else {
  ramingen <- NULL
}

# Neem de naam van het model in dat gespecificeerd is in MODEL_VECTOR
model_naam <- MODEL_VECTOR[1]

# Selecteer de train en predict functie die bij het gekozen model hoort
train_fun <- paste0("train_", model_naam) %>% get()
pred_fun  <- paste0("pred_", model_naam) %>% get()

# Het model wordt getraind met de trainfunctie en met de predict functie wordt 
# een voorspelling gemaakt
if (is.null(regressoren)) {
  # Univariate tijdreeksanalyse
  print("Uni")
  getraind_model <- train_fun(doelreeks)
  prediction_df <- getraind_model %>% pred_fun(H)
} else {
  enddate <- STARTTEST[1] + (STARTTEST[2] - 1)/tsp(doelreeks)[3]
  doelreeks <- na.trim(doelreeks)
  print(str(doelreeks))
  reg_train <- window(regressoren, end = tsp(doelreeks)[2])
  
  getraind_model <- train_fun(doelreeks, reg_train)
  
  if (is.null(ramingen)) {
    if (CONJUNCTUUR) {
      # Geen ramingen, wel terug-naar-het-conjunctuurgemiddelde
      reg_pred <- terug_naar_conjunctuur(reg_train, H)
    } else {
      # Historische data
      # Geen ramingen, geen terug-naar-het-conjunctuurgemiddelde.
      reg_pred <- window(regressoren, 
                         start = tsp(doelreeks)[2] + 1/tsp(doelreeks)[3])
    }
  } else {
    # Wel ramingen
    start_pred <- enddate - (H - 1)/tsp(doelreeks)[3]
    end_pred <- enddate
    reg_pred <- haal_ramingen_op(ramingen, start_pred, end_pred, tsp(doelreeks)[3])
  }
  prediction_df <- getraind_model %>% pred_fun(H, reg_pred)
}

voorspelling <- prediction_df$voorspelling

# Plot met rood de voorspelling achter de geobserveerde tijdreeks
ts.plot(doelreeks, voorspelling, gpars=list(col = c("black", "red")))

# Bereken het jaarcijfer 
voorspeld_jaarcijfer <- tail(voorspelling, 12) %>% sum()

# Maak een dataframe met het jaar, maand en bijbehorende voorspelling per jaar 
# en print deze
datum_vector <- seq(end(doelreeks)[1] + (end(doelreeks)[2])/FREQ, 
                    end(doelreeks)[1] + (end(doelreeks)[2] + H - 1)/FREQ, 
                    length.out = H)

jaar_vec <- floor(datum_vector) 
maand_vec <- round((datum_vector - floor(datum_vector)) * FREQ) + 1
voorspelling_df <- data.frame("Jaar" = jaar_vec,
                              "Maand" = maand_vec,
                              "Voorspelling" = as.numeric(voorspelling))
sink(LOGBESTAND, append = TRUE)
print(voorspelling_df)
sink()

# Opslaan van het dataframe met de gemaakte voorspelling
voorspel_csv <- substr(LOGBESTAND, start = 1, stop = nchar(LOGBESTAND) - 4) %>%
  paste0("_voorspelling.csv")
write.csv2(voorspelling_df, file = voorspel_csv, row.names = FALSE)

