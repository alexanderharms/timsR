# Algemene opzet code
# Tijdreeksanalyse zonder hulpvariabelen

library(dplyr)
library(zoo) #voor functie na.trim.ts
library(timeDate)  # nodig voor forecast 
library(forecast)  # nodig voor auto.arima
library(magrittr)
library(tseries)   # nodig voor stationary test (adf.test)
library(KFAS)

source("./settings/settings.R")
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
print("Tijdkolommen:")
print(TIJDKOLOMMEN)
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
                        TIJDKOLOMMEN = TIJDKOLOMMEN,
                        REGKOLOMMEN = REGKOLOMMEN)

doelreeks <- tijdreeks_list$doelreeks
extra_doelreeksen <- tijdreeks_list$extra_doelreeksen
regressoren <- tijdreeks_list$regressoren

if (!is.null(RAMINGBESTAND)) {
  ramingen <- inlezen_data_csv(DATAPAD, RAMINGBESTAND, csv_sep = ';')
} else {
  ramingen <- NULL
}

# Modellen testen -------------------------------------------------------------
# Voer per model in de MODEL_VECTOR de rolling horizon uit
model_loop <- rol_hor_model_loop_mig_saldo(doelreeks, extra_doelreeks, 
                                           MODEL_VECTOR, STARTTEST, N_TEST, H)

# Sorteren en rapporteren -----------------------------------------------------
metrieken_maand <- model_loop$metrieken_maand
metrieken_jaar  <- model_loop$metrieken_jaar
# Sorteer de data frames met metrieken op de RMSE waarde
metrieken_maand_sort <- metrieken_maand %>% dplyr::arrange(RMSE)
metrieken_jaar_sort  <- metrieken_jaar %>% dplyr::arrange(RMSE)

# Schrijf de data frames met metrieken weg naar het log bestand
sink(LOGBESTAND, append = TRUE)
print("Tijdreeks, vooruit:")
print("Maand:")
print(metrieken_maand_sort)
print("Jaar:")
print(metrieken_jaar_sort)
print("Error modellen:")
print(model_loop$error_modellen)
sink()

vooruit_csv <- substr(LOGBESTAND, start = 1, stop = nchar(LOGBESTAND) - 4) %>%
  paste0("_vooruit.csv")
write.csv2(metrieken_jaar_sort, file = vooruit_csv)

# Omgekeerde tijdreeks --------------------------------------------------------
# De tests kunnen ook op de omgekeerde tijdreeks uitgevoerd worden. 
if (INCLUDE_REV) {
  # De time series worden omgedraaid maar de start en eind datums worden 
  # hetzelfde gehouden ivm ts die niet wilt dat start > end. 
  doelreeks_rev <- ts(rev(doelreeks), start = start(doelreeks), 
                      end = end(doelreeks), frequency = FREQ)
  
  if (!is.null(regressoren)) {
    regressoren_rev <- ts(rev(regressoren), start = start(regressoren),
                          end = end(regressoren), frequency = FREQ)
  } else {
    regressoren_rev <- NULL
  }
  
  # Voer per model in MODEL_VECTOR de rolling horizon uit
  
  model_loop_rev <- rol_hor_model_loop_mig_saldo(doelreeks_rev, 
                                                 extra_doelreeks_rev, 
                                                 MODEL_VECTOR, 
                                                 STARTTEST, N_TEST, H, 
                                                 regressoren = regressoren_rev)
  
  metrieken_maand_rev <- model_loop_rev$metrieken_maand
  metrieken_jaar_rev  <- model_loop_rev$metrieken_jaar
  # Sorteer de uitkomsten
  metrieken_maand_rev_sort <- metrieken_maand_rev %>% dplyr::arrange(RMSE)
  metrieken_jaar_rev_sort  <- metrieken_jaar_rev %>% dplyr::arrange(RMSE)
  
  sink(LOGBESTAND, append = TRUE)
  print("Tijdreeks, achteruit:")
  print("Maand:")
  print(metrieken_maand_rev_sort)
  print("Jaar:")
  print(metrieken_jaar_rev_sort)
  print("Error modellen:")
  print(model_loop_rev$error_modellen)
  sink()
  
  achteruit_csv <- substr(LOGBESTAND, start = 1, 
                          stop = nchar(LOGBESTAND) - 4) %>%
    paste0("_achteruit.csv")
  write.csv2(metrieken_jaar_rev_sort, file = achteruit_csv)
  # Als er modellen zijn die errors opgeroepen hebben dan staan die niet 
  # in de data frames. Om toch gemiddelde waardes uit te kunnen rekenen
  # dan moeten ze ook uit de data frame van de tests de andere kant op
  # gehaald worden. 
  if (!is.null(model_loop_rev$error_modellen)) {
    metrieken_maand <- filter(metrieken_maand, 
                              !(model_naam %in% model_loop_rev$error_modellen))
    metrieken_jaar <- filter(metrieken_jaar, 
                             !(model_naam %in% model_loop_rev$error_modellen))
  }
  
  if (!is.null(model_loop$error_modellen)) {
    metrieken_maand_rev <- filter(metrieken_maand_rev, 
                                  !(model_naam %in% model_loop$error_modellen))
    metrieken_jaar_rev <- filter(metrieken_jaar_rev, 
                                  !(model_naam %in% model_loop$error_modellen))
  }
  
  # Bereken het gemiddelde tussen de test vooruit en de test achteruit.
  if (all(dim(metrieken_maand) == dim(metrieken_maand_rev)) &
      all(dim(metrieken_jaar) == dim(metrieken_jaar_rev))) {
    metrieken_maand_gem <- ((metrieken_maand + metrieken_maand_rev)/2) %>%
      dplyr::mutate(model_naam = metrieken_maand$model_naam) %>%
      dplyr::arrange(RMSE)
    
    metrieken_jaar_gem <- ((metrieken_jaar + metrieken_jaar_rev)/2) %>%
      dplyr::mutate(model_naam = metrieken_jaar$model_naam) %>%
      dplyr::arrange(RMSE)
    
    sink(LOGBESTAND, append=TRUE)
    print("Gemiddeld:")
    print("Maand:")
    print(metrieken_maand_gem)
    print("Jaar:")
    print(metrieken_jaar_gem)
    sink()
    
    gemiddeld_csv <- substr(LOGBESTAND, start = 1, 
                            stop = nchar(LOGBESTAND) - 4) %>%
      paste0("_gemiddeld.csv")
    write.csv2(metrieken_jaar_gem, file = gemiddeld_csv)
  }
  
}
