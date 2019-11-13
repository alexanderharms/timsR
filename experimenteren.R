### Experimenteeropzet voor tijdreeksanalyse
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
library(KFAS)

source("./R/hulpfuncties.R")
source("./R/rol_hor_functies.R")
source("./R/modellen.R")
source("./R/modellen-arima.R")
source("./R/modellen-expsmooth.R")

if (!exists("REGKOLOMMEN")) REGKOLOMMEN <- NULL

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


# Modellen testen -------------------------------------------------------------
# Voer per model in de MODEL_VECTOR de rolling horizon uit
model_loop <- rol_hor_model_loop(doelreeks, MODEL_VECTOR, STARTTEST, N_TEST, 
                                 H, regressoren = regressoren)

# Sorteren en rapporteren -----------------------------------------------------
metrieken_maand <- model_loop$metrieken_maand
#metrieken_jaar  <- model_loop$metrieken_jaar
# Sorteer de data frames met metrieken op de RMSE waarde
metrieken_maand_sort <- metrieken_maand %>% dplyr::arrange(RMSE)
#metrieken_jaar_sort  <- metrieken_jaar %>% dplyr::arrange(RMSE)

# Schrijf de data frames met metrieken weg naar het log bestand
sink(LOGBESTAND, append = TRUE)
print("Tijdreeks, vooruit:")
print("Maand:")
print(metrieken_maand_sort)
#print("Jaar:")
#print(metrieken_jaar_sort)
print("Error modellen:")
print(model_loop$error_modellen)
sink()

vooruit_csv <- substr(LOGBESTAND, start = 1, stop = nchar(LOGBESTAND) - 4) %>%
  paste0("_vooruit.csv")
write.csv2(metrieken_maand_sort, file = vooruit_csv)

