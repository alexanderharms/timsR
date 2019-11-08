### Alle locaties van bestanden zijn relatief ten opzichte van de locatie 
### van het R-project.
# Het databestand bevat de doelreeks(en) en de regressoren.
DATAPAD <- "./data/"
DATABESTAND <- "data.csv"

# Tijdens de berekeningen worden een aantal gegevens gelogd.
# De berekende metrieken worden tussendoor weggeschreven als csv-bestanden.
# Als LOGBESTAND <- "./logs/log.txt" dan worden ze weggeschreven op 
# "./logs/log_vooruit.csv".
LOGBESTAND <- "./logs/log.txt"

# Frequentie van de tijdreeks. Voor maandelijkse data, FREQ <- 12.
FREQ <- 12

# Kolomnaam in DATABESTAND voor de te voorspellen tijdreeks.
DOELREEKS_VAR <- "Doel" 
# Kolomnamen in DATABESTAND die de regressoren aangeven.
# Voer NULL in om regressoren uit te schakelen.
REGKOLOMMEN <- c("Regressor")

# Startdatum van reeksen in het invoerbestand
# Geef hier in wat de eerste periode is die in het bestand staat.
STARTDATA <- c(1995, 1)

# Vanaf welke periode wil je het model laten doorrekenen?
# Moet altijd gelijk of later dan STARTDATA zijn
STARTMODEL <- c(1995, 1) 

# STARTTEST is het eindpunt van de eerste rolling horizon test.
STARTTEST <- c(2011, 1)

H <- 14 # Voorspelhorizon
N_TEST <- 96 # Aantal rolling horizons

# Geef hier de te testen modellen aan.
# Het script zal op zoek gaan naar de functie "train_naam" om het model te 
# trainen en "pred_naam" om te voorspellen met het model, waar 'naam' 
# de aangegeven naam in de onderstaande vector is.
MODEL_VECTOR <- c("arima19")
# R bestanden met de bijbehorende functie-definities kunnen ook in dit 
# bestand ge-'source'-d worden.
