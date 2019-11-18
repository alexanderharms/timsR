library(dplyr)
library(zoo) #voor functie na.trim.ts
library(timeDate)  # nodig voor forecast 
library(forecast)  # nodig voor auto.arima
library(magrittr)
library(tidyr)

source("./R/hulpfuncties.R")
source("./settings/settings_geboortes.R")

DATABESTAND_BEVOLKING <- "Bevolking_op_leeftijd_jaarcijfers.csv"
DATABESTAND_GEBOORTES <- "37744ned_Geboortes_leeftijd_moeder.csv"

stand_jaar <- 1995
# Inlezen data en hernoemen van de kolommen
data_bevolking <- inlezen_data_csv(DATAPAD, DATABESTAND_BEVOLKING,
                                   csv_sep = ";") %>%
  transmute(Jaar = sapply(Perioden, function (x) substring(x, 1, 4)),
            Totaal         = TotaleBevolking_1,
            AandeelVrouwen = Vrouwen_3/Totaal,
            N0_20          = JongerDan20Jaar_10,
            N20_40         = k_20Tot40Jaar_11,
            N40_plus       = k_40Tot65Jaar_12 + k_65Tot80Jaar_13 +
              k_80JaarOfOuder_14) %>%
  mutate(Jaar = as.numeric(Jaar)) %>%
  mutate(TotaalVrouwen = AandeelVrouwen * Totaal,
         N0_20    = AandeelVrouwen * N0_20,
         N20_40   = AandeelVrouwen * N20_40,
         N40_plus = AandeelVrouwen * N40_plus)
  
# Lees data in, hernoem de kolommen en groepeer de waardes zodat de 
# leeftijdsgroepen overeen komen met data_bevolking.
data_geboortes <- inlezen_data_csv(DATAPAD, DATABESTAND_GEBOORTES,
                                   csv_sep = ";") %>%
  select(Perioden, LeeftijdVanDeMoeder,
         LevendgebLeeftijdMoederOp3112_1) %>%
  transmute(Jaar = sapply(Perioden, function (x) substring(x, 1, 4)),
            Leeftijd  = LeeftijdVanDeMoeder,
            Geboortes = LevendgebLeeftijdMoederOp3112_1) %>%
  mutate(Jaar = as.numeric(Jaar),
         Geboortes = as.numeric(Geboortes)) %>%
  filter(Jaar >= 1995) %>%
  spread(Leeftijd, Geboortes) %>%
  transmute(Jaar = Jaar,
            N0_20 =  .[, 2],
            N20_40 = rowSums(.[, 3:6]),
            N40_plus = rowSums(.[, 7:8]),
            Totaal = rowSums(.[, 2:8]))
print(data_geboortes)
  
# Lees de data in voor de overledenen om deze te kunnen standaardiseren
tijdreeks_list <- inlezen_data_csv(DATAPAD, DATABESTAND, csv_sep = ';') %>% 
  prepareer_tijdreeksen(STARTDATA, STARTMODEL, FREQ, DOELREEKS_VAR)

doelreeks <- tijdreeks_list$doelreeks
# Standaardiseren van het geboortecijfer

datum_vector <- seq(start(doelreeks)[1] + (start(doelreeks)[2] - 1)/FREQ, 
                    end(doelreeks)[1] + (end(doelreeks)[2] - 1)/FREQ, 
                    length.out = length(doelreeks)) 
jaar_vector <- floor(datum_vector)
maand_vector <- round((datum_vector - jaar_vector) * FREQ) + 1

# G_+s, Totaal geboortes in standaardisatie-jaar
tot_geboortes_s <- data_geboortes %>% 
  filter(Jaar == stand_jaar) %>% 
  pull(Totaal)

# G_is, Geboortes per leeftijdsgroep in standaardisatie-jaar
geboortes_leeftijd_s <- data_geboortes %>%
  filter(Jaar == stand_jaar) %>%
  select(N0_20, N20_40, N40_plus)

# M_is, Aantal vrouwen per leeftijdsgroep in standaardisatie-jaar
bevolking_leeftijd_s <- data_bevolking %>%
  filter(Jaar == stand_jaar) %>%
  select(N0_20, N20_40, N40_plus)

# M_ij, Aantal vrouwen per leeftijdsgroep in jaar j
bevolking_leeftijd_reeks_j <- lapply(jaar_vector, function (x) {
  data_bevolking %>%
    filter(Jaar == x) %>%
    select(N0_20, N20_40, N40_plus)}) %>%
  bind_rows()

# Schrijf beide variabelen als matrices om de sommering als 
# inproduct te kunnen berekenen.
# V_is, Vruchtbaarheidscijfer per leeftijdsgroep in standaardisatie-jaar
vbcijfer_leeftijd_s <- data.matrix(geboortes_leeftijd_s / 
                                     bevolking_leeftijd_s) %>% t()
bevolking_leeftijd_reeks_j <- data.matrix(bevolking_leeftijd_reeks_j)

doelreeks_stand <- doelreeks * 
  1.0 / (bevolking_leeftijd_reeks_j %*% vbcijfer_leeftijd_s) * 
  tot_geboortes_s

ts.plot(doelreeks_stand, doelreeks, gpars=list(col=c('black', 'red')))

stand_df <- data.frame("Jaar" = jaar_vector,
                       "Maand" = maand_vector,
                       "GeboortesIndirStand" = doelreeks_stand) 
write.csv(stand_df, file = "Geboortes_IndirStand.csv", row.names = FALSE)