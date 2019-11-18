library(dplyr)
library(zoo) #voor functie na.trim.ts
library(timeDate)  # nodig voor forecast 
library(forecast)  # nodig voor auto.arima
library(magrittr)
library(tidyr)

source("./R/hulpfuncties.R")
source("./settings/settings_overledenen.R")

DATABESTAND_BEVOLKING <- "Bevolking_op_leeftijd_jaarcijfers.csv"
DATABESTAND_STERFTE <- "Sterfte_op_leeftijd_jaarcijfers.csv"

stand_jaar <- 1995

# Inlezen data en hernoemen van de kolommen
data_bevolking <- inlezen_data_csv(DATAPAD, DATABESTAND_BEVOLKING,
                                   csv_sep = ";") %>% 
  transmute(Jaar   = sapply(Perioden, function (x) substring(x, 1, 4)),
            Totaal = TotaleBevolking_1,
            N0_20  = JongerDan20Jaar_10,
            N20_40 = k_20Tot40Jaar_11,
            N40_65 = k_40Tot65Jaar_12,
            N65_80 = k_65Tot80Jaar_13,
            N80_plus = k_80JaarOfOuder_14) %>%
  mutate(Jaar = as.numeric(Jaar))

print(data_bevolking)

# Lees data in, hernoem de kolommen en groepeer de waardes zodat de 
# leeftijdsgroepen overeen komen met data_bevolking.
data_sterfte <- inlezen_data_csv(DATAPAD, DATABESTAND_STERFTE,
                                 csv_sep = ";") %>%
  select(Perioden, Leeftijd, OverledenenLeeftijdBijOverlijden_1) %>%
  transmute(Jaar = sapply(Perioden, function (x) substring(x, 1, 4)),
            Leeftijd = Leeftijd,
            Overledenen = OverledenenLeeftijdBijOverlijden_1) %>%
  mutate(Jaar = as.numeric(Jaar),
         Overledenen = as.numeric(Overledenen)) %>%
  filter(Jaar >= 1995) %>%
  spread(Leeftijd, Overledenen) %>%
  transmute(Jaar = Jaar,
            N0_20 =  rowSums(.[, 2:5]),
            N20_40 = rowSums(.[, 6:9]),
            N40_65 = rowSums(.[, 10:14]),
            N65_80 = rowSums(.[, 15:17]),
            N80_plus = rowSums(.[, 18:21]),
            Totaal = rowSums(.[, 2:21]))

print(data_sterfte)
# Lees de data in voor de overledenen om deze te kunnen standaardiseren
tijdreeks_list <- inlezen_data_csv(DATAPAD, DATABESTAND, csv_sep = ';') %>% 
  prepareer_tijdreeksen(STARTDATA, STARTMODEL, FREQ, DOELREEKS_VAR)

doelreeks <- tijdreeks_list$doelreeks

datum_vector <- seq(start(doelreeks)[1] + (start(doelreeks)[2] - 1)/FREQ, 
                    end(doelreeks)[1] + (end(doelreeks)[2] - 1)/FREQ, 
                    length.out = length(doelreeks)) 
jaar_vector <- floor(datum_vector)
maand_vector <- round((datum_vector - jaar_vector) * FREQ) + 1

# D_+s, Totaal overledenen in het standaardisatie-jaar
tot_overledenen_s <- data_sterfte %>% 
  filter(Jaar == stand_jaar) %>% 
  pull(Totaal)

# D_is, Overledenen per leeftijdsgroep in standaardisatie-jaar
overledenen_leeftijd_s <- data_sterfte %>%
  filter(Jaar == stand_jaar) %>%
  select(N0_20, N20_40, N40_65, N65_80, N80_plus)

# N_is, Bevolking per leeftijdsgroep in standaardisatie-jaar
bevolking_leeftijd_s <- data_bevolking %>%
  filter(Jaar == stand_jaar) %>%
  select(N0_20, N20_40, N40_65, N65_80, N80_plus)

# N_ij, Bevolking per leeftijdsgroep in jaar j
# Zelfde grootte als doelreeks
bevolking_leeftijd_reeks_j <- lapply(jaar_vector, function (x) {
  data_bevolking %>%
    filter(Jaar == x) %>%
    select(N0_20, N20_40, N40_65, N65_80, N80_plus)}) %>%
  bind_rows()

# Schrijf beide variabelen als matrices om de sommering als 
# inproduct te kunnen berekenen.
# Y_is, Sterftefractie per leeftijdsgroep in standaardisatie-jaar
sterftefractie_leeftijd_s <- data.matrix(overledenen_leeftijd_s / 
                                           bevolking_leeftijd_s) %>% t()
bevolking_leeftijd_reeks_j <- data.matrix(bevolking_leeftijd_reeks_j)

print(sterftecijfer_leeftijd_s)
print(bevolking_leeftijd_reeks_j)

doelreeks_stand <- doelreeks * 
  (1.0 / (bevolking_leeftijd_reeks_j %*% sterftefractie_leeftijd_s)) * 
  tot_overledenen_s

ts.plot(doelreeks_stand, doelreeks, gpars=list(col=c('black', 'red')))

stand_df <- data.frame("Jaar" = jaar_vector,
                       "Maand" = maand_vector,
                       "SterfteIndirStand" = doelreeks_stand) 
write.csv(stand_df, file = "Overledenen_IndirStand.csv", row.names = FALSE)
