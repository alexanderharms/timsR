# Experimenteeropzet voor het uitvoeren van tijdreeksanalyse
Auteurs: Alexander Harms, Pauline Sluijpers

In deze map staan twee scripts: experimenteren.R en voorspellen.R. Met 
experimenteren.R kan men met tijdreeksmodellen testen met de rolling horizon-
methode. Het script voorspellen.R kan vervolgens gebruikt worden om met deze 
modellen toekomstige waardes te voorspellen. De instellingen voor de 
experimenten of de voorspellingen kunnen aangegeven worden in een apart bestand.
Een voorbeeld hiervan is gegeven in settings/settings.R. 

Voor het maken van een voorspelling behoort de te voorspellen tijdreeks in het 
databestand te lopen tot het beginpunt van de voorspelling.
Als de data van de hulpvariabele wel beschikbaar is de voorspel-
periode van de tijdreeks, laat dan de rijen voor de voorspel-datums leeg in de 
kolom van de doelreeks.

