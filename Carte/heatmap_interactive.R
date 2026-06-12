#===> Mathis et Gemini

library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(sf)

source("BigData_groupe8/Nettoyage/main.R")

#On récupère le coutour de la France
france_complete <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_metro <- france_complete

carte_interactive <- leaflet(df_clean) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  # Contour de la France
  addPolygons(
    data = france_metro,
    color = "#4A4A4A",         
    weight = 2,                
    opacity = 0.8,             
    fillColor = "transparent"
  ) %>%
  
  # Heatmap (Désormais strictement confinée aux frontières !)
  addHeatmap(
    lng = ~lon, 
    lat = ~lat, 
    intensity = ~nbre_pdc, 
    blur = 28,             
    max = 0.05,            
    radius = 15            
  )

# Affichage
carte_interactive
