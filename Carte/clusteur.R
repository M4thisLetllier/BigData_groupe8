library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(sf)

source("BigData_groupe8/Nettoyage/main.R")

#On récupère le coutour de la France
france_complete <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_metro <- france_complete


carte_clusters <- leaflet(df_clean) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  # Contour de la France
  addPolygons(
    data = france_metro,
    color = "#4A4A4A",         
    weight = 1.5,                
    opacity = 0.5,             
    fillColor = "transparent"
  ) %>%
  
  # Ajout des marqueurs avec l'option de regroupement
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    radius = 6,                # Taille des points individuels (une fois zoomé)
    stroke = FALSE,            # Pas de bordure pour les points individuels
    fillOpacity = 0.8,
    
    # Texte qui s'affiche lorsqu'on clique sur un point individuel
    popup = ~paste0("<strong>", nom_station, "</strong><br>Points de charge : ", nbre_pdc),
    
    #LA LIGNE MAGIQUE : Active le regroupement automatique
    clusterOptions = markerClusterOptions() 
  )

# Affichage de la carte
carte_clusters