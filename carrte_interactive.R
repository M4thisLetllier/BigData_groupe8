library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(sf)
df <- read.csv("BigData_groupe8/IRVE.csv", stringsAsFactors = FALSE, sep = ",")

# =================================================================
# 1. Charger le contour exact de la France métropolitaine
# =================================================================
france_complete <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
#france_metro <- st_crop(france_complete, xmin = -5.5, ymin = 41, xmax = 10, ymax = 51.5)
france_metro <- france_complete
# =================================================================
# 2. Préparation basique des données
# =================================================================
df_reduit <- df %>% 
  select(nbre_pdc, consolidated_longitude, consolidated_latitude,nom_station) %>%
  filter(!is.na(consolidated_longitude) & !is.na(consolidated_latitude)) %>%
  mutate(
    lon = as.numeric(consolidated_longitude),
    lat = as.numeric(consolidated_latitude),
    nbre_pdc = as.numeric(nbre_pdc)
  )

# =================================================================
# 3. LA DÉCOUPE PARFAITE
# =================================================================
# a. On transforme votre tableau en véritable objet spatial (des points géographiques)
# Le "crs = 4326" indique à R qu'il s'agit de coordonnées GPS classiques.
stations_sf <- st_as_sf(df_reduit, coords = c("lon", "lat"), crs = 4326)

# b. On ne garde que les points qui sont strictement DANS le polygone France
stations_filtrees_sf <- st_filter(stations_sf, france_metro)

# c. On repasse en tableau classique pour l'utiliser facilement dans la heatmap
df_final <- stations_filtrees_sf %>%
  mutate(
    lon = st_coordinates(.)[,1], # Récupère la longitude
    lat = st_coordinates(.)[,2]  # Récupère la latitude
  ) %>%
  st_drop_geometry() # Enlève la surcouche spatiale devenue inutile

# =================================================================
# 4. Construction de la carte interactive
# =================================================================
carte_interactive <- leaflet(df_final) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  # Contour de la France
  addPolygons(
    data = france_metro,
    color = "#FFC0CB",         
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

# =================================================================
# 3. Construction de la carte avec Groupement (Clustering)
# =================================================================
carte_clusters <- leaflet(df_final) %>%
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