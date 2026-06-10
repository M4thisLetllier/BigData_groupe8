# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

# 1. Lecture des données
# (Assurez-vous que RStudio pointe bien vers le dossier contenant IRVE.csv)
df <- read.csv("BigData_groupe8/IRVE.csv", stringsAsFactors = FALSE, sep = ",")

# 2. Nettoyage et préparation
# On filtre les lignes sans coordonnées et on s'assure du format numérique
df_clean <- df %>%
  filter(!is.na(consolidated_longitude) & !is.na(consolidated_latitude)) %>%
  mutate(
    lon = as.numeric(consolidated_longitude),
    lat = as.numeric(consolidated_latitude),
    nbre_pdc = as.numeric(nbre_pdc)
  )

# Optionnel : Si vous voulez vous limiter à la France Métropolitaine pour 
# éviter que la carte ne soit dézoomée par les DOM-TOM, décommentez ceci :
# df_clean <- df_clean %>%
#   filter(lon > -5.5 & lon < 10 & lat > 41 & lat < 51.5)

# =========================================================
# OPTION 1 : Heatmap Statique avec ggplot2
# =========================================================

carte_statique <- ggplot(df_clean, aes(x = lon, y = lat)) +
  # Ajout de la couche de densité
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno") + # Palette de couleurs du jaune au violet foncé
  coord_quickmap() + # Conserve les proportions géographiques
  theme_minimal() +
  labs(
    title = "Densité des stations de recharge (IRVE)",
    subtitle = "Basé sur les coordonnées géographiques",
    x = "Longitude",
    y = "Latitude",
    fill = "Densité"
  )

# Afficher la carte statique
print(carte_statique)

# =========================================================
# OPTION 2 : Heatmap Interactive avec leaflet
# =========================================================

carte_interactive <- leaflet(df_clean) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # Ajoute un fond de carte (style gris clair)
  addHeatmap(
    lng = ~lon, 
    lat = ~lat, 
    intensity = ~nbre_pdc, # On pondère la chaleur par le nombre de prises (nbre_pdc)
    blur = 25,             # Flou autour des points (à ajuster)
    max = 0.05,            # Intensité maximale (à ajuster selon la densité de vos données)
    radius = 15            # Rayon d'influence d'un point
  )

# Afficher la carte interactive (s'ouvrira dans l'onglet "Viewer" de RStudio)
carte_interactive