#Mathis et Gemini

# Chargement des bibliothèques
library(ggplot2)

source("BigData_groupe8/Nettoyage/main.R")

df_clean <- df %>%
  filter(!is.na(consolidated_longitude) & !is.na(consolidated_latitude)) %>%
  mutate(
    lon = as.numeric(consolidated_longitude),
    lat = as.numeric(consolidated_latitude)
  )

# =========================================================
#Heatmap Statique avec ggplot2
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