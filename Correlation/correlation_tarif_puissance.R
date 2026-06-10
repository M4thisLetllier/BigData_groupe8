#tarif_kwh_clean
#puissance_nominale

# 1. Chargement des bibliothèques
library(dplyr)
library(ggplot2)

source("BigData_groupe8/Nettoyage/main.R")
# ---------------------------------------------------------
#L'étude statistique
# ---------------------------------------------------------

# Option A : Un calcul rapide du coefficient (Pearson)
coeff_cor <- cor(df_clean$puissance_nominale, df_clean$tarif_kwh_clean)
print(paste("Coefficient de corrélation :", round(coeff_cor, 3)))

# Option B : Le test statistique complet (recommandé pour votre rapport)
# Il donne le coefficient ET la "p-value" pour prouver que le résultat n'est pas dû au hasard.
test_cor <- cor.test(df_clean$puissance_nominale, df_clean$tarif_kwh_clean)
print(test_cor)

# ---------------------------------------------------------
#La visualisation graphique (Nuage de points)
# ---------------------------------------------------------

graphique_correlation <- ggplot(df_clean, aes(x = puissance_nominale, y = tarif_kwh_clean)) +
  # Nuage de points (alpha = 0.3 rend les points un peu transparents pour voir la densité)
  geom_point(alpha = 0.3, color = "#2980b9") + 
  
  # Ajout d'une ligne de tendance (Régression Linéaire)
  geom_smooth(method = "lm", color = "#c0392b", fill = "#e74c3c", alpha = 0.2) + 
  
  # Esthétique
  theme_minimal() +
  labs(
    title = "Puissance de la borne vs Tarif au kWh",
    x = "Puissance Nominale (kW)",
    y = "Tarif (€/kWh)",
    caption = "La ligne rouge représente la tendance linéaire"
  )

# Afficher le graphique
print(graphique_correlation)

