#==> Gemini et Joas


# ==============================================================================
# FONCTIONNALITÉ 4 : SÉLECTION DES VARIABLES POUR L'IA (Matrice de Corrélation)
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. Connexion au pipeline de nettoyage (Source Unique de Vérité)
# On s'assure d'utiliser exactement la même base (df_clean) que la F2
source("BigData_groupe8/Nettoyage/main.R")

# 2. Préparation des données pour l'algorithme de corrélation
# On convertit tout en chiffres (1 = Oui, 0 = Non) car la matrice ne lit pas le texte
df_cor <- df_clean %>%
  # On garde UNIQUEMENT les colonnes qui sont des nombres (ou des 0/1)
  select(where(is.numeric)) %>%
  
  # On supprime toutes les coordonnées GPS (lon, lat) comme demandé
  select(
    -starts_with("lon"), 
    -starts_with("lat"),
    -starts_with("consolidated_lon"), 
    -starts_with("consolidated_lat"),
    -contains("code_postal"), # On enlève le CP si jamais il a été converti en nombre
    -contains("insee")        # Pareil pour le code insee
  )


# 3. Calcul mathématique de la matrice (Corrélation de Pearson)
# ASTUCE 1 : 'use = "pairwise.complete.obs"' force R à ignorer les cases vides 
# plutôt que de faire planter tout le calcul de la colonne.
matrice <- cor(df_cor, use = "pairwise.complete.obs")


# 4. Formatage des données pour dessiner la carte de chaleur
matrice_longue <- as.data.frame(as.table(matrice))


# ==============================================================================
# 5. Création de la Heatmap (Carte de Chaleur)
# ==============================================================================
graph_matrice <- ggplot(matrice_longue, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient2(
    low = "#e74c3c", high = "#2980b9", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Score de\nCorrélation",
    na.value = "grey90" # ASTUCE 2 : Colorie en gris clair les cases impossibles à calculer (NA)
  ) +
  # ASTUCE 3 : na.rm = TRUE empêche l'apparition du message d'erreur rouge dans la console
  geom_text(aes(label = round(Freq, 2)), color = "black", fontface = "bold", size = 5, na.rm = TRUE) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face="bold"),
    axis.text.y = element_text(face="bold"),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  ) +
  labs(
    title = "Matrice de Corrélation : Identification des variables clés",
    subtitle = "Évaluation de l'impact des variables pour le modèle de Machine Learning"
  )

print(graph_matrice)