#Mathis et Gemini

source("BigData_groupe8/Nettoyage/main.R")

# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(ggcorrplot) # Nouvelle bibliothèque pour les superbes matrices

# ---------------------------------------------------------
# ÉTAPE 1 : Préparation du jeu de données numérique
# ---------------------------------------------------------
df_numerique <- df_clean %>%
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

# ---------------------------------------------------------
# ÉTAPE 2 : Calcul de la Matrice de Corrélation
# ---------------------------------------------------------
# "pairwise.complete.obs" permet à R de faire le calcul même s'il y a quelques NA,
# en les ignorant au cas par cas lors du croisement de deux colonnes.
matrice_cor <- cor(df_numerique, use = "pairwise.complete.obs", method = "pearson")

# ---------------------------------------------------------
# ÉTAPE 3 : Visualisation (Le Corrélogramme)
# ---------------------------------------------------------
graphique_corr <- ggcorrplot(
  matrice_cor,
  hc.order = TRUE,           # Réordonne intelligemment (regroupe les variables corrélées entre elles)
  type = "lower",            # N'affiche que le triangle du bas (évite les doublons visuels)
  lab = TRUE,                # Affiche les chiffres dans les cases
  lab_size = 2.5,            # Taille du texte (à réduire si les cases se chevauchent)
  colors = c("#e74c3c", "white", "#2ecc71"), # Rouge (Inverse), Blanc (Neutre), Vert (Positive)
  title = "Matrice de corrélation de Pearson (Préparation à la Régression)",
  ggtheme = theme_minimal()
) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9)
  )

# Affichage du graphe
print(graphique_corr)