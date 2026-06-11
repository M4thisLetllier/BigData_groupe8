# ==============================================================================
# FONCTIONNALITÉ 4 : SÉLECTION DES VARIABLES POUR L'IA (Matrice de Corrélation)
# ==============================================================================

library(dplyr)
library(ggplot2)

source("BigData_groupe8/Nettoyage/main.R")
# 1. Chargement et nettoyage de survie (Watts -> kW)
# donnees_irve <- read.csv("IRVE_brouillon_100k.csv", stringsAsFactors = FALSE) %>%
#   mutate(puissance_nominale = ifelse(puissance_nominale > 1000, puissance_nominale / 1000, puissance_nominale)) %>%
#   filter(puissance_nominale <= 350 & puissance_nominale > 0)

# 2. Préparation des données pour l'algorithme de corrélation
# On convertit tout en chiffres (1 = Oui, 0 = Non) car la matrice ne lit pas le texte

df_cor <- df_clean %>%
  filter(!is.na(puissance_nominale), !is.na(nbre_pdc)) %>%
  mutate(
    Puissance_kW = puissance_nominale,
    Nbre_Prises = as.numeric(nbre_pdc),
    Est_Gratuit = as.numeric(gratuit),
    Prise_Rapide_DC = as.numeric(prise_type_combo_ccs),
    Prise_Standard_AC = as.numeric(prise_type_2)
  ) %>%
  # On sélectionne uniquement les colonnes qu'on veut comparer
  select(Puissance_kW, Nbre_Prises, Est_Gratuit, Prise_Rapide_DC, Prise_Standard_AC) %>%
  # On supprime les lignes avec des données vides pour ne pas faire planter le calcul
  na.omit()

# 3. Calcul mathématique de la matrice (Corrélation de Pearson)
matrice <- cor(df_cor)

# 4. Formatage des données pour dessiner la carte de chaleur
matrice_longue <- as.data.frame(as.table(matrice))

# 5. Création de la Heatmap (Carte de Chaleur)
graph_matrice <- ggplot(matrice_longue, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  # Code couleur : Bleu = Impact positif, Rouge = Impact négatif
  scale_fill_gradient2(low = "#e74c3c", high = "#2980b9", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Score de\nCorrélation") +
  # Affichage des scores exacts dans chaque case
  geom_text(aes(label = round(Freq, 2)), color = "black", fontface = "bold", size = 5) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16)) +
  labs(title = "Matrice de Corrélation : Identification des variables clés",
       subtitle = "Évaluation de l'impact des variables pour le modèle de Machine Learning")

print(graph_matrice)
ggsave("f4_matrice_correlation.png", plot = graph_matrice, width = 10, height = 8, dpi = 300)