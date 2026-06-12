#==> Gemini et Joas


# ==============================================================================
# FONCTIONNALITÉ 4 : SÉLECTION DES VARIABLES POUR L'IA (Matrice de Corrélation)
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. Connexion au pipeline de nettoyage
source("BigData_groupe8/Nettoyage/main.R")

# 2. Préparation des données pour l'algorithme de corrélation
df_cor <- df_clean %>%
<<<<<<< HEAD
  filter(!is.na(puissance_nominale), !is.na(nbre_pdc)) %>%
  mutate(
    Puissance_kW = puissance_nominale,
    Nbre_Prises = as.numeric(nbre_pdc),
    
    # --- LA VARIABLE TARIF ---
    Tarif_kWh = as.numeric(tarif_kwh_clean),
    
    # --- LES OPTIONS TARIFAIRES ---
    Est_Gratuit = ifelse(gratuit %in% c("Gratuit", "VRAI", "true", "TRUE", "1"), 1, 0),
    Paiement_CB = ifelse(paiement_cb %in% c("VRAI", "true", "TRUE", "1"), 1, 0),
    
    # --- LES TYPES DE PRISES ---
    Prise_Rapide_DC = ifelse(prise_type_combo_ccs %in% c("VRAI", "true", "TRUE", "1"), 1, 0),
    Prise_Standard_AC = ifelse(prise_type_2 %in% c("VRAI", "true", "TRUE", "1"), 1, 0),
    Prise_Domest_EF = ifelse(prise_type_ef %in% c("VRAI", "true", "TRUE", "1"), 1, 0),
    Prise_CHAdeMO = ifelse(prise_type_chademo %in% c("VRAI", "true", "TRUE", "1"), 1, 0)
  ) %>%
  # Sélection des 9 colonnes finales
  select(Puissance_kW, Nbre_Prises, Tarif_kWh, Est_Gratuit, Paiement_CB, 
         Prise_Rapide_DC, Prise_Standard_AC, Prise_Domest_EF, Prise_CHAdeMO)

# 3. Calcul mathématique de la matrice (Corrélation de Pearson)
# L'argument pairwise.complete.obs est CRUCIAL ici à cause des NA du Tarif_kWh !
matrice <- cor(df_cor, use = "pairwise.complete.obs")
=======
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

>>>>>>> 5ecf0b252a2fd24b550537a8f559a52a46c64a8b

# 4. Formatage des données pour dessiner la carte de chaleur
matrice_longue <- as.data.frame(as.table(matrice))


# ==============================================================================
# 5. Création de la Heatmap (Carte de Chaleur)
# ==============================================================================
graph_matrice <- ggplot(matrice_longue, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
<<<<<<< HEAD
  scale_fill_gradient2(low = "#e74c3c", high = "#2980b9", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Score de\nCorrélation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", fontface = "bold", size = 4) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16)) +
  labs(title = "Matrice de Corrélation : Identification des variables clés",
       subtitle = "Évaluation de l'impact des variables pour le modèle de Machine Learning")

print(graph_matrice)

# Exportation automatique de l'image
ggsave("BigData_groupe8/Image.png_pour_le_rapport/f4_matrice_correlation.png", plot = graph_matrice, width = 10, height = 8, dpi = 300)


# ==============================================================================
# F4 (Suite) : MOSAICPLOT - ANALYSE CROISÉE (Prise Rapide DC vs Paiement CB)
# ==============================================================================

# 1. Création du tableau croisé (Standard AC vs Gratuité)
tableau_mosaic <- table(df_cor$Prise_Standard_AC, df_cor$Est_Gratuit)

# 2. Renommer pour un rendu propre
rownames(tableau_mosaic) <- c("Sans Prise AC", "Avec Prise AC (Standard)")
colnames(tableau_mosaic) <- c("Payant", "Gratuit")

# Affichage de contrôle dans la console
print("--- Tableau croisé des effectifs ---")
print(tableau_mosaic)

# 3. Initialisation de la sauvegarde automatique de l'image
png("BigData_groupe8/Image.png_pour_le_rapport/mosaicplot_pertinent.png", width = 800, height = 600, res = 120)

# 4. Génération du graphique en mosaïque
mosaicplot(tableau_mosaic, 
           main = "Modèle de monétisation selon l'infrastructure", 
           xlab = "Capacité Technique", 
           ylab = "Option de Paiement",
           color = c("#bdc3c7", "#2980b9"), # Code couleur pro : Gris et Bleu
           border = "white",
           las = 1,          # Garde le texte horizontal
           cex.axis = 0.9)   # Taille de la police adaptée

# 5. Fermeture et validation de la sauvegarde
dev.off()
=======
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
>>>>>>> 5ecf0b252a2fd24b550537a8f559a52a46c64a8b
