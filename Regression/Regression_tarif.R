#Mathis et Gemini

source("Nettoyage/main.R")

# Chargement des bibliothèques
library(dplyr)

# ==============================================================================
# 1. PRÉPARATION DU DATASET POUR LE MACHINE LEARNING
# ==============================================================================
# On filtre les valeurs manquantes pour les variables de notre modèle
df_ml <- df_clean %>%
  filter(
    !is.na(tarif_kwh_clean), 
    !is.na(puissance_nominale),
    !is.na(prise_type_ef),
    !is.na(prise_type_2),
    !is.na(prise_type_combo_ccs)
  ) %>%
  # On ne garde que les colonnes utiles pour notre modèle
  select(tarif_kwh_clean, puissance_nominale, prise_type_ef, prise_type_2, prise_type_combo_ccs)

# ==============================================================================
# 2. SÉPARATION ENTRAÎNEMENT (80%) ET VALIDATION (20%)
# ==============================================================================
# set.seed permet de "figer" l'aléatoire pour que vos résultats ne changent pas à chaque clic
set.seed(42) 

# On tire au sort 80% des numéros de lignes
index_train <- sample(1:nrow(df_ml), size = 0.8 * nrow(df_ml))

# On crée les deux tableaux
data_train <- df_ml[index_train, ]  # Les 80% pour l'apprentissage
data_test  <- df_ml[-index_train, ] # Les 20% restants pour l'examen final

cat("Taille du jeu d'entraînement :", nrow(data_train), "lignes\n")
cat("Taille du jeu de validation :", nrow(data_test), "lignes\n")

# ==============================================================================
# 3. CRÉATION ET ENTRAÎNEMENT DU MODÈLE
# ==============================================================================
# "lm" signifie Linear Model. Le tilde (~) sépare la cible des variables explicatives.
modele_prix <- lm(tarif_kwh_clean ~ puissance_nominale + prise_type_ef + prise_type_2 + prise_type_combo_ccs, 
                  data = data_train)

# On affiche les résultats de l'apprentissage
print(summary(modele_prix))

# ==============================================================================
# 4. ÉVALUATION SUR LE JEU DE VALIDATION (TEST)
# ==============================================================================
# On demande au modèle de deviner les prix des 20% de bornes qu'il n'a jamais vues
predictions <- predict(modele_prix, newdata = data_test)

# On calcule le RMSE (Root Mean Squared Error) : l'erreur moyenne en euros
rmse <- sqrt(mean((data_test$tarif_kwh_clean - predictions)^2, na.rm = TRUE))

cat("\n======================================================\n")
cat("Erreur quadratique moyenne (RMSE) sur le jeu Test :", round(rmse, 3), "€/kWh\n")
cat("======================================================\n")