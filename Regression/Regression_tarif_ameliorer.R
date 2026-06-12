# Chargement des bibliothèques
library(dplyr)
library(forcats) # Indispensable pour regrouper le texte proprement

# ==============================================================================
# 1. PRÉPARATION DU DATASET AVEC VARIABLES CATÉGORIELLES
# ==============================================================================
df_ml <- df_clean %>%
  filter(
    !is.na(tarif_kwh_clean), 
    !is.na(puissance_nominale),
    !is.na(prise_type_combo_ccs),
    !is.na(nom_operateur),
    !is.na(implantation_station)
  ) %>%
  mutate(
    # ASTUCE PRO : On ne garde que les 10 plus gros opérateurs, le reste devient "Autres"
    operateur_groupe = fct_lump_n(as.factor(nom_operateur), n = 10, other_level = "Autres"),
    
    # On transforme l'implantation en facteur (catégorie)
    implantation = as.factor(implantation_station)
  ) %>%
  # On ajoute nos nouvelles variables dans le select
  select(tarif_kwh_clean, puissance_nominale,
         prise_type_combo_ccs, operateur_groupe, implantation)

# ==============================================================================
# 2. SÉPARATION ENTRAÎNEMENT (80%) ET VALIDATION (20%)
# ==============================================================================
set.seed(42) 
index_train <- sample(1:nrow(df_ml), size = 0.8 * nrow(df_ml))

data_train <- df_ml[index_train, ]  
data_test  <- df_ml[-index_train, ] 

# ==============================================================================
# 3. CRÉATION ET ENTRAÎNEMENT DU MODÈLE
# ==============================================================================
# On ajoute simplement les nouvelles variables dans la formule
modele_prix_complet <- lm(tarif_kwh_clean ~ puissance_nominale +  prise_type_combo_ccs + 
                            operateur_groupe + implantation, 
                          data = data_train)

# Affichage des résultats
print(summary(modele_prix_complet))

# ==============================================================================
# 4. ÉVALUATION SUR LE JEU DE VALIDATION (TEST)
# ==============================================================================
predictions <- predict(modele_prix_complet, newdata = data_test)
rmse <- sqrt(mean((data_test$tarif_kwh_clean - predictions)^2, na.rm = TRUE))

cat("Nouveau RMSE sur le jeu Test :", round(rmse, 3), "€/kWh\n")