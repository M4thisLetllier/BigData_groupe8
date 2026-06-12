#==>Victor et Claude

# ==============================================================================
# F5 : RÉGRESSION LINÉAIRE MULTIPLE 
# ==============================================================================

library(dplyr)

source("Nettoyage/main.R")

# 1. Nettoyage et préparation ciblée
# On crée un sous-ensemble propre, sans NA, uniquement avec la cible et les prédicteurs forts
df_regression <- df_clean %>%
  select(Puissance_kW, Prise_Rapide_DC, Nbre_Prises, Tarif_kWh) %>%
  na.omit() # On s'assure qu'aucune donnée manquante ne fasse planter le calcul

# 2. Construction du modèle linéaire
# La formule signifie : Prédire la Puissance_kW en fonction des 3 variables clés
modele_final <- lm(Puissance_kW ~ Prise_Rapide_DC + Nbre_Prises + Tarif_kWh, 
                   data = df_regression)

# 3. Analyse des résultats
# Le summary donne la significativité (p-value) et le R-squared (performance)
print(summary(modele_final))

# 4. Préparation du tableau pour ton rapport Word
# On extrait les coefficients et p-values proprement
resultats_reg <- as.data.frame(summary(modele_final)$coefficients)
colnames(resultats_reg) <- c("Estimation", "Erreur_Std", "t_value", "p_value")

# Arrondi pour une lecture propre dans le rapport
resultats_reg <- round(resultats_reg, 4)

print("--- Résumé formaté pour votre rapport ---")
print(resultats_reg)

# 5. Calcul du R-squared (Score de précision du modèle)
r_squared <- summary(modele_final)$r.squared
cat("Le modèle explique", round(r_squared * 100, 2), "% de la variance de la puissance.\n")