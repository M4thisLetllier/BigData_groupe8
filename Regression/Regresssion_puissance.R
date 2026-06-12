# ==============================================================================
# RÉGRESSION LINÉAIRE MULTIPLE : PRÉDICTION DE LA PUISSANCE NOMINALE
# ==============================================================================
library(dplyr)
library(caret)

# Étape 1 : Isolation des variables sélectionnées via la matrice (|r| >= 0.3)
df_reg_lin <- df_clean %>%
  select(
    puissance_nominale, 
    prise_type_combo_ccs, 
    prise_type_chademo, 
    cable_t2_attache, 
    paiement_cb
  )

# Étape 2 : Partitionnement des données (80% entraînement / 20% test)
set.seed(123) # Fixation du hasard pour la reproductibilité
index_lin <- createDataPartition(df_reg_lin$puissance_nominale, p = 0.8, list = FALSE)
train_lin <- df_reg_lin[index_lin, ]
test_lin  <- df_reg_lin[-index_lin, ]

# Étape 3 : Ajustement du modèle linéaire par les moindres carrés (OLS)
modele_lin <- lm(puissance_nominale ~ ., data = train_lin)

# Affichage du rapport complet du modèle (Coefficients, P-values, R-squared)
summary(modele_lin)

# Étape 4 : Évaluation des performances prédictives sur le jeu de test
predictions_lin <- predict(modele_lin, newdata = test_lin)

metrics_performance <- data.frame(
  RMSE = RMSE(predictions_lin, test_lin$puissance_nominale),
  R2   = R2(predictions_lin, test_lin$puissance_nominale)
)
print(metrics_performance)
# ==============================================================================
# VISUALISATION DE LA RÉGRESSION LINÉAIRE : OBSERVÉ VS PRÉDIT
# ==============================================================================
library(ggplot2)

# Étape 1 : Construire un jeu de données dédié à la visualisation
df_visu_lin <- data.frame(
  Reel   = test_lin$puissance_nominale,
  Predit = predictions_lin
)

# Étape 2 : Création du graphique haute définition avec légende automatique corrigée
graphique_performance <- ggplot(df_visu_lin) +
  # Points bleus avec une légère transparence
  geom_point(aes(x = Reel, y = Predit), color = "#3498db", alpha = 0.5, size = 2) +
  
  # Diagonale de référence parfaite (y = x) utilisant geom_line pour sécuriser le mapping de la légende
  geom_line(aes(x = Reel, y = Reel, color = "Prédiction parfaite (y = x)"), linetype = "dashed", size = 1) +
  
  # Ligne de tendance réelle ajustée par le modèle (formula = y ~ x rend le message silencieux)
  geom_smooth(aes(x = Reel, y = Predit, color = "Tendance réelle du modèle"), method = "lm", formula = y ~ x, se = FALSE, size = 1) +
  
  # Configuration manuelle des couleurs de la légende
  scale_color_manual(
    name = "Indicateurs",
    values = c(
      "Prédiction parfaite (y = x)" = "#e74c3c",   # Rouge
      "Tendance réelle du modèle"   = "#2ecc71"    # Vert
    )
  ) +
  
  # Force le style des lignes (pointillé vs continu) dans le carré de la légende (Désormais 2 niveaux valides)
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid")))) +
  
  # Titres et légendes soignés
  labs(
    title = "Analyse des résidus : Puissances Réelles vs Puissances Prédites",
    subtitle = "Évaluation de la régression linéaire multiple sur l'échantillon de test (20%)",
    x = "Puissance Nominale Réelle (kW)",
    y = "Puissance Nominale Prédite par le Modèle (kW)"
  ) +
  
  # Thème minimaliste professionnel
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.title    = element_text(face = "bold", color = "#2c3e50"),
    axis.text     = element_text(color = "#34495e"),
    panel.grid.major = element_line(color = "#f1f2f6"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    legend.background = element_rect(fill = "#f8fafc", color = NA)
  )

# Affichage dans la console / RStudio
print(graphique_performance)
# ==============================================================================
# EXTRACTEUR DE MÉTRIQUES ET INDICATEURS DE RÉGRESSION
# ==============================================================================
library(dplyr)
library(caret)
library(broom) # Bibliothèque indispensable pour nettoyer les résultats de modèles

# ------------------------------------------------------------------------------
# 1. TOUTES LES MÉTRIQUES GLOBALES (R², R² ajusté, Statistique F, p-value globale)
# ------------------------------------------------------------------------------
# La fonction glance() extrait la "santé" globale du modèle sous forme de tableau
performance_globale <- glance(modele_lin)

cat("\n========================================= \n")
cat("📊 ANOMALIE GLOBALE DU MODÈLE (TRAIN SET) \n")
cat("========================================= \n")
print(
  performance_globale %>% 
    select(
      `R² Classique`  = r.squared, 
      `R² Ajusté`     = adj.r.squared, 
      `Erreur Type σ` = sigma, 
      `Statistique F` = statistic, 
      `p-value F-test` = p.value
    )
)

# ------------------------------------------------------------------------------
# 2. TOUS LES INDICATEURS INDIVIDUELS (Coefficients, Erreurs types, p-values)
# ------------------------------------------------------------------------------
# La fonction tidy() transforme le tableau des coefficients en un dataframe propre
tableau_variables <- tidy(modele_lin) %>%
  mutate(
    # Ajout d'une colonne de lecture rapide pour la significativité
    Significativite = case_when(
      p.value < 0.001 ~ "*** (Très significatif)",
      p.value < 0.01  ~ "** (Significatif)",
      p.value < 0.05  ~ "* (Faiblement significatif)",
      TRUE            ~ "NS (Non Significatif - À REJETER)"
    )
  )

cat("\n========================================= \n")
cat("🎯 IMPACT ET FIABILITÉ DES VARIABLES INDIVIDUELLES \n")
cat("========================================= \n")
print(
  tableau_variables %>% 
    select(
      Variable     = term, 
      Coefficient  = estimate, 
      `Erreur Type` = std.error, 
      `p-value`    = p.value,
      Significativite
    )
)

# ------------------------------------------------------------------------------
# 3. TOUTES LES MÉTRIQUES D'ERREUR RÉELLES (RMSE, MAE sur le jeu de test)
# ------------------------------------------------------------------------------
cat("\n========================================= \n")
cat(" PERFORMANCE DE PRÉDICTION (TEST SET - 20%) \n")
cat("========================================= \n")

rmse_final <- RMSE(predictions_lin, test_lin$puissance_nominale)
mae_final  <- MAE(predictions_lin, test_lin$puissance_nominale)

cat("Marge d'erreur RMSE (Pénalise les gros écarts) :", round(rmse_final, 2), "kW\n")
cat("Marge d'erreur MAE  (Erreur absolue moyenne)   :", round(mae_final, 2), "kW\n")