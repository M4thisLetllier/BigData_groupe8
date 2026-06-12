#Mathis et Gemini

source("Nettoyage/main.R")


# Chargement des bibliothèques
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# ÉTAPE 1 : Ingénierie des données (Création des catégories)
# ---------------------------------------------------------
df_etude_horaires <- df_clean %>%
  # On exclut les valeurs manquantes pour les tarifs et les horaires
  filter(!is.na(tarif_kwh_clean) & !is.na(horaires) & horaires != "") %>%
  
  # On s'assure qu'on étudie à l'échelle de la station (pour éviter la pseudoréplication)
  # NOTE : Si vous préférez étudier le prix moyen de chaque point de charge individuel, 
  # vous pouvez enlever cette ligne.
  distinct(id_station_itinerance, .keep_all = TRUE) %>%
  
  # CRÉATION DE LA CATÉGORIE
  mutate(
    tarif_kwh_clean = as.numeric(tarif_kwh_clean),
    categorie_horaire = case_when(
      horaires == "24/7" ~ "Accès 24/7 (Toujours ouvert)",
      TRUE ~ "Horaires restreints (Journée / Jours ouvrés)"
    )
  )

# Afficher la répartition des stations dans la console
print(table(df_etude_horaires$categorie_horaire))

# ---------------------------------------------------------
# ÉTAPE 2 : Le Test Statistique (Wilcoxon / Mann-Whitney)
# ---------------------------------------------------------
# Ce test vérifie si la différence de prix entre les 2 groupes est statistiquement significative
test_horaires <- wilcox.test(tarif_kwh_clean ~ categorie_horaire, data = df_etude_horaires)
print(test_horaires)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Boxplot)
# ---------------------------------------------------------
graphique_horaires <- ggplot(df_etude_horaires, aes(
  x = categorie_horaire, 
  y = tarif_kwh_clean, 
  fill = categorie_horaire
)) +
  geom_boxplot(outlier.alpha = 0.4, outlier.color = "red") + 
  
  # On utilise coord_cartesian pour zoomer sur les prix normaux (ex: entre 0 et 1.50€) 
  # sans effacer les valeurs aberrantes du calcul mathématique.
  coord_cartesian(ylim = c(0, 1.5)) + 
  
  theme_minimal() +
  scale_fill_manual(values = c("Accès 24/7 (Toujours ouvert)" = "#2ecc71", 
                               "Horaires restreints (Journée / Jours ouvrés)" = "#e67e22")) +
  theme(legend.position = "none") +
  labs(
    title = "Impact de la disponibilité horaire sur la tarification",
    subtitle = "Comparaison des tarifs au kWh entre les stations 24/7 et celles à accès restreint",
    x = "Disponibilité horaire",
    y = "Tarif (€/kWh)",
    caption = "Graphique tronqué à 1.50€/kWh pour la lisibilité."
  )

print(graphique_horaires)