source("BigData_groupe8/Nettoyage/main.R")

# Chargement des bibliothèques
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# ÉTAPE 1 : Préparation et filtrage (Top 15 opérateurs)
# ---------------------------------------------------------
df_etude_op <- df_clean %>%
  # On exclut les lignes où l'opérateur ou le tarif est manquant
  filter(!is.na(nom_operateur) & nom_operateur != "Non spécifié" & !is.na(tarif_kwh_clean)) %>%
  mutate(tarif_kwh_clean = as.numeric(tarif_kwh_clean))

# Identifier les 15 opérateurs ayant le plus de bornes dans notre jeu de données
top_operateurs <- df_etude_op %>%
  count(nom_operateur, sort = TRUE) %>%
  top_n(15, n) %>%
  pull(nom_operateur)

# Filtrer le dataset pour ne garder QUE ces 15 gros opérateurs
df_top_op <- df_etude_op %>%
  filter(nom_operateur %in% top_operateurs)

# ---------------------------------------------------------
# ÉTAPE 2 : Le test statistique (Kruskal-Wallis)
# ---------------------------------------------------------
test_operateur <- kruskal.test(tarif_kwh_clean ~ as.factor(nom_operateur), data = df_top_op)
print(test_operateur)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Boxplot)
# ---------------------------------------------------------
graphique_operateur <- ggplot(df_top_op, aes(
  # reorder trie les opérateurs du moins cher (en bas) au plus cher (en haut)
  x = reorder(nom_operateur, tarif_kwh_clean, FUN = median, na.rm = TRUE), 
  y = tarif_kwh_clean, 
  fill = nom_operateur
)) +
  geom_boxplot(outlier.alpha = 0.4, outlier.color = "red") + 
  
  # On pivote le graphe et on limite l'axe Y à 1.00€ pour ne pas être écrasé par des erreurs de saisie
  coord_flip(ylim = c(0, 1.0)) + 
  
  theme_minimal() +
  theme(legend.position = "none") + # On masque la légende inutile
  labs(
    title = "Politique tarifaire selon l'opérateur (Top 15)",
    subtitle = "Distribution du tarif au kWh pour les principaux réseaux français",
    x = "Nom de l'opérateur",
    y = "Tarif (€/kWh)",
    caption = "Note : Le graphique est tronqué à 1.00€/kWh pour une lisibilité optimale."
  )

print(graphique_operateur)

df_clean %>%
  filter(nom_operateur == "Bouygues Energies & Services") %>%
  select(tarif_kwh_clean) %>%
  summary()