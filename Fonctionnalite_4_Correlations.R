# ==============================================================================
# FONCTIONNALITÉ 4 : ANALYSES BIVARIÉES ET CORRÉLATIONS
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. Chargement des données propres
donnees_irve <- read.csv("IRVE_brouillon_100k.csv", stringsAsFactors = FALSE)


# ==============================================================================
# ÉTAPE 1 : LA BOÎTE À MOUSTACHES (Boxplot) - Quantitatif vs Qualitatif
# ==============================================================================
# Objectif : Voir la répartition exacte des puissances selon le lieu (avec la médiane et les anomalies)

# On filtre les valeurs extrêmes (> 350kW) et on garde les 3 lieux principaux pour la lisibilité
df_boxplot <- donnees_irve %>%
  filter(puissance_nominale <= 350) %>%
  filter(implantation_station %in% c("Parking public", "Voirie", "Station dédiée à la recharge rapide"))

graph_boxplot <- ggplot(df_boxplot, aes(x = implantation_station, y = puissance_nominale, fill = implantation_station)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  labs(title = "Dispersion des puissances selon l'implantation",
       subtitle = "En rouge : Les valeurs atypiques (Outliers)",
       x = "Lieu d'implantation",
       y = "Puissance Nominale (kW)") +
  theme(legend.position = "none", # On cache la légende car les noms sont déjà sur l'axe X
        axis.text.x = element_text(face = "bold", size = 11))

print(graph_boxplot)
ggsave("f4_boxplot_puissance.png", plot = graph_boxplot, width = 8, height = 6, dpi = 300)


# ==============================================================================
# ÉTAPE 2 : LE TEST STATISTIQUE DU CHI-DEUX - Qualitatif vs Qualitatif
# ==============================================================================
# Attention mathématique : Le Chi-2 nécessite deux variables catégorielles (texte).
# On va donc re-créer nos catégories de puissance pour les tester face au lieu d'implantation.

df_stat <- donnees_irve %>%
  filter(implantation_station %in% c("Parking public", "Voirie", "Station dédiée à la recharge rapide")) %>%
  filter(!is.na(puissance_nominale)) %>%
  mutate(categorie_puissance = case_when(
    puissance_nominale < 22 ~ "1. Lente (<22kW)",
    puissance_nominale == 22 ~ "2. Standard (22kW)",
    puissance_nominale > 22 & puissance_nominale <= 149 ~ "3. Rapide (23-149kW)",
    puissance_nominale >= 150 ~ "4. Ultra-Rapide (150+)",
    TRUE ~ "Autre"
  )) %>%
  filter(categorie_puissance != "Autre")

# Création du Tableau de Contingence (Tableau Croisé)
tableau_croise <- table(df_stat$implantation_station, df_stat$categorie_puissance)

# Affichage du tableau dans la console pour le rapport
cat("\n--- TABLEAU CROISÉ : IMPLANTATION vs PUISSANCE ---\n")
print(tableau_croise)

# Exécution du test du Chi-2
test_chi2 <- chisq.test(tableau_croise)

cat("\n--- RÉSULTAT DU TEST DU CHI-DEUX ---\n")
print(test_chi2)


# ==============================================================================
# ÉTAPE 3 (CORRIGÉE) : LE MOSAIC PLOT (Visualisation du Chi-2)
# ==============================================================================

png("f4_mosaicplot_correlation_v2.png", width = 800, height = 600, res = 100)

# Astuce de pro : On raccourcit les noms trop longs pour éviter le chevauchement
df_stat_propre <- df_stat %>%
  mutate(implantation_station = case_when(
    implantation_station == "Station dédiée à la recharge rapide" ~ "Station dédiée",
    implantation_station == "Parking public" ~ "Parking",
    TRUE ~ implantation_station
  ))

# On recrée le tableau croisé avec les noms courts
tableau_croise_court <- table(df_stat_propre$implantation_station, df_stat_propre$categorie_puissance)

# On trace le graphique sans que les textes ne s'écrasent
mosaicplot(tableau_croise_court, 
           shade = TRUE, 
           main = "Corrélation entre Implantation et Catégorie de Puissance",
           xlab = "Lieu d'implantation",
           ylab = "Catégorie de Puissance",
           las = 1, 
           border = "white",
           cex.axis = 0.9) # Réduit très légèrement la police des axes

dev.off()