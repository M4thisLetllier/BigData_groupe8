source("BigData_groupe8/Nettoyage/main.R")

# Chargement des bibliothèques
library(dplyr)
library(ggplot2)
library(stringr)

# ---------------------------------------------------------
# ÉTAPE 1 : Ingénierie des données (Création de la variable Département)
# ---------------------------------------------------------
df_etude_dept <- df_clean %>%
  # On exclut les lignes sans code postal ou sans tarif
  filter(!is.na(consolidated_code_postal) & !is.na(tarif_kwh_clean)) %>%
  
  mutate(
    # ASTUCE : str_pad rajoute un "0" devant si le code postal a perdu son 0 initial 
    # (ex: l'Ain "1000" redevient "01000")
    code_postal_str = str_pad(as.character(consolidated_code_postal), width = 5, pad = "0"),
    
    # On extrait les 2 premiers caractères
    departement = str_sub(code_postal_str, 1, 2),
    
    tarif_kwh_clean = as.numeric(tarif_kwh_clean)
  ) %>%
  # On garde une seule ligne par station pour la justesse statistique
  distinct(id_station_itinerance, .keep_all = TRUE)

# Pour la lisibilité du graphique, on identifie les 15 départements les plus équipés
top_departements <- df_etude_dept %>%
  count(departement, sort = TRUE) %>%
  top_n(15, n) %>%
  pull(departement)

# On filtre le dataset
df_top_dept <- df_etude_dept %>%
  filter(departement %in% top_departements)

# ---------------------------------------------------------
# ÉTAPE 2 : Le test statistique (Kruskal-Wallis)
# ---------------------------------------------------------
test_dept <- kruskal.test(tarif_kwh_clean ~ as.factor(departement), data = df_top_dept)
print(test_dept)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Boxplot)
# ---------------------------------------------------------
graphique_dept <- ggplot(df_top_dept, aes(
  # reorder trie les départements du moins cher au plus cher en médiane
  x = reorder(departement, tarif_kwh_clean, FUN = median, na.rm = TRUE), 
  y = tarif_kwh_clean, 
  fill = departement
)) +
  geom_boxplot(outlier.alpha = 0.4, outlier.color = "red") + 
  
  # Zoom pour ne pas être écrasé par les valeurs extrêmes
  coord_flip(ylim = c(0, 1.0)) + 
  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Disparités tarifaires selon le département (Top 15)",
    subtitle = "Distribution des prix au kWh dans les 15 départements les plus équipés",
    x = "Numéro de Département",
    y = "Tarif (€/kWh)",
    caption = "Note : Graphique tronqué à 1.00€/kWh pour une meilleure lisibilité."
  )

print(graphique_dept)