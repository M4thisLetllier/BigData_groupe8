source("BigData_groupe8/Nettoyage/main.R")


# Chargement des bibliothèques
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# ÉTAPE 1 : Préparation et filtrage (Exclusion des inconnus)
# ---------------------------------------------------------
df_chi2_pmr <- df_clean %>%
  # On exclut les valeurs vides et les statuts inconnus pour avoir un test fiable
  filter(
    !is.na(implantation_station) & implantation_station != "",
    !is.na(accessibilite_pmr) & accessibilite_pmr != "Accessibilité inconnue"
  )

# ---------------------------------------------------------
# ÉTAPE 2 : Le Test du Chi-deux
# ---------------------------------------------------------
# Création du tableau croisé
tableau_pmr <- table(df_chi2_pmr$implantation_station, df_chi2_pmr$accessibilite_pmr)

# Lancement du test statistique
test_chi2_pmr <- chisq.test(tableau_pmr)

# Affichage des résultats dans la console
print(tableau_pmr)
print(test_chi2_pmr)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Bar chart 100%)
# ---------------------------------------------------------
graphique_pmr <- ggplot(df_chi2_pmr, aes(x = implantation_station, fill = accessibilite_pmr)) +
  geom_bar(position = "fill") + 
  coord_flip() +                
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  
  # On choisit des couleurs logiques (Rouge = Non, Vert/Bleu = Oui)
  scale_fill_manual(values = c(
    "Non accessible" = "#e74c3c", 
    "Accessible non réservé" = "#f1c40f", 
    "Réservé PMR" = "#2ecc71"
  )) +
  labs(
    title = "Accessibilité PMR selon le lieu d'implantation",
    subtitle = "Répartition en pourcentages (hors données inconnues)",
    x = "Type d'implantation",
    y = "Proportion",
    fill = "Statut PMR"
  )

print(graphique_pmr)