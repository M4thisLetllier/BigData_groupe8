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

# ---------------------------------------------------------
# ÉTAPE 4 : La visualisation (Mosaic Plot)
# ---------------------------------------------------------
df_mosaic <- df_clean %>%
  filter(
    !is.na(implantation_station) & implantation_station != "",
    !is.na(accessibilite_pmr) & accessibilite_pmr != "Accessibilité inconnue"
  ) %>%
    mutate(
      # On remplace les longues phrases par des "tags" courts
      implantation_courte = case_when(
        implantation_station == "Parking privé à usage public"       ~ "Privé (Public)",
        implantation_station == "Parking privé réservé à la clientèle" ~ "Privé (Client)",
        implantation_station == "Station dédiée à la recharge rapide"  ~ "Station Rapide",
        TRUE ~ implantation_station # Laisse "Voirie" et "Parking public" tels quels
      )
  )

# On crée le tableau de contingence avec la NOUVELLE colonne
tableau_pmr_mosaic <- table(df_mosaic$implantation_courte, df_mosaic$accessibilite_pmr)

# On augmente un peu la marge du haut (3ème chiffre) pour laisser la place aux lignes
par(mar = c(4, 12, 6, 2)) 

# On trace le mosaic plot

mosaicplot(
  tableau_pmr_mosaic,
  main = "Accessibilité PMR selon l'implantation",
  xlab = "Type d'implantation (Largeur = Volume de bornes)",
  ylab = "Statut PMR",
  color = c("#f1c40f", "#e74c3c", "#2ecc71"), 
  las = 2, 
  cex.axis = 0.8)