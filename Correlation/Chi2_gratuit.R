#Mathis et Gemini

source("BigData_groupe8/Nettoyage/main.R")


library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# ÉTAPE 1 : Préparation et filtrage
# ---------------------------------------------------------
df_chi2 <- df_clean %>%
  # On enlève les valeurs manquantes pour être précis
  filter(
    !is.na(implantation_station) & implantation_station != "",
    !is.na(gratuit)
  ) %>%
  # On transforme le 0/1 de la gratuité en texte clair pour le graphique
  mutate(
    gratuit_label = ifelse(gratuit == 1, "Gratuit", "Payant")
  )

# ---------------------------------------------------------
# ÉTAPE 2 : Le Test du Chi-deux
# ---------------------------------------------------------
# On crée le tableau croisé (Combien de bornes gratuites par type d'implantation ?)
tableau_contingence <- table(df_chi2$implantation_station, df_chi2$gratuit_label)

# On lance le test
test_chi2 <- chisq.test(tableau_contingence)

# Afficher les résultats
print(tableau_contingence)
print(test_chi2)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Bar chart 100%)
# ---------------------------------------------------------
# La meilleure façon de visualiser un Chi-2 est un graphique en barres empilées à 100%
graphique_chi2 <- ggplot(df_chi2, aes(x = implantation_station, fill = gratuit_label)) +
  geom_bar(position = "fill") + # "fill" met toutes les barres à 100% pour comparer les proportions
  coord_flip() +                # On tourne le graphique pour lire les textes
  theme_minimal() +
  scale_fill_manual(values = c("Gratuit" = "#2ecc71", "Payant" = "#e74c3c")) +
  scale_y_continuous(labels = scales::percent_format()) + # Met l'axe Y en pourcentages
  labs(
    title = "Proportion de bornes gratuites selon l'implantation",
    x = "Type d'implantation",
    y = "Répartition (Payant vs Gratuit)",
    fill = "Tarification"
  )

print(graphique_chi2)