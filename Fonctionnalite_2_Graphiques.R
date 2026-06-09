# Chargement des bibliothèques nécessaires pour la Fonctionnalité 2 de graphique 
library(ggplot2)
library(dplyr)

# Importation temporaire du fichier brut en attendant le nettoyage de Victor 
donnees_irve <- read.csv("IRVE.csv", sep=",", stringsAsFactors=TRUE)

# Commande pour voir le taux de valeurs manquantes par colonne (en %) a metttre dans fonction 1 
sapply(donnees_irve, function(x) sum(is.na(x) | x == "") / nrow(donnees_irve) * 100)

# Nombre de données qu'on a en tout 
dim(donnees_irve)

# Pour lister les données 
names(donnees_irve)

# --- DEBUT DES GRAPHIQUES ---

# --- 1. PARTS DE MARCHÉ DES OPÉRATEURS ---

# Création d'un sous-tableau avec le Top 10 des opérateurs. 
  top_operateurs <- donnees_irve %>%
  filter(nom_operateur != "") %>%   # Pour faire le nettoyage de la case vide 4276 en attendant les données parfaitement traités
  count(nom_operateur) %>%
  top_n(10, n) %>%
  arrange(desc(n))

# Création du graphique en barres horizontales
graph_operateurs <- ggplot(top_operateurs, aes(x = reorder(nom_operateur, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Inverse les axes pour lire les noms longs sans qu'ils se chevauchent. 
  theme_minimal() +
  labs(title = "Parts de marché : Top 10 des opérateurs",
       x = "Opérateur",
       y = "Nombre de points de charge")


# Affichage dans RStudio
print(graph_operateurs)

# Exportation obligatoire en PNG
ggsave("parts_marche_operateurs.png", plot = graph_operateurs, width = 8, height = 6, dpi = 300)



# --- 2. RÉPARTITION DES PUISSANCES ---

# Sécurité : on s'assure que la colonne est bien lue comme un format numérique
# A supprimer si les donnees nettoyer en fonctionnalités 1 sont bien traités 
  donnees_irve$puissance_nominale <- as.numeric(as.character(donnees_irve$puissance_nominale))

# Création de l'histogramme
  graph_puissance <- ggplot(donnees_irve, aes(x = puissance_nominale)) +
    
  # On limite l'axe X (ex: 0 à 350 kW) pour éviter que les valeurs aberrantes n'écrasent le graphique
  geom_histogram(fill = "darkorange", color = "black", binwidth = 22) +
  xlim(0, 350) + 
  theme_minimal() +
  labs(title = "Répartition des puissances des points de charge",
       x = "Puissance Nominale (kW)",
       y = "Fréquence")

# Affichage dans RStudio
print(graph_puissance)

# Exportation obligatoire en PNG
ggsave("repartition_puissances.png", plot = graph_puissance, width = 8, height = 6, dpi = 300)




# --- 3. ÉVOLUTION TEMPORELLE DES MISES EN SERVICE ---

# 1. Préparation et nettoyage des données temporelles
evolution_stations <- donnees_irve %>%
  
  # Sécurité : on force la conversion en texte pour éviter les erreurs de format (Facteurs)
  mutate(date_texte = as.character(date_mise_en_service)) %>%
  
  # On supprime les lignes où la date n'est pas renseignée
  filter(date_texte != "" & !is.na(date_texte)) %>%
  mutate(
    # On extrait uniquement les 10 premiers caractères (format AAAA-MM-JJ)
    date_courte = substr(date_texte, 1, 10),
    
    # On convertit ce texte au format "Date" mathématique officiel de R
    date_propre = as.Date(date_courte, format="%Y-%m-%d"),
    
    # On crée une étiquette "Année-Mois" pour pouvoir regrouper les données
    annee_mois = format(date_propre, "%Y-%m")
  ) %>%
  # Nettoyage : on garde la période pertinente (2010) et on coupe à fin mai 2026 
  # pour éviter l'effet de chute du mois en cours
  
  filter(date_propre >= as.Date("2010-01-01") & date_propre <= as.Date("2026-05-31")) %>%
  
  # On compte le nombre de nouvelles stations pour chaque mois et on trie chronologiquement
  count(annee_mois) %>%
  arrange(annee_mois)

# 2. Préparation de l'axe X pour le graphique
# R a besoin d'un jour précis pour tracer une courbe de temps. 
# On fixe arbitrairement toutes les dates au 1er du mois.
evolution_stations$date_graphique <- as.Date(paste0(evolution_stations$annee_mois, "-01"))

# 3. Création du graphique
graph_evolution <- ggplot(evolution_stations, aes(x = date_graphique, y = n)) +
  
  # Tracé de la courbe (l'argument 'linewidth' est utilisé selon les dernières normes de R)
  geom_line(color = "forestgreen", linewidth = 1) + 
  theme_minimal() +
  labs(title = "Évolution des mises en service de stations IRVE (2010 - Mai 2026)",
       x = "Année",
       y = "Nouvelles stations par mois") +
  
  #  affichage de l'année (%Y) avec un espacement de 2 ans
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  
  # Inclinaison du texte des années à 45 degrés pour éviter tout chevauchement visuel
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Affichage du résultat
print(graph_evolution)

# Exportation automatique au format PNG pour valider le livrable
ggsave("evolution_mises_en_service.png", plot = graph_evolution, width = 10, height = 6, dpi = 300)




# --- 4. RÉPARTITION DES TYPES DE PRISES ---

# 1. Création d'un tableau récapitulatif des équipements
# L'opérateur %in% permet de compter la présence d'une prise, que le fichier l'écrive "True", "true", "1" ou TRUE.
tableau_prises <- data.frame(
  Equipement = c("Type 2 (Standard)", "Prise Domestique (EF)", "Combo CCS (Rapide)", "CHAdeMO", "Autre"),
  Nombre = c(
    sum(donnees_irve$prise_type_2 %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(donnees_irve$prise_type_ef %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(donnees_irve$prise_type_combo_ccs %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(donnees_irve$prise_type_chademo %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(donnees_irve$prise_type_autre %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE)
  )
)

# 2. Création du diagramme en barres
# L'argument reorder(..., -Nombre) permet de classer les barres de la plus grande à la plus petite
graph_prises <- ggplot(tableau_prises, aes(x = reorder(Equipement, -Nombre), y = Nombre)) +
  geom_bar(stat = "identity", fill = "purple", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Répartition des types de prises sur le réseau IRVE",
       x = "Type d'équipement",
       y = "Nombre de points de charge équipés") +
  # Légère inclinaison du texte de l'axe X pour une lisibilité parfaite
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 11))

# Affichage du résultat dans RStudio
print(graph_prises)

# Exportation automatique au format PNG pour valider le livrable
ggsave("repartition_types_prises.png", plot = graph_prises, width = 8, height = 6, dpi = 300)