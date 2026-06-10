# ==============================================================================
# FONCTIONNALITÉ 2 : VISUALISATION GRAPHIQUE 
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. Chargement de la base
#df_clean <- read.csv("IRVE_brouillon_100k.csv", stringsAsFactors = FALSE)
source("BigData_groupe8/Nettoyage/main.R")

# ------------------------------------------------------------------------------
# RUSTINE DE NETTOYAGE : Correction du piège des Watts vs KiloWatts
# Objectif : Ramener les volumes à l'échelle de la France entière
# ------------------------------------------------------------------------------
# df_clean <- df_clean %>%
#   mutate(puissance_nominale = ifelse(puissance_nominale > 1000, puissance_nominale / 1000, puissance_nominale)) %>%
#   filter(puissance_nominale <= 350 & puissance_nominale > 0)

# ==============================================================================
# GRAPHIQUE 1 : PARTS DE MARCHÉ (Méthode de l'Annotation Explicite)
# ==============================================================================

# 1. On calcule d'abord qui sont les 10 plus gros opérateurs
top_operateurs <- df_clean %>%
  filter(!is.na(nom_operateur) & nom_operateur != "") %>% 
  group_by(nom_operateur) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  pull(nom_operateur)

# 2. On prépare les données pour ces 10 opérateurs
df_top10 <- df_clean %>%
  filter(nom_operateur %in% top_operateurs) %>%
  mutate(gratuit = ifelse(gratuit == 1, "Gratuit", "Payant")) %>%
  group_by(nom_operateur, gratuit) %>%
  summarise(total = n(), .groups = 'drop')

# 3. On génère le graphique
graph_operateurs <- ggplot(df_top10, aes(x = reorder(nom_operateur, total, sum), y = total, fill = gratuit)) +
  # Retour aux barres empilées pures, sans bordure blanche parasite
  geom_bar(stat = "identity", width = 0.6) +
  # Affichage des gros chiffres blancs à l'intérieur des barres
  geom_text(aes(label = ifelse(total > 100, format(total, big.mark = " "), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  
  # L'ARME SECRÈTE : L'annotation manuelle qui pointe directement sur le bout de la barre
  annotate("text", x = "TotalEnergies Marketing France", y = 3450, 
           label = "← 17 gratuites", color = "#00e676", fontface = "bold", size = 4.5, hjust = 0) +
  
  coord_flip() + 
  # Un vert hyper saturé (#00e676) pour survivre au vidéoprojecteur
  scale_fill_manual(values = c("Gratuit" = "#00e676", "Payant" = "#2980b9")) + 
  theme_minimal(base_size = 13) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  labs(title = "Top 10 des opérateurs du réseau public",
       subtitle = "Répartition du parc selon le modèle économique",
       x = "Opérateurs", y = "Nombre total de points de charge", fill = "Modèle :") +
  theme(legend.position = "top", 
        axis.text.y = element_text(face = "bold", color = "#2c3e50"),
        plot.title = element_text(face = "bold"))

print(graph_operateurs)
ggsave("parts_marche_operateurs_finale.png", plot = graph_operateurs, width = 10, height = 7, dpi = 300)


# ==============================================================================
# GRAPHIQUE 2 : RÉPARTITION DES PUISSANCES (Axes ultra-explicites)
# ==============================================================================
df_puissance_agg <- df_clean %>%
  filter(implantation_station %in% c("Parking public", "Voirie", "Station dédiée à la recharge rapide")) %>%
  filter(!is.na(puissance_nominale)) %>%
  mutate(categorie_puissance = case_when(
    puissance_nominale < 22 ~ "1. Lente (< 22 kW)",
    puissance_nominale >= 22 & puissance_nominale <= 24 ~ "2. Standard (22 kW)",
    puissance_nominale > 24 & puissance_nominale <= 149 ~ "3. Rapide (50-120 kW)",
    puissance_nominale >= 150 ~ "4. Ultra-Rapide (150+ kW)",
    TRUE ~ "Autre"
  )) %>%
  filter(categorie_puissance != "Autre") %>%
  group_by(categorie_puissance, implantation_station) %>%
  summarise(nombre = n(), .groups = 'drop')

graph_puissance <- ggplot(df_puissance_agg, aes(x = categorie_puissance, y = nombre, fill = implantation_station)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "white", width = 0.8) +
  geom_text(aes(label = format(nombre, big.mark = " ")), 
            position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 3.5, color = "#2c3e50") +
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  # LA CORRECTION : Des titres d'axes impossibles à confondre
  labs(title = "Analyse de la puissance selon le lieu d'implantation",
       subtitle = "Les catégories absorbent les variations d'encodage (ex: 22.08 kW reclassé en 22 kW)",
       x = "Puissance Nominale des bornes (en KiloWatts)", 
       y = "Nombre de bornes (Quantité installée)", 
       fill = "Type d'implantation :") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

print(graph_puissance)
ggsave("repartition_puissances_finale.png", plot = graph_puissance, width = 11, height = 7, dpi = 300)
# ==============================================================================
# GRAPHIQUE 3 : ÉVOLUTION TEMPORELLE
# ==============================================================================
evolution_stations <- df_clean %>%
  filter(!is.na(date_mise_en_service) & date_mise_en_service != "") %>%
  mutate(date_propre = as.Date(substr(date_mise_en_service, 1, 10), format="%Y-%m-%d")) %>%
  filter(date_propre >= as.Date("2015-01-01") & date_propre <= as.Date("2026-05-31")) %>%
  mutate(annee_mois = format(date_propre, "%Y-%m")) %>%
  count(annee_mois) %>%
  mutate(date_graphique = as.Date(paste0(annee_mois, "-01")))

graph_evolution <- ggplot(evolution_stations, aes(x = date_graphique, y = n)) +
  geom_area(fill = "#bdc3c7", alpha = 0.4) + 
  geom_line(color = "#7f8c8d", alpha = 0.5) + 
  geom_smooth(method = "loess", span = 0.1, color = "#c0392b", linewidth = 1.2, se = FALSE) + 
  theme_minimal(base_size = 12) +
  labs(title = "Dynamique de déploiement du réseau IRVE (2015 - 2026)",
       subtitle = "En rouge : Courbe de tendance (Moyenne mobile des mises en service)",
       x = NULL, y = "Nouvelles stations par mois") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(face = "bold"))

print(graph_evolution)
ggsave("evolution_mises_en_service_finale.png", plot = graph_evolution, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 4 : RÉPARTITION DES TYPES DE PRISES 
# ==============================================================================
tableau_prises <- data.frame(
  Equipement = c("Type 2 (Standard AC)", "Combo CCS (Rapide DC)", "Prise Domestique", "CHAdeMO", "Autre"),
  Nombre = c(
    sum(df_clean$prise_type_2 == 1, na.rm = TRUE),
    sum(df_clean$prise_type_combo_ccs == 1, na.rm = TRUE),
    sum(df_clean$prise_type_ef == 1, na.rm = TRUE),
    sum(df_clean$prise_type_chademo == 1, na.rm = TRUE),
    sum(df_clean$prise_type_autre == 1, na.rm = TRUE)
  )
)

graph_prises <- ggplot(tableau_prises, aes(x = reorder(Equipement, -Nombre), y = Nombre)) +
  geom_bar(stat = "identity", fill = "#34495e", width = 0.6) + 
  geom_text(aes(label = format(Nombre, big.mark = " ")), vjust = -1, size = 4, fontface = "bold", color = "#2c3e50") +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(title = "Équipement technologique du réseau",
       x = NULL, y = "Volume de prises installées") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

print(graph_prises)
ggsave("repartition_types_prises_finale.png", plot = graph_prises, width = 8, height = 6, dpi = 300)
# Chargement des bibliothèques nécessaires pour la Fonctionnalité 2 de graphique 
library(ggplot2)
library(dplyr)

# Importation temporaire du fichier brut en attendant le nettoyage de Victor 
df_clean <- read.csv("IRVE.csv", sep=",", stringsAsFactors=TRUE)

# Commande pour voir le taux de valeurs manquantes par colonne (en %) a metttre dans fonction 1 
sapply(df_clean, function(x) sum(is.na(x) | x == "") / nrow(df_clean) * 100)

# Nombre de données qu'on a en tout 
dim(df_clean)

# Pour lister les données 
names(df_clean)

# --- DEBUT DES GRAPHIQUES ---

# --- 1. PARTS DE MARCHÉ DES OPÉRATEURS ---

# Création d'un sous-tableau avec le Top 10 des opérateurs. 
  top_operateurs <- df_clean %>%
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
  df_clean$puissance_nominale <- as.numeric(as.character(df_clean$puissance_nominale))

# Création de l'histogramme
  graph_puissance <- ggplot(df_clean, aes(x = puissance_nominale)) +
    
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
evolution_stations <- df_clean %>%
  
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
    sum(df_clean$prise_type_2 %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(df_clean$prise_type_ef %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(df_clean$prise_type_combo_ccs %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(df_clean$prise_type_chademo %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE),
    sum(df_clean$prise_type_autre %in% c("True", "TRUE", "true", 1, TRUE), na.rm = TRUE)
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


