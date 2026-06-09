# ==============================================================================
# 1. CHARGEMENT DES PACKAGES ET DES DONNÉES
# ==============================================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(rnaturalearth)
library(sf)
# Chargement du jeu de données IRVE
df <- read.csv("IRVE.csv")


# ==============================================================================
# 2. EXPLORATION GLOBALE (STRUCTURE ET TYPES)
# ==============================================================================

# Dimensions du tableau (Renvoie le nombre de lignes et colonnes)
dim(df) 

# Structure et types de données
glimpse(df)

# Aperçu des premières et dernières lignes
head(df, 5)
tail(df, 5)

# Résumé statistique (Min, Max, Moyenne, Médiane, Quartiles)
summary(df$puissance_nominale)
summary(df$nbre_pdc)


# ==============================================================================
# 3. DIAGNOSTIC DES VALEURS MANQUANTES
# ==============================================================================

# 1. Remplacer tous les textes vides "" par des vrais NA
df[df == ""] <- NA

# 2. Compter le nombre de valeurs manquantes pour CHAQUE colonne
manquants_par_colonne <- colSums(is.na(df))

# Création du tableau de synthèse (Nombre et Pourcentage)
bilan_manquants <- data.frame(
  Nb_Manquants = colSums(is.na(df)),
  Pourcentage  = round((colSums(is.na(df)) / nrow(df)) * 100, 2)
)

# Affichage des résultats dans la console
print(bilan_manquants)
print(manquants_par_colonne)


# ==============================================================================
# 4. NETTOYAGE ET SUPPRESSION DES COLONNES INUTILES
# ==============================================================================
# Si une colonne essentielle comme 'puissance_nominale' a des NA, on supprime ces lignes
df_clean <- df %>% filter(!is.na(puissance_nominale))
# Suppression des colonnes très vides ou inutiles pour le consommateur
df_clean$observations    <- NULL # 75% vierge
df_clean$tarification    <- NULL # 75% vierge
df_clean$siren_amenageur <- NULL # Inutile pour un consommateur et 40% manquant
df_clean$coordonneesXY   <- NULL # Doublon de coordonnées dans un format pas intéressant

# Suppression des métadonnées de la plateforme data.gouv
df_clean <- df_clean %>% select(-c(
  datagouv_dataset_id, 
  datagouv_resource_id, 
  datagouv_organization_or_owner, 
  created_at, 
  last_modified, 
  date_maj
))


# ==============================================================================
# 5. IMPUTATIONS ET CORRECTIONS DES DONNÉES
# ==============================================================================

# Si contact aménageur vide on y injecte contact operateur 
df_clean <- df_clean %>% 
  mutate(nom_enseigne = ifelse(is.na(contact_amenageur) | contact_amenageur == "", 
                               contact_operateur, 
                               contact_amenageur))



# Pour les colonnes textuelles, on remplace les NA restants par "Non spécifié"
df_clean$nom_amenageur[is.na(df_clean$nom_amenageur)] <- "Non spécifié"
df_clean$nom_operateur[is.na(df_clean$nom_operateur)] <- "Non spécifié"


# ==============================================================================
# 6. EXTRACTION ET RECONSTRUCTION GÉOGRAPHIQUE
# ==============================================================================

df_clean <- df_clean %>%
  mutate(
    # Étape 1 : Extraire le code postal (les 5 chiffres consécutifs)
    cp_extrait = str_extract(adresse_station, "\\d{5}"),
    
    # Étape 2 : Extraire la commune (tout ce qui se trouve APRÈS les 5 chiffres du code postal)
    commune_extraite = str_trim(str_extract(adresse_station, "(?<=\\d{5}\\s).+")),
    
    # Étape 3 : Convertir le code postal extrait en ENTIER (<int>) pour correspondre à votre colonne
    cp_extrait = as.integer(cp_extrait),
    
    # Étape 4 : Remplir consolidated_code_postal si vide (NA ou "")
    consolidated_code_postal = ifelse(is.na(consolidated_code_postal) | consolidated_code_postal == "", 
                                      cp_extrait, 
                                      consolidated_code_postal),
    
    # Étape 5 : Remplir consolidated_commune si vide (NA ou "")
    consolidated_commune = ifelse(is.na(consolidated_commune) | consolidated_commune == "", 
                                  commune_extraite, 
                                  consolidated_commune),
    
    # ÉTAPE 5.5 : Nettoyer l'adresse pour ne garder QUE la rue
    
    adresse_station = str_remove(adresse_station, "\\s?\\d{5}.*")
  ) %>% 
  # Étape 6 : Supprimer les deux colonnes temporaires qui ont servi à l'extraction
  select(-c(cp_extrait, commune_extraite))
# ==============================================================================
# 7. Reconstruction des Booléens
# ==============================================================================
bool_cols <- c("prise_type_ef","prise_type_2","prise_type_combo_ccs",
               "prise_type_chademo","prise_type_autre","gratuit",
               "paiement_acte","paiement_cb","reservation","station_deux_roues")

df_clean <- df_clean %>%
  mutate(across(all_of(bool_cols), ~as.integer(tolower(as.character(.)) %in% c("true","1"))))

# ==============================================================================
# 8. Reconstruction des encodage corrompu
# ==============================================================================
df_clean <- df_clean %>%
  mutate(condition_acces = case_when(
    stri_detect_fixed(stri_trans_general(condition_acces,"Latin-ASCII"), "libre")  ~ "Accès libre",
    stri_detect_fixed(stri_trans_general(condition_acces,"Latin-ASCII"), "reserv") ~ "Accès réservé",
    TRUE ~ condition_acces
  ))
df_clean <- df_clean %>%
  mutate(accessibilite_pmr = case_when(
    str_detect(accessibilite_pmr, regex("non r.serv", ignore_case = TRUE))  ~ "Accessible non réservé",
    str_detect(accessibilite_pmr, regex("r.serv",     ignore_case = TRUE))  ~ "Réservé PMR",
    str_detect(accessibilite_pmr, regex("non access", ignore_case = TRUE))  ~ "Non accessible",
    TRUE                                                                     ~ "Accessibilité inconnue"
  ))

# ==============================================================================
# 10. Enleve les valeurs abberante et passe les W en Kw
# ==============================================================================

df_clean <- df_clean %>%
  mutate(
    puissance_nominale = ifelse(puissance_nominale > 350, puissance_nominale / 1000, puissance_nominale),
    puissance_nominale = ifelse(puissance_nominale > 350, 350, puissance_nominale)
  )
# =================================================================
# 11. Charger le contour exact de la France métropolitaine
# =================================================================
france_complete <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_metro <- st_crop(france_complete, xmin = -5.5, ymin = 41, xmax = 10, ymax = 51.5)

# =================================================================
# 3. LA DÉCOUPE PARFAITE
# =================================================================
# a. On transforme votre tableau en véritable objet spatial (des points géographiques)
# Le "crs = 4326" indique à R qu'il s'agit de coordonnées GPS classiques.
stations_sf <- st_as_sf(df_clean, coords = c("lon", "lat"), crs = 4326)

# b. On ne garde que les points qui sont strictement DANS le polygone France
stations_filtrees_sf <- st_filter(stations_sf, france_metro)

# c. On repasse en tableau classique pour l'utiliser facilement dans la heatmap
df_clean <- stations_filtrees_sf %>%
  mutate(
    lon = st_coordinates(.)[,1], # Récupère la longitude
    lat = st_coordinates(.)[,2]  # Récupère la latitude
  ) %>%
  st_drop_geometry() # Enlève la surcouche spatiale devenue inutile
write.csv(df_clean, "IRVE_clean.csv", row.names = FALSE)
dim(df_clean)
glimpse(df_clean)
summary(df_clean$puissance_nominale)
table(df_clean$accessibilite_pmr)
table(df_clean$condition_acces)
bilan_manquants <- data.frame(
  Nb_Manquants = colSums(is.na(df_clean)),
  Pourcentage  = round((colSums(is.na(df_clean)) / nrow(df_clean)) * 100, 2)
)
