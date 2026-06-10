# ==============================================================================
# 1. CHARGEMENT DES PACKAGES ET DES DONNÉES
# ==============================================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(rnaturalearth)
library(sf)
library(writexl)
library(lubridate)
library(readr)
library(geodist)
library(httr)
library(jsonlite)
library(geosphere)
library(data.table)
library(R.utils)

# Chargement du jeu de données IRVE
df <- read.csv("IRVE.csv")
ref_cp <- read.csv("base-officielle-codes-postaux.csv", sep = ",")
names(ref_cp)

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
#df_clean$tarification    <- NULL # 75% vierge
df_clean$siren_amenageur <- NULL # Inutile pour un consommateur et 40% manquant
df_clean$coordonneesXY   <- NULL # Doublon de coordonnées dans un format pas intéressant
df_clean$consolidated_is_code_insee_modified <- NULL
df_clean$consolidated_is_code_insee_verified <- NULL
df_clean <- df_clean %>% filter(puissance_nominale > 0)

df_clean$id_station_local <- NULL
# Suppression des métadonnées de la plateforme data.gouv
df_clean <- df_clean %>% select(-c(
  datagouv_dataset_id, 
  datagouv_resource_id, 
  datagouv_organization_or_owner, 
  created_at, 
  last_modified, 
  
))

# ==============================================================================
# 5. IMPUTATIONS ET CORRECTIONS DES DONNÉES
# ==============================================================================

# Si contact aménageur vide on y injecte contact operateur 
df_clean <- df_clean %>% 
  mutate(contact_amenageur = ifelse(is.na(contact_amenageur) | contact_amenageur == "", 
                               contact_operateur, 
                               contact_amenageur))

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
#  6.2 CORRECTION DES COMMUNES MANQUANTES
# ==============================================================================



# Garder uniquement les colonnes utiles (Modifié avec le vrai nom : nom_de_la_commune)
ref_cp <- ref_cp %>%
  select(code_commune_insee, nom_de_la_commune, code_postal) %>%
  distinct(code_commune_insee, .keep_all = TRUE)  # 1 ligne par code INSEE

# Jointure avec df_clean
df_clean <- df_clean %>%
  left_join(ref_cp, by = c("code_insee_commune" = "code_commune_insee")) %>%
  mutate(
    consolidated_code_postal = ifelse(is.na(consolidated_code_postal), code_postal, consolidated_code_postal),
    consolidated_commune     = ifelse(is.na(consolidated_commune), nom_de_la_commune, consolidated_commune)
  ) %>%
  select(-code_postal, -nom_de_la_commune)  # Supprimer les colonnes temporaires de la jointure


# Étape 1 : extraire le code postal depuis le nom de commune pour les 406
df_clean <- df_clean %>%
  mutate(
    cp_extrait_nom = as.integer(str_extract(consolidated_commune, "\\d{5}")),
    consolidated_code_postal = ifelse(
      is.na(consolidated_code_postal) & !is.na(cp_extrait_nom),
      cp_extrait_nom,
      consolidated_code_postal
    )
  ) %>%
  select(-cp_extrait_nom)

# Étape 2 : nettoyer le nom de commune (supprimer tout ce qui n'est pas la ville)
df_clean <- df_clean %>%
  mutate(consolidated_commune = str_remove_all(consolidated_commune, "\\d{5}"),        # Enlève code postal
         consolidated_commune = str_remove_all(consolidated_commune, regex("france", ignore_case = TRUE)), # Enlève France
         consolidated_commune = str_trim(str_squish(consolidated_commune)),             # Nettoie les espaces
         consolidated_commune = na_if(consolidated_commune, ""))                        # Vide → NA

# Étape 3 : jointure par code postal pour retrouver la commune
# (Couvre les 33 352 "France" et les 406 corrigés)
ref_cp_par_cp <- ref_cp %>%
  select(code_postal, nom_de_la_commune) %>%
  distinct(code_postal, .keep_all = TRUE)

df_clean <- df_clean %>%
  left_join(ref_cp_par_cp, by = c("consolidated_code_postal" = "code_postal")) %>%
  mutate(consolidated_commune = ifelse(
    is.na(consolidated_commune),
    nom_de_la_commune,
    consolidated_commune
  )) %>%
  select(-nom_de_la_commune)

# Étape 4 : jointure par code INSEE pour le reste
df_clean <- df_clean %>%
  left_join(ref_cp %>% select(code_commune_insee, code_postal) %>% rename(cp_insee = code_postal),
            by = c("code_insee_commune" = "code_commune_insee")) %>%
  mutate(consolidated_code_postal = ifelse(
    is.na(consolidated_code_postal),
    cp_insee,
    consolidated_code_postal
  )) %>%
  select(-cp_insee)
# ==============================================================================
# 6.3 RECONSTRUCTION DU CODE INSEE (COMMUNE + CP)
# ==============================================================================

# Fonction de normalisation
normaliser_commune <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("-", " ") %>%
    str_squish()
}

# Préparer la table de référence La Poste
ref_insee_cp <- ref_cp %>%
  mutate(
    commune_norm = normaliser_commune(nom_de_la_commune)
  ) %>%
  select(
    commune_norm,
    code_postal,
    code_commune_insee
  )

# Préparer le fichier IRVE
df_clean <- df_clean %>%
  mutate(
    commune_norm = normaliser_commune(consolidated_commune)
  )

# Jointure Commune + CP -> INSEE
df_clean <- df_clean %>%
  left_join(
    ref_insee_cp,
    by = c(
      "commune_norm",
      "consolidated_code_postal" = "code_postal"
    )
  ) %>%
  mutate(
    code_insee_commune = ifelse(
      is.na(code_insee_commune),
      code_commune_insee,
      code_insee_commune
    )
  ) %>%
  select(
    -commune_norm,
    -code_commune_insee
  )
# ==============================================================================
# 7. Reconstruction des Booléens
# ==============================================================================
bool_cols <- c("prise_type_ef","prise_type_2","prise_type_combo_ccs",
               "prise_type_chademo","prise_type_autre","gratuit",
               "paiement_acte","paiement_cb","reservation","station_deux_roues","paiement_autre","cable_t2_attache")

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
# étape 9 (NORMALISATION DES HORAIRES)
# ==============================================================================


# Fonction de normalisation OSM
normalize_opening_hours <- function(h, tol_minutes = 10) {
  # Gestion des NA ou chaîne vide
  if(is.na(h) || h == "") return(NA_character_)
  
  # 1. Mise en forme initiale (suppression accents, majuscules, unify sep.)
  hn <- toupper(h)
  hn <- stri_trans_general(hn, "Latin-ASCII")           # Supprimer accents
  hn <- str_replace_all(hn, "[\\.hH]", ":")            # Convertir "h" ou "." en ":"
  hn <- str_replace_all(hn, "\\s+", " ")               # Unifier espaces
  hn <- str_trim(hn, side = "both")
  
  # 2. Traduction jour FR → anglais, suppression de "DU ... AU"
  #    Ex: "Du lundi au vendredi 8:00-17:00" → "MO-FR 8:00-17:00"
  # Remplacer noms de jours complets et abbréviations
  replacements <- c(
    "\\bDU\\b"="", "\\bAU\\b"="",
    "\\bLUN(?:DI)?\\b"="MO", "\\bMAR(?:DI)?\\b"="TU",
    "\\bMER(?:CREDI)?\\b"="WE", "\\bJEU(?:DI)?\\b"="TH",
    "\\bVEN(?:DREDI)?\\b"="FR", "\\bSAM(?:EDI)?\\b"="SA",
    "\\bDIM(?:ANCHE)?\\b"="SU",
    "SUN" = "SU",   # ← ajout
    "SAT" = "SA"    # ← ajout
  )
  for(pat in names(replacements)) {
    hn <- str_replace_all(hn, regex(pat, ignore_case = TRUE), replacements[pat])
  }
  
  # 3. Nettoyage des séparateurs de jours et heures
  hn <- str_replace_all(hn, "\\s*:\\s*", " ")    # Supprimer ": " après jour
  hn <- str_replace_all(hn, "24H/24|24/24", "00:00-24:00") 
  hn <- str_replace_all(hn, "7J/7", "00:00-24:00") 
  # Ajouter zéro devant un chiffre seul (ex: "8:00" -> "08:00")
  hn <- str_replace_all(hn, "(?<=^|\\s)(\\d):", "0\\1:")
  # Uniformiser les séparateurs multiples
  hn <- str_replace_all(hn, "[,;]+", "; ")
  hn <- str_squish(hn)
  
  # 4. Détection quasi-24h → "24/7"
  # Extraire toutes les heures sous forme numériques
  tm <- str_extract_all(hn, "\\d{1,2}:\\d{2}")[[1]]
  if(length(tm) >= 2) {
    starts <- as.integer(sub(":.*", "", tm[seq(1, length(tm), by=2)])) * 60 +
      as.integer(sub(".*:", "", tm[seq(1, length(tm), by=2)]))
    ends   <- as.integer(sub(":.*", "", tm[seq(2, length(tm), by=2)])) * 60 +
      as.integer(sub(".*:", "", tm[seq(2, length(tm), by=2)]))
    # Ajuster 00:00 de fin vers 1440 min
    ends[ends == 0] <- 1440
    # Vérifier couverture quasi-continue
    if(min(starts, na.rm=TRUE) <= tol_minutes &&
       max(ends, na.rm=TRUE) >= 1440 - tol_minutes) {
      # Vérifier les écarts entre les intervalles
      sorted_idx <- order(starts)
      ok <- TRUE
      for(i in seq_along(sorted_idx)[-length(sorted_idx)]) {
        if(starts[sorted_idx[i+1]] - ends[sorted_idx[i]] > tol_minutes) {
          ok <- FALSE; break
        }
      }
      if(ok) return("24/7")
    }
  }
  
  # 5. Compression des jours et format OSM
  # Décomposer en règles séparées par ";"
  parts <- str_split(hn, ";")[[1]]
  parts <- str_trim(parts)
  full_days <- c("MO","TU","WE","TH","FR","SA","SU")
  osm_parts <- c()
  for(part in parts) {
    # Extraire la partie jours (ex: "MO,WE-TH,FR")
    day_part <- str_extract(part, "^(?:MO|TU|WE|TH|FR|SA|SU)(?:[-,](?:MO|TU|WE|TH|FR|SA|SU))*")
    time_part <- str_trim(str_sub(part, nchar(day_part)+1))
    if(day_part == "" || is.na(day_part)) {
      # Si pas de jour précisé, laisser inchangé
      osm_parts <- c(osm_parts, part)
      next
    }
    # Gérer listes de jours
    days <- unlist(str_split(day_part, ","))
    idx <- sort(match(days, full_days))
    # Plage continue de début à fin ?
    if(all(idx == 1:7)) {
      new_day <- "Mo-Su"
    } else if(all(diff(idx) == 1)) {
      new_day <- paste0(full_days[min(idx)], "-", full_days[max(idx)])
    } else {
      # Cas non consécutifs : lister séparés par virgule
      # (laisser trié pour cohérence)
      new_day <- paste(full_days[idx], collapse=",")
    }
    # Assembler jour + intervalle horaire
    osm_parts <- c(osm_parts, paste(new_day, time_part))
  }
  # Reconstruire la chaîne en séparant les règles par "; "
  out <- paste(osm_parts, collapse="; ")
  out <- str_squish(out)
  
  return(out)
}

# 6. Créer la table de correspondance pour les valeurs uniques
horaires_unique <- df_clean %>%
  distinct(horaires) %>%
  rename(Origine = horaires) %>%
  mutate(
    Normalise = sapply(Origine, normalize_opening_hours),
    Regle = case_when(
      Normalise == "24/7"                      ~ "plage quasi-24h",
      str_detect(Origine, "[LunMarMerJeuVenSamDim]") ~ "jours FR→EN",
      str_detect(Origine, "(Mo|Tu|We|Th|Fr)(?:,(Mo|Tu|We|Th|Fr))+") ~ "compression jours",
      Origine == Normalise                     ~ "déjà OSM ou inchangé",
      TRUE                                     ~ "autre"
    )
  )



# 7. Appliquer la correspondance au dataset principal
df_clean <- df_clean %>%
  left_join(horaires_unique %>% select(Origine, Normalise), 
            by = c("horaires" = "Origine")) %>%
  mutate(horaires = if_else(!is.na(Normalise), Normalise, horaires)) %>%
  select(-Normalise)
df_clean <- df_clean %>%
  mutate(horaires = ifelse(str_detect(horaires, "^T;"), NA, horaires))


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
stations_sf <- st_as_sf(df_clean, coords = c("consolidated_longitude", "consolidated_latitude"), crs = 4326)

# b. On ne garde que les points qui sont strictement DANS le polygone France
stations_filtrees_sf <- st_filter(stations_sf, france_metro)

# c. On repasse en tableau classique pour l'utiliser facilement dans la heatmap
df_clean <- stations_filtrees_sf %>%
  mutate(
    lon = st_coordinates(.)[,1], # Récupère la longitude
    lat = st_coordinates(.)[,2]  # Récupère la latitude
  ) %>%
  st_drop_geometry() # Enlève la surcouche spatiale devenue inutile

df_clean$consolidated_is_lon_lat_correct <- NULL

# ==============================================================================
# SUPPRESSION DES DOUBLONS - garder la ligne la plus récente
# ==============================================================================
df_clean <- df_clean %>%
  mutate(date_maj = as.Date(date_maj)) %>%
  arrange(id_pdc_itinerance, desc(date_maj)) %>%
  distinct(id_pdc_itinerance, .keep_all = TRUE)

# ==============================================================================
# VÉRIFICATION GPS PAR CODE POSTAL - BASE LA POSTE
# ==============================================================================


# Étape 1 : Centroïde par code postal depuis la base La Poste
# (ref_cp est déjà chargé dans ton script)
ref_coords <- ref_cp %>%
  select(code_postal, latitude, longitude) %>%
  distinct(code_postal, .keep_all = TRUE)

# Étape 2 : Jointure avec df_clean
df_clean <- df_clean %>%
  left_join(ref_coords, by = c("consolidated_code_postal" = "code_postal"))

# Étape 3 : Calculer la distance
df_clean <- df_clean %>%
  mutate(
    dist_m = ifelse(
      !is.na(longitude) & !is.na(latitude) & !is.na(lon) & !is.na(lat),
      distHaversine(cbind(lon, lat), cbind(longitude, latitude)),
      NA_real_
    )
  )

# Étape 4 : Diagnostic
message("Lignes avec distance > 50km : ", sum(df_clean$dist_m > 50000, na.rm = TRUE))
message("Lignes non vérifiables      : ", sum(is.na(df_clean$dist_m)))

# Étape 5 : Supprimer les lignes incohérentes
df_clean <- df_clean %>%
  filter(is.na(dist_m) | dist_m <= 50000)

# Étape 6 : Nettoyage
df_clean <- df_clean %>%
  select(-latitude, -longitude, -dist_m)

message("Lignes restantes : ", nrow(df_clean))






write_xlsx(df_clean, "IRVE_clean2.xlsx")
write.csv(df_clean, "IRVE_clean2.csv")
dim(df_clean)
glimpse(df_clean)
summary(df_clean$puissance_nominale)
table(df_clean$accessibilite_pmr)
table(df_clean$condition_acces)
bilan_manquants <- data.frame(
  Nb_Manquants = colSums(is.na(df_clean)),
  Pourcentage  = round((colSums(is.na(df_clean)) / nrow(df_clean)) * 100, 2)
)
sum(is.na(df$consolidated_code_postal))
#liste_enseignes <- unique(df$horaires)
#write.csv(liste_enseignes, file = "tarification.csv", row.names = FALSE)