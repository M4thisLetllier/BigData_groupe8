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
df <- read.csv("BigData_groupe8/IRVE.csv")


ref_cp <- read.csv("base-officielle-codes-postaux.csv", sep = ",")


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

# # Remplacer tous les textes vides "" par des vrais NA
# df[df == ""] <- NA
# 
# # Compter le nombre de valeurs manquantes pour CHAQUE colonne
# manquants_par_colonne <- colSums(is.na(df))
# 
# # Création du tableau de synthèse (Nombre et Pourcentage)
# bilan_manquants <- data.frame(
#   Nb_Manquants = colSums(is.na(df)),
#   Pourcentage  = round((colSums(is.na(df)) / nrow(df)) * 100, 2)
# )
# 
# print(bilan_manquants)
# print(manquants_par_colonne)


# ==============================================================================
# 4. NETTOYAGE ET SUPPRESSION DES COLONNES INUTILES
# ==============================================================================

# Créer df_clean en supprimant les lignes sans puissance_nominale
df_clean <- df %>% filter(!is.na(puissance_nominale))

# Suppression des colonnes très vides ou inutiles pour le consommateur
df_clean$observations                        <- NULL # 75% vierge
df_clean$siren_amenageur                     <- NULL # Inutile pour un consommateur et 40% manquant
df_clean$coordonneesXY                       <- NULL # Doublon de coordonnées dans un format pas intéressant
df_clean$consolidated_is_code_insee_modified <- NULL
df_clean$consolidated_is_code_insee_verified <- NULL
df_clean$consolidated_is_lon_lat_correct     <- NULL
df_clean$id_station_local                    <- NULL
df_clean$id_pdc_local                        <- NULL
df_clean$num_pdl                             <- NULL

# Suppression des métadonnées de la plateforme data.gouv
df_clean <- df_clean %>% select(-c(
  datagouv_dataset_id,
  datagouv_resource_id,
  datagouv_organization_or_owner,
  created_at,
  last_modified,
  
))

# Supprimer les lignes avec puissance = 0
df_clean <- df_clean %>% filter(puissance_nominale > 0)

cat("Etape 4 fini, nettoyage et supression des données effectué")
write.csv(df_clean, "IRVE_clean.csv", row.names = FALSE)
# ==============================================================================
# 5. IMPUTATIONS ET CORRECTIONS DES DONNÉES
# ==============================================================================

# Si contact aménageur vide on y injecte contact operateur
df_clean <- df_clean %>%
  mutate(contact_amenageur = ifelse(
    is.na(contact_amenageur) | contact_amenageur == "",
    contact_operateur,
    contact_amenageur
  ))

# Pour les colonnes textuelles, on remplace les NA par "Non spécifié"
df_clean$nom_amenageur[is.na(df_clean$nom_amenageur)] <- "Non spécifié"
df_clean$nom_operateur[is.na(df_clean$nom_operateur)] <- "Non spécifié"


# ==============================================================================
# 6. EXTRACTION ET RECONSTRUCTION GÉOGRAPHIQUE
# ==============================================================================

df_clean <- df_clean %>%
  mutate(
    # Extraire le code postal (les 5 chiffres consécutifs)
    cp_extrait = str_extract(adresse_station, "\\d{5}"),
    
    # Extraire la commune (tout ce qui se trouve APRÈS les 5 chiffres du code postal)
    commune_extraite = str_trim(str_extract(adresse_station, "(?<=\\d{5}\\s).+")),
    
    # Convertir le code postal extrait en entier
    cp_extrait = as.integer(cp_extrait),
    
    # Remplir consolidated_code_postal si vide
    consolidated_code_postal = ifelse(
      is.na(consolidated_code_postal) | consolidated_code_postal == "",
      cp_extrait,
      consolidated_code_postal
    ),
    
    # Remplir consolidated_commune si vide
    consolidated_commune = ifelse(
      is.na(consolidated_commune) | consolidated_commune == "",
      commune_extraite,
      consolidated_commune
    ),
    
    # Nettoyer l'adresse pour ne garder QUE la rue
    adresse_station = str_remove(adresse_station, "\\s?\\d{5}.*")
  ) %>%
  select(-c(cp_extrait, commune_extraite))


# ==============================================================================
# 6.2 CORRECTION DES COMMUNES MANQUANTES
# ==============================================================================

# Garder uniquement les colonnes utiles de la base La Poste
ref_cp <- ref_cp %>%
  select(code_commune_insee, nom_de_la_commune, code_postal, latitude, longitude) %>%
  distinct(code_commune_insee, .keep_all = TRUE) # 1 ligne par code INSEE

# Jointure par code INSEE
df_clean <- df_clean %>%
  left_join(ref_cp, by = c("code_insee_commune" = "code_commune_insee")) %>%
  mutate(
    consolidated_code_postal = ifelse(is.na(consolidated_code_postal), code_postal, consolidated_code_postal),
    consolidated_commune     = ifelse(is.na(consolidated_commune), nom_de_la_commune, consolidated_commune)
  ) %>%
  select(-code_postal, -nom_de_la_commune, -latitude, -longitude)

# Extraire le code postal depuis le nom de commune pour les cas corrompus
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

# Nettoyer le nom de commune
df_clean <- df_clean %>%
  mutate(
    consolidated_commune = str_remove_all(consolidated_commune, "\\d{5}"),
    consolidated_commune = str_remove_all(consolidated_commune, regex("france", ignore_case = TRUE)),
    consolidated_commune = str_trim(str_squish(consolidated_commune)),
    consolidated_commune = na_if(consolidated_commune, "")
  )

# Jointure par code postal pour retrouver la commune manquante
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

# Jointure par code INSEE pour compléter le code postal manquant
df_clean <- df_clean %>%
  left_join(
    ref_cp %>% select(code_commune_insee, code_postal) %>% rename(cp_insee = code_postal),
    by = c("code_insee_commune" = "code_commune_insee")
  ) %>%
  mutate(consolidated_code_postal = ifelse(
    is.na(consolidated_code_postal),
    cp_insee,
    consolidated_code_postal
  )) %>%
  select(-cp_insee)


# ==============================================================================
# 6.3 RECONSTRUCTION DU CODE INSEE (COMMUNE + CP)
# ==============================================================================

normaliser_commune <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("-", " ") %>%
    str_squish()
}

ref_insee_cp <- ref_cp %>%
  mutate(commune_norm = normaliser_commune(nom_de_la_commune)) %>%
  select(commune_norm, code_postal, code_commune_insee)

df_clean <- df_clean %>%
  mutate(commune_norm = normaliser_commune(consolidated_commune))

df_clean <- df_clean %>%
  left_join(
    ref_insee_cp,
    by = c("commune_norm", "consolidated_code_postal" = "code_postal")
  ) %>%
  mutate(
    code_insee_commune = ifelse(
      is.na(code_insee_commune),
      code_commune_insee,
      code_insee_commune
    )
  ) %>%
  select(-commune_norm, -code_commune_insee)


# ==============================================================================
# 7. RECONSTRUCTION DES BOOLÉENS
# ==============================================================================

bool_cols <- c(
  "prise_type_ef", "prise_type_2", "prise_type_combo_ccs",
  "prise_type_chademo", "prise_type_autre", "gratuit",
  "paiement_acte", "paiement_cb", "paiement_autre",
  "reservation", "station_deux_roues", "cable_t2_attache"
)

df_clean <- df_clean %>%
  mutate(across(all_of(bool_cols), ~as.integer(tolower(as.character(.)) %in% c("true", "1"))))

# Supprimer les lignes où l'INSEE, la commune ET le code postal sont TOUS manquants
df_clean <- df_clean %>%
  filter(!(is.na(code_insee_commune)  & is.na(consolidated_code_postal)))
# ==============================================================================
# 8. CORRECTION DES ENCODAGES CORROMPUS
# ==============================================================================

df_clean <- df_clean %>%
  mutate(condition_acces = case_when(
    stri_detect_fixed(stri_trans_general(condition_acces, "Latin-ASCII"), "libre")  ~ "Accès libre",
    stri_detect_fixed(stri_trans_general(condition_acces, "Latin-ASCII"), "reserv") ~ "Accès réservé",
    TRUE ~ condition_acces
  ))

df_clean <- df_clean %>%
  mutate(accessibilite_pmr = case_when(
    str_detect(accessibilite_pmr, regex("non r.serv", ignore_case = TRUE)) ~ "Accessible non réservé",
    str_detect(accessibilite_pmr, regex("r.serv",     ignore_case = TRUE)) ~ "Réservé PMR",
    str_detect(accessibilite_pmr, regex("non access", ignore_case = TRUE)) ~ "Non accessible",
    TRUE                                                                    ~ "Accessibilité inconnue"
  ))


# ==============================================================================
# 9. NORMALISATION DES HORAIRES
# ==============================================================================

normalize_opening_hours <- function(h, tol_minutes = 10) {
  if (is.na(h) || h == "") return(NA_character_)
  
  hn <- toupper(h)
  hn <- stri_trans_general(hn, "Latin-ASCII")
  hn <- str_replace_all(hn, "[\\.hH]", ":")
  hn <- str_replace_all(hn, "\\s+", " ")
  hn <- str_trim(hn, side = "both")
  
  replacements <- c(
    "\\bDU\\b" = "", "\\bAU\\b" = "",
    "\\bLUN(?:DI)?\\b" = "MO", "\\bMAR(?:DI)?\\b" = "TU",
    "\\bMER(?:CREDI)?\\b" = "WE", "\\bJEU(?:DI)?\\b" = "TH",
    "\\bVEN(?:DREDI)?\\b" = "FR", "\\bSAM(?:EDI)?\\b" = "SA",
    "\\bDIM(?:ANCHE)?\\b" = "SU",
    "SUN" = "SU",
    "SAT" = "SA"
  )
  for (pat in names(replacements)) {
    hn <- str_replace_all(hn, regex(pat, ignore_case = TRUE), replacements[pat])
  }
  
  hn <- str_replace_all(hn, "\\s*:\\s*", " ")
  hn <- str_replace_all(hn, "24H/24|24/24", "00:00-24:00")
  hn <- str_replace_all(hn, "7J/7", "00:00-24:00")
  hn <- str_replace_all(hn, "(?<=^|\\s)(\\d):", "0\\1:")
  hn <- str_replace_all(hn, "[,;]+", "; ")
  hn <- str_squish(hn)
  
  tm <- str_extract_all(hn, "\\d{1,2}:\\d{2}")[[1]]
  if (length(tm) >= 2) {
    starts <- as.integer(sub(":.*", "", tm[seq(1, length(tm), by = 2)])) * 60 +
      as.integer(sub(".*:", "", tm[seq(1, length(tm), by = 2)]))
    ends <- as.integer(sub(":.*", "", tm[seq(2, length(tm), by = 2)])) * 60 +
      as.integer(sub(".*:", "", tm[seq(2, length(tm), by = 2)]))
    ends[ends == 0] <- 1440
    if (min(starts, na.rm = TRUE) <= tol_minutes &&
        max(ends, na.rm = TRUE) >= 1440 - tol_minutes) {
      sorted_idx <- order(starts)
      ok <- TRUE
      for (i in seq_along(sorted_idx)[-length(sorted_idx)]) {
        if (starts[sorted_idx[i + 1]] - ends[sorted_idx[i]] > tol_minutes) {
          ok <- FALSE
          break
        }
      }
      if (ok) return("24/7")
    }
  }
  
  parts <- str_split(hn, ";")[[1]]
  parts <- str_trim(parts)
  full_days <- c("MO", "TU", "WE", "TH", "FR", "SA", "SU")
  osm_parts <- c()
  for (part in parts) {
    day_part  <- str_extract(part, "^(?:MO|TU|WE|TH|FR|SA|SU)(?:[-,](?:MO|TU|WE|TH|FR|SA|SU))*")
    time_part <- str_trim(str_sub(part, nchar(day_part) + 1))
    if (day_part == "" || is.na(day_part)) {
      osm_parts <- c(osm_parts, part)
      next
    }
    days <- unlist(str_split(day_part, ","))
    idx  <- sort(match(days, full_days))
    if (all(idx == 1:7)) {
      new_day <- "Mo-Su"
    } else if (all(diff(idx) == 1)) {
      new_day <- paste0(full_days[min(idx)], "-", full_days[max(idx)])
    } else {
      new_day <- paste(full_days[idx], collapse = ",")
    }
    osm_parts <- c(osm_parts, paste(new_day, time_part))
  }
  out <- paste(osm_parts, collapse = "; ")
  out <- str_squish(out)
  return(out)
}

horaires_unique <- df_clean %>%
  distinct(horaires) %>%
  rename(Origine = horaires) %>%
  mutate(
    Normalise = sapply(Origine, normalize_opening_hours),
    Regle = case_when(
      Normalise == "24/7"                                                    ~ "plage quasi-24h",
      str_detect(Origine, "[LunMarMerJeuVenSamDim]")                        ~ "jours FR→EN",
      str_detect(Origine, "(Mo|Tu|We|Th|Fr)(?:,(Mo|Tu|We|Th|Fr))+")        ~ "compression jours",
      Origine == Normalise                                                   ~ "déjà OSM ou inchangé",
      TRUE                                                                   ~ "autre"
    )
  )

df_clean <- df_clean %>%
  left_join(horaires_unique %>% select(Origine, Normalise),
            by = c("horaires" = "Origine")) %>%
  mutate(horaires = if_else(!is.na(Normalise), Normalise, horaires)) %>%
  select(-Normalise)

# Mettre à NA les horaires corrompus
df_clean <- df_clean %>%
  mutate(horaires = ifelse(str_detect(horaires, "^T;"), NA, horaires))


# ==============================================================================
# 10. CORRECTION DES VALEURS ABERRANTES - PUISSANCE NOMINALE
# ==============================================================================

df_clean <- df_clean %>%
  mutate(
    puissance_nominale = ifelse(puissance_nominale > 350, puissance_nominale / 1000, puissance_nominale),
    puissance_nominale = ifelse(puissance_nominale > 350, 350, puissance_nominale)
  )


# ==============================================================================
# 11. FILTRAGE GÉOGRAPHIQUE - FRANCE MÉTROPOLITAINE
# ==============================================================================

france_complete <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_metro    <- st_crop(france_complete, xmin = -5.5, ymin = 41, xmax = 10, ymax = 51.5)

stations_sf <- st_as_sf(
  df_clean,
  coords = c("consolidated_longitude", "consolidated_latitude"),
  crs = 4326
)

stations_filtrees_sf <- st_filter(stations_sf, france_metro)

df_clean <- stations_filtrees_sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()


# ==============================================================================
# 12. SUPPRESSION DES DOUBLONS - garder la ligne la plus récente
# ==============================================================================

df_clean <- df_clean %>%
  mutate(date_maj = as.Date(date_maj)) %>%
  arrange(id_pdc_itinerance, desc(date_maj)) %>%
  distinct(id_pdc_itinerance, .keep_all = TRUE)


# # ==============================================================================
# # 13. VÉRIFICATION GPS PAR GEOCODAGE - Nouvelle API Géoplateforme (IGN)
# # ==============================================================================
# library(httr)
# library(jsonlite)
# library(geosphere)
# library(stringr)
# library(dplyr)
# 
# # Étape 1 : Construire et nettoyer l'adresse complète pour chaque borne
# df_clean <- df_clean %>%
#   mutate(
#     adresse_complete = paste(
#       ifelse(is.na(adresse_station), "", adresse_station),
#       ifelse(is.na(consolidated_code_postal), "", as.integer(consolidated_code_postal)),
#       ifelse(is.na(consolidated_commune), "", consolidated_commune)
#     ),
#     # Nettoyage des espaces multiples laissés par les NA
#     adresse_complete = str_squish(adresse_complete)
#   )
# 
# # Étape 2 : Geocodage en batch par la nouvelle API IGN (Géoplateforme)
# geocoder_batch <- function(adresses) {
#   tmp <- tempfile(fileext = ".csv")
#   write.csv(data.frame(adresse = adresses), tmp, row.names = FALSE, fileEncoding = "UTF-8")
#   
#   # Le nouveau point de terminaison de la Géoplateforme de l'IGN
#   response <- POST(
#     "https://data.geopf.fr/geocodage/search/csv",
#     body = list(data = upload_file(tmp), columns = "adresse"),
#     encode = "multipart",
#     config = httr::config(connecttimeout = 60), 
#     timeout(120)                                
#   )
#   
#   # Parser le résultat
#   result <- content(response, as = "text", encoding = "UTF-8")
#   
#   #On force tout en texte avec colClasses = "character"
#   read.csv(text = result, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
# }
# 
# # Étape 3 : Découper en chunks (lots de 2500) et ajouter une pause
# chunk_size <- 2500
# n <- nrow(df_clean)
# chunks <- split(df_clean$adresse_complete, ceiling(seq_along(df_clean$adresse_complete) / chunk_size))
# 
# message("Géocodage en cours sur ", n, " adresses en ", length(chunks), " batch(s)...")
# 
# resultats <- lapply(seq_along(chunks), function(i) {
#   message("  Traitement du batch ", i, "/", length(chunks), "...")
# 
#   # PAUSE DE 3 SECONDES pour respecter la limite d'usage de la nouvelle API
#   if(i > 1) Sys.sleep(3)
# 
#   geocoder_batch(chunks[[i]])
# })
# 
# # Combiner tous les résultats
# geocodes <- bind_rows(resultats)
# 
# # Étape 4 : Récupérer les coordonnées géocodées
# # suppressWarnings masque les alertes si l'API renvoie des vides (non trouvés)
# suppressWarnings({
#   df_clean$lon_geocode   <- as.numeric(geocodes$longitude)
#   df_clean$lat_geocode   <- as.numeric(geocodes$latitude)
#   df_clean$geocode_score <- as.numeric(geocodes$result_score)
# })
# 
# # Étape 5 : Calculer la distance entre les coordonnées d'origine et celles géocodées
# df_clean <- df_clean %>%
#   mutate(
#     dist_geocode = ifelse(
#       !is.na(lon_geocode) & !is.na(lat_geocode) & !is.na(lon) & !is.na(lat),
#       distHaversine(cbind(lon, lat), cbind(lon_geocode, lat_geocode)),
#       NA_real_
#     )
#   )
# 
# message("Lignes avec distance > 50km : ", sum(df_clean$dist_geocode > 50000, na.rm = TRUE))
# 
# # Étape 6 : Supprimer les lignes aberrantes
# df_clean <- df_clean %>%
#   filter(
#     is.na(dist_geocode) |          # L'API n'a rien trouvé -> On garde au bénéfice du doute
#       geocode_score < 0.5 |          # L'API n'est pas sûre de l'adresse -> On garde
#       dist_geocode <= 50000          # La distance est < 50km -> C'est cohérent, on garde
#   )
# 
# # Étape 7 : Nettoyage des colonnes temporaires
# df_clean <- df_clean %>%
#   select(-adresse_complete, -lon_geocode, -lat_geocode, -geocode_score, -dist_geocode)
# 
# message("Lignes restantes après vérification géocodée : ", nrow(df_clean))
# 
# ==============================================================================
# 13. VÉRIFICATION GPS PAR CROISEMENT AVEC LE CENTROÏDE DE LA COMMUNE
# ==============================================================================
library(dplyr)
library(geosphere)

cat("Vérification de la cohérence géographique en cours...\n")

# Étape 1 : Préparer le fichier de référence (La Poste)
# On ne garde qu'une seule coordonnée (le centre) par code INSEE
ref_coord <- ref_cp %>%
  select(code_commune_insee, lat_commune = latitude, lon_commune = longitude) %>%
  filter(!is.na(lat_commune) & !is.na(lon_commune)) %>%
  mutate(
    lat_commune = as.numeric(lat_commune),
    lon_commune = as.numeric(lon_commune)
  ) %>%
  distinct(code_commune_insee, .keep_all = TRUE)

# Étape 2 : Joindre les coordonnées du centre de la ville à nos bornes
df_clean <- df_clean %>%
  left_join(ref_coord, by = c("code_insee_commune" = "code_commune_insee"))

# Étape 3 : Calculer la distance (en mètres) entre la borne et le centre de sa ville
df_clean <- df_clean %>%
  mutate(
    dist_centre_ville = ifelse(
      !is.na(lon) & !is.na(lat) & !is.na(lon_commune) & !is.na(lat_commune),
      distHaversine(cbind(lon, lat), cbind(lon_commune, lat_commune)),
      NA_real_
    )
  )

# Afficher un petit bilan avant de supprimer les erreurs
nb_erreurs <- sum(df_clean$dist_centre_ville > 30000, na.rm = TRUE)
message("Lignes détectées avec une position aberrante (> 30km du centre) : ", nb_erreurs)

# Étape 4 : Supprimer les valeurs aberrantes
df_clean <- df_clean %>%
  filter(
    is.na(dist_centre_ville) | dist_centre_ville <= 30000
  )

# Étape 5 : Nettoyer les colonnes temporaires
df_clean <- df_clean %>%
  select(-lat_commune, -lon_commune, -dist_centre_ville)

message("Vérification terminée. Lignes restantes : ", nrow(df_clean))
# ==============================================================================
# 14. Uniformisation du tarif
# ==============================================================================


# Définition de la fonction de nettoyage et d'extraction ultra-robuste
extraire_tarif_kwh <- function(texte) {
  # Mettre en minuscules et supprimer les espaces inutiles au début/fin
  txt <- str_trim(str_to_lower(texte))
  
  # Étape A : Correction des erreurs d'encodage fréquentes pour le symbole €
  txt <- str_replace_all(txt, "â‚¬", "€")
  txt <- str_replace_all(txt, "ū", "€")
  
  # Étape B : Nettoyage des mentions techniques collées (ex: "0.25ac €/kWh" -> "0.25 €/kWh")
  txt <- str_replace_all(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(ac|dc)", "\\1")
  
  # Initialisation du vecteur de résultats avec des valeurs vides (NA)
  resultat <- rep(NA_real_, length(texte))
  
  # ----------------------------------------------------
  # RÈGLE 1 : Gestion des centimes (ex: "59 cts/kWh", "45cts/kWh", "0.35 c/kWh")
  # ----------------------------------------------------
  match_cts <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(?:cts|ct|c)\\s*(?:/|par)?\\s*kw\\s*h")
  if (!all(is.na(match_cts))) {
    val_cts <- as.numeric(str_replace(match_cts[, 2], ",", "."))
    # Si le texte contenait déjà une virgule/un point ou était < 1 (ex: 0.35 c/kwh), c'est déjà en euros.
    # Sinon (ex: 45cts), on divise par 100 pour l'avoir en euros (0.45).
    is_decimal <- str_detect(match_cts[, 2], "[.,]") | (!is.na(val_cts) & val_cts < 1)
    val_cts <- ifelse(is_decimal, val_cts, val_cts / 100)
    resultat[!is.na(val_cts)] <- val_cts[!is.na(val_cts)]
  }
  
  # ----------------------------------------------------
  # RÈGLE 2 : Motif principal avec séparateur (ex: "0,38 €/kWh", "0.33334€ par kwh")
  # ----------------------------------------------------
  match1 <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(?:€|e)?\\s*(?:ttc|ht)?\\s*(?:/|par)\\s*kw\\s*/?\\s*h")
  val1 <- as.numeric(str_replace(match1[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val1)] <- val1[is.na(resultat) & !is.na(val1)]
  
  # ----------------------------------------------------
  # RÈGLE 3 : Motif sans séparateur direct (ex: "0,55€/kWh", "0.40€ kwh")
  # ----------------------------------------------------
  match2 <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(?:€|e)\\s*(?:ttc|ht)?\\s*kw\\s*/?\\s*h")
  val2 <- as.numeric(str_replace(match2[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val2)] <- val2[is.na(resultat) & !is.na(val2)]
  
  # ----------------------------------------------------
  # RÈGLE 4 : Expressions textuelles ciblées (ex: "0.3 : prix au kwh")
  # ----------------------------------------------------
  match_au <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*:\\s*prix\\s*au\\s*kw\\s*/?\\s*h")
  val_au <- as.numeric(str_replace(match_au[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val_au)] <- val_au[is.na(resultat) & !is.na(val_au)]
  
  # ----------------------------------------------------
  # RÈGLE 5 : Nombre suivi de juste kw/h ou kwh (ex: "0.40 kw/h") - Seuil max de cohérence à 2€
  # ----------------------------------------------------
  match3 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*kw\\s*/?\\s*h")
  val3 <- as.numeric(str_replace(match3[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val3) & val3 < 2] <- val3[is.na(resultat) & !is.na(val3) & val3 < 2]
  
  # ----------------------------------------------------
  # RÈGLE 6 : Chiffre suivi de / kwh (ex: "0,2668 / kwh")
  # ----------------------------------------------------
  match4 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*/\\s*kw\\s*/?\\s*h")
  val4 <- as.numeric(str_replace(match4[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val4)] <- val4[is.na(resultat) & !is.na(val4)]
  
  # ----------------------------------------------------
  # RÈGLE 7 : Chiffres décimaux simples suivis de € (ex: "0.36€", "1,23€")
  # ----------------------------------------------------
  match5 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*(?:€|e)")
  val5 <- as.numeric(str_replace(match5[, 2], ",", "."))
  resultat[is.na(resultat) & !is.na(val5) & val5 < 2] <- val5[is.na(resultat) & !is.na(val5) & val5 < 2]
  
  # ----------------------------------------------------
  # RÈGLE 8 : Chiffres décimaux isolés (ex: "0.38", "0,22")
  # ----------------------------------------------------
  is_pure_decimal <- str_detect(txt, "^[0-9]+[.,][0-9]+$")
  val6 <- as.numeric(str_replace(txt, ",", "."))
  resultat[is.na(resultat) & is_pure_decimal & val6 < 2] <- val6[is.na(resultat) & is_pure_decimal & val6 < 2]
  
  # ----------------------------------------------------
  # RÈGLE 9 : Bornes gratuites enregistrées textuellement ou égales à "0"
  # ----------------------------------------------------
  is_zero <- (txt == "0") | str_detect(txt, "gratuit") | str_detect(txt, "0 pour utilisateur")
  resultat[is.na(resultat) & is_zero] <- 0.0
  
  return(resultat)
}

# 3. Application des traitements sur le dataframe
df_clean <- df_clean %>%
  mutate(
    # A. On extrait les tarifs comme avant
    tarif_kwh_clean = extraire_tarif_kwh(tarification),
    
    # B. On force à 0 si la colonne 'gratuit' l'indique (1, "true", "vrai", etc.)
    # On met tout en minuscules pour éviter les soucis de majuscules (True, TRUE, etc.)
    tarif_kwh_clean = ifelse(str_to_lower(gratuit) %in% c("1", "true", "vrai"), 0, tarif_kwh_clean)
  ) %>%
  # C. On supprime l'ancienne colonne avec le signe moins (-)
  select(-tarification)


#==============================================================================
# 15. EXPORT Final
# ==============================================================================

#write_xlsx(df_clean, "IRVE_clean.xlsx")
write.csv(df_clean, "IRVE_clean.csv", row.names = FALSE)


# ==============================================================================
# 16. VÉRIFICATION FINALE
# ==============================================================================

dim(df_clean)
glimpse(df_clean)
summary(df_clean$puissance_nominale)
table(df_clean$accessibilite_pmr)
table(df_clean$condition_acces)

bilan_final <- data.frame(
  Nb_Manquants = colSums(is.na(df_clean)),
  Pourcentage  = round((colSums(is.na(df_clean)) / nrow(df_clean)) * 100, 2)
)
print(bilan_final)


