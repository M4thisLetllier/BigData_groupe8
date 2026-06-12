#==> Victor et 

cat("Lecture csv code postaux ...")
ref_cp <- read.csv("base-officielle-codes-postaux.csv", sep = ",")

# ==============================================================================
# EXTRACTION ET RECONSTRUCTION GÉOGRAPHIQUE
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
# CORRECTION DES COMMUNES MANQUANTES
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
# RECONSTRUCTION DU CODE INSEE (COMMUNE + CP)
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
#FILTRAGE GÉOGRAPHIQUE - FRANCE MÉTROPOLITAINE
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
# VÉRIFICATION GPS PAR CROISEMENT AVEC LE CENTROÏDE DE LA COMMUNE
# ==============================================================================

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