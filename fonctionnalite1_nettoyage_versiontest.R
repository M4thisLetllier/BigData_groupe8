# =============================================================================
# PROJET BIG DATA / IA / WEB – FISE3 2026
# Fonctionnalité 1 : Description, exploration et nettoyage des données IRVE
# =============================================================================
# Dataset : Infrastructure de Recharge pour Véhicules Électriques (data.gouv.fr)
# Technologie : R
# =============================================================================

# ─── 0. PACKAGES ──────────────────────────────────────────────────────────────
# install.packages(c("dplyr", "stringr", "lubridate"))
library(dplyr)
library(stringr)
library(lubridate)


# =============================================================================
# ÉTAPE 1 – CHARGEMENT
# =============================================================================

donnees_irve <- read.csv(
  "IRVE.csv",
  header           = TRUE,
  sep              = ",",
  encoding         = "UTF-8",
  stringsAsFactors = FALSE,
  na.strings       = c("", "NA", "N/A", "null", "NULL")
)

cat(sprintf("Dimensions initiales : %d lignes x %d colonnes\n",
            nrow(donnees_irve), ncol(donnees_irve)))

nb_lignes_initial <- nrow(donnees_irve)


# =============================================================================
# ÉTAPE 2 – DESCRIPTION DU JEU DE DONNÉES
# =============================================================================

cat("\n--- Aperçu (colonnes clés) ---\n")
print(head(donnees_irve[, c("nom_operateur", "nom_station", "implantation_station",
                             "puissance_nominale", "nbre_pdc", "condition_acces",
                             "date_mise_en_service")], 5))

cat("\n--- Types des colonnes ---\n")
print(sapply(donnees_irve, class))

# Taux de valeurs manquantes par colonne 
cat("\n--- Taux de valeurs manquantes par colonne (%) ---\n")
taux_na <- sapply(donnees_irve,
                  function(x) round(sum(is.na(x) | x == "") / nrow(donnees_irve) * 100, 1))
taux_na_tri <- sort(taux_na[taux_na > 0], decreasing = TRUE)
print(taux_na_tri)

cat("\n--- Statistiques descriptives univariées ---\n")
print(summary(donnees_irve[, c("puissance_nominale", "nbre_pdc",
                                "consolidated_longitude", "consolidated_latitude")]))

cat("\n--- condition_acces (avant nettoyage) ---\n")
print(table(donnees_irve$condition_acces, useNA = "ifany"))

cat("\n--- implantation_station (avant nettoyage) ---\n")
print(table(donnees_irve$implantation_station, useNA = "ifany"))

cat("\n--- Top 10 operateurs ---\n")
print(head(sort(table(donnees_irve$nom_operateur), decreasing = TRUE), 10))


# =============================================================================
# ÉTAPE 3 – DÉDUPLICATION
# Problème : le même point de charge (id_pdc_itinerance) apparaît en
# plusieurs versions dans le fichier (mises à jour successives).
# Solution  : on trie par last_modified décroissant et on garde la
#             version la plus récente de chaque point de charge.
# =============================================================================

donnees_irve$last_modified <- ymd_hms(donnees_irve$last_modified,
                                       tz = "UTC", quiet = TRUE)

nb_doublons <- sum(duplicated(donnees_irve$id_pdc_itinerance, incomparables = NA))
cat(sprintf("\nDoublons sur id_pdc_itinerance (versions obsoletes) : %d\n", nb_doublons))

donnees_irve <- donnees_irve %>%
  arrange(desc(last_modified)) %>%
  distinct(id_pdc_itinerance, .keep_all = TRUE)

cat(sprintf("Lignes apres deduplication : %d  (supprimees : %d)\n",
            nrow(donnees_irve), nb_lignes_initial - nrow(donnees_irve)))


# =============================================================================
# ÉTAPE 4 – NETTOYAGE : LE MÉGA-BLOC
# On applique les 6 stratégies du cahier des charges en un seul mutate()
# Stratégies (source : NETTOYAGE_DES_DONNEES_SUPERVISE.pdf) :
#   1 – Dates        -> NA si absurde (< 2010 ou > aujourd'hui)
#   2 – Équipements  -> "Faux" si vide / NA
#   3 – Chiffres     -> imputation par la MÉDIANE si 0, absurde ou NA
#   4 – Texte        -> "Inconnu" si vide / NA
#   5 – Encodage     -> correction regex des variantes corrompues
#   6 – Poubelles    -> suppression de colonnes (apres le mutate)
# =============================================================================

# --- Pré-calculs des médianes AVANT le méga-bloc ---
# (na.rm = TRUE : on ignore les NA pour calculer sur les valeurs valides)
mediane_puissance <- median(donnees_irve$puissance_nominale, na.rm = TRUE)
mediane_nbre_pdc  <- median(donnees_irve$nbre_pdc,           na.rm = TRUE)

cat(sprintf("\nMediane puissance_nominale : %.1f kW\n", mediane_puissance))
cat(sprintf("Mediane nbre_pdc           : %.1f\n",    mediane_nbre_pdc))

# --- Extraction des coordonnées GPS depuis coordonneesXY ---
# Format : "[lon, lat]" -> on récupère la longitude et la latitude
donnees_irve$longitude <- as.numeric(
  str_match(donnees_irve$coordonneesXY, "\\[([^,]+),")[, 2]
)
donnees_irve$latitude <- as.numeric(
  str_match(donnees_irve$coordonneesXY, ",\\s*([^\\]]+)\\]")[, 2]
)
# Priorité aux coordonnées consolidées (déjà validées par l'État) quand disponibles
donnees_irve$longitude <- ifelse(!is.na(donnees_irve$consolidated_longitude),
                                  donnees_irve$consolidated_longitude,
                                  donnees_irve$longitude)
donnees_irve$latitude  <- ifelse(!is.na(donnees_irve$consolidated_latitude),
                                  donnees_irve$consolidated_latitude,
                                  donnees_irve$latitude)


# ─── LE MÉGA-BLOC ─────────────────────────────────────────────────────────────
donnees_irve <- donnees_irve %>%
  mutate(

    # ── STRATÉGIE 1 : Dates -> NA si absurde ──────────────────────────────────
    # On nettoie le texte, on parse, puis on invalide les dates hors plage.
    # On ne supprime PAS la ligne (la borne existe, seule la date est inconnue).

    date_test = as.Date(substr(as.character(date_mise_en_service), 1, 10),
                        format = "%Y-%m-%d"),
    date_mise_en_service = ifelse(
      is.na(date_test) |
        date_test < as.Date("2010-01-01") |
        date_test > Sys.Date(),
      NA_character_,
      as.character(date_mise_en_service)
    ),

    date_maj = {
      d <- as.Date(substr(as.character(date_maj), 1, 10), format = "%Y-%m-%d")
      ifelse(is.na(d) | d < as.Date("2010-01-01") | d > Sys.Date(),
             NA_character_, as.character(date_maj))
    }, 

    # ── STRATÉGIE 2 : Équipements -> "Faux" si vide / NA ─────────────────────
    # Si l'opérateur ne coche pas la case, c'est dans 99% des cas
    # parce que l'équipement n'est pas présent.

    prise_type_ef        = ifelse(is.na(prise_type_ef)        | prise_type_ef == "",
                                   "Faux", as.character(prise_type_ef)),
    prise_type_2         = ifelse(is.na(prise_type_2)         | prise_type_2 == "",
                                   "Faux", as.character(prise_type_2)),
    prise_type_combo_ccs = ifelse(is.na(prise_type_combo_ccs) | prise_type_combo_ccs == "",
                                   "Faux", as.character(prise_type_combo_ccs)),
    prise_type_chademo   = ifelse(is.na(prise_type_chademo)   | prise_type_chademo == "",
                                   "Faux", as.character(prise_type_chademo)),
    prise_type_autre     = ifelse(is.na(prise_type_autre)     | prise_type_autre == "",
                                   "Faux", as.character(prise_type_autre)),
    gratuit              = ifelse(is.na(gratuit)              | gratuit == "",
                                   "Faux", as.character(gratuit)),
    paiement_acte        = ifelse(is.na(paiement_acte)        | paiement_acte == "",
                                   "Faux", as.character(paiement_acte)),
    paiement_cb          = ifelse(is.na(paiement_cb)          | paiement_cb == "",
                                   "Faux", as.character(paiement_cb)),
    paiement_autre       = ifelse(is.na(paiement_autre)       | paiement_autre == "",
                                   "Faux", as.character(paiement_autre)),
    reservation          = ifelse(is.na(reservation)          | reservation == "",
                                   "Faux", as.character(reservation)),
    station_deux_roues   = ifelse(is.na(station_deux_roues)   | station_deux_roues == "",
                                   "Faux", as.character(station_deux_roues)),
    cable_t2_attache     = ifelse(is.na(cable_t2_attache)     | cable_t2_attache == "",
                                   "Faux", as.character(cable_t2_attache)),

    # ── STRATÉGIE 3 : Chiffres vitaux -> imputation par la médiane ────────────
    # Si puissance = 0, absurde (> 1000 kW) ou manquante -> médiane.
    # Si nbre_pdc <= 0 ou manquant -> médiane.

    puissance_nominale = ifelse(
      is.na(puissance_nominale) |
        puissance_nominale == 0  |
        puissance_nominale > 1000,
      mediane_puissance,
      puissance_nominale
    ),

    nbre_pdc = ifelse(
      is.na(nbre_pdc) | nbre_pdc <= 0,
      mediane_nbre_pdc,
      nbre_pdc
    ),

    # ── STRATÉGIE 4 : Texte descriptif -> "Inconnu" si vide / NA ─────────────
    # On garde la ligne : l'absence de nom n'empêche pas d'étudier la puissance.

    nom_operateur   = ifelse(is.na(nom_operateur)   | nom_operateur == "",
                              "Inconnu", nom_operateur),
    nom_amenageur   = ifelse(is.na(nom_amenageur)   | nom_amenageur == "",
                              "Inconnu", nom_amenageur),
    nom_enseigne    = ifelse(is.na(nom_enseigne)    | nom_enseigne == "",
                              "Non Renseigne", nom_enseigne),
    adresse_station = ifelse(is.na(adresse_station) | adresse_station == "",
                              "Non Renseigne", adresse_station),
    nom_station     = ifelse(is.na(nom_station)     | nom_station == "",
                              "Non Renseigne", nom_station),

    # ── STRATÉGIE 5 : Encodage -> correction des variantes corrompues ─────────
    # Certaines valeurs ont des caractères corrompus (problème UTF-8/Latin-1).
    # On utilise des regex pour attraper toutes les variantes de "Acces".

    condition_acces = case_when(
      str_detect(condition_acces, "Acc.{1,4}s libre")          ~ "Acces libre",
      str_detect(condition_acces, "Acc.{1,4}s r.{1,6}serv")   ~ "Acces reserve",
      condition_acces == "Acces libre"                          ~ "Acces libre",
      condition_acces == "Acces reserve"                        ~ "Acces reserve",
      TRUE ~ condition_acces
    ),

    implantation_station = case_when(
      str_detect(implantation_station,
                 "Parking priv.{1,4} .{1,4} usage public")
        ~ "Parking prive a usage public",
      str_detect(implantation_station,
                 "Parking priv.{1,4} r.{1,6}serv.{1,4} .{1,4} la client")
        ~ "Parking prive reserve a la clientele",
      TRUE ~ implantation_station
    )

  ) %>%

  # Nettoyage de la colonne temporaire utilisée pour tester les dates
  select(-date_test)


# =============================================================================
# ÉTAPE 5 – STRATÉGIE 1 COORDONNÉES GPS
# Suppression STRICTE des lignes sans coordonnées GPS.
# "Une borne sans GPS est un fantôme. Elle fera planter la cartographie."
# =============================================================================

nb_avant_gps <- nrow(donnees_irve)

donnees_irve <- donnees_irve %>%
  filter(!is.na(longitude) & !is.na(latitude))

cat(sprintf("\nLignes supprimees (pas de GPS) : %d\n",
            nb_avant_gps - nrow(donnees_irve)))
cat(sprintf("Lignes restantes               : %d\n", nrow(donnees_irve)))


# =============================================================================
# ÉTAPE 6 – STRATÉGIE 6 : SUPPRESSION DES COLONNES "POUBELLES"
# Ce sont des métadonnées du site data.gouv.fr, inutiles pour notre projet.
# =============================================================================

cols_poubelles <- c(
  "datagouv_dataset_id",
  "datagouv_resource_id",
  "datagouv_organization_or_owner",
  "created_at",
  "coordonneesXY",                      # remplacee par longitude / latitude
  "consolidated_longitude",             # fusionnee dans longitude
  "consolidated_latitude",              # fusionnee dans latitude
  "consolidated_is_lon_lat_correct",
  "consolidated_is_code_insee_verified",
  "consolidated_is_code_insee_modified"
)

cols_a_supprimer <- cols_poubelles[cols_poubelles %in% names(donnees_irve)]
donnees_irve <- donnees_irve %>% select(-all_of(cols_a_supprimer))

cat(sprintf("\nColonnes supprimees (%d) :\n", length(cols_a_supprimer)))
cat(paste(" -", cols_a_supprimer, collapse = "\n"), "\n")
cat(sprintf("Colonnes finales : %d\n", ncol(donnees_irve)))


# =============================================================================
# ÉTAPE 7 – VÉRIFICATION FINALE
# =============================================================================

cat("\n=== VERIFICATION FINALE ===\n")

taux_na_final <- sapply(donnees_irve,
                         function(x) round(sum(is.na(x)) / nrow(donnees_irve) * 100, 1))
taux_na_final_tri <- sort(taux_na_final[taux_na_final > 0], decreasing = TRUE)
cat("Valeurs manquantes restantes (%) :\n")
print(taux_na_final_tri)

cat("\ncondition_acces (apres nettoyage) :\n")
print(table(donnees_irve$condition_acces, useNA = "ifany"))

cat("\nimplantation_station (apres nettoyage) :\n")
print(table(donnees_irve$implantation_station, useNA = "ifany"))

cat("\npuissance_nominale (apres imputation mediane) :\n")
print(summary(donnees_irve$puissance_nominale))

cat("\n====================================================\n")
cat("RECAPITULATIF FINAL\n")
cat("====================================================\n")
cat(sprintf("  Lignes initiales                 : %d\n", nb_lignes_initial))
cat(sprintf("  Lignes apres deduplication       : %d\n", nb_avant_gps))
cat(sprintf("  Lignes apres filtrage GPS        : %d\n", nrow(donnees_irve)))
cat(sprintf("  Total lignes supprimees          : %d\n",
            nb_lignes_initial - nrow(donnees_irve)))
cat(sprintf("  Colonnes finales                 : %d\n", ncol(donnees_irve)))
cat(sprintf("  Mediane puissance appliquee      : %.1f kW\n", mediane_puissance))
cat(sprintf("  Mediane nbre_pdc appliquee       : %.1f\n", mediane_nbre_pdc))


# =============================================================================
# ÉTAPE 8 – EXPORT (Fonctionnalité 6 : données nettoyées pour la partie IA)
# =============================================================================

write.csv(donnees_irve,
          file         = "IRVE_cleaned.csv",
          row.names    = FALSE,
          fileEncoding = "UTF-8")

cat("\nFichier exporte : IRVE_cleaned.csv\n")
cat(sprintf("%d lignes x %d colonnes\n", nrow(donnees_irve), ncol(donnees_irve)))
