# ==============================================================================
#DIAGNOSTIC DES VALEURS MANQUANTES
# ==============================================================================

# Remplacer tous les textes vides "" par des vrais NA
df[df == ""] <- NA

# Compter le nombre de valeurs manquantes pour CHAQUE colonne
manquants_par_colonne <- colSums(is.na(df))

# Création du tableau de synthèse (Nombre et Pourcentage)
bilan_manquants <- data.frame(
  Nb_Manquants = colSums(is.na(df)),
  Pourcentage  = round((colSums(is.na(df)) / nrow(df)) * 100, 2)
)

print(bilan_manquants)
print(manquants_par_colonne)


# ==============================================================================
#NETTOYAGE ET SUPPRESSION DES COLONNES INUTILES
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


# ==============================================================================
#IMPUTATIONS ET CORRECTIONS DES DONNÉES
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
#CORRECTION DES ENCODAGES CORROMPUS
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
#RECONSTRUCTION DES BOOLÉENS
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


#==============================================================================
#CORRECTION DES VALEURS ABERRANTES - PUISSANCE NOMINALE
# ==============================================================================

df_clean <- df_clean %>%
  # Conserver uniquement les puissances inférieures ou égales à 350 kW
  # (ce qui supprime automatiquement toutes les lignes supérieures à 350)
  filter(puissance_nominale <= 350)

# ==============================================================================
#SUPPRESSION DES DOUBLONS - garder la ligne la plus récente
# ==============================================================================

df_clean <- df_clean %>%
  mutate(date_maj = as.Date(date_maj)) %>%
  arrange(id_pdc_itinerance, desc(date_maj)) %>%
  distinct(id_pdc_itinerance, .keep_all = TRUE)

df_clean <- df_clean %>% #Si c'est gratuit, on force les moyens de paiement à 0.
  mutate(
    paiement_acte  = ifelse(gratuit == 1, 0, paiement_acte),
    paiement_cb    = ifelse(gratuit == 1, 0, paiement_cb),
    paiement_autre = ifelse(gratuit == 1, 0, paiement_autre)
    
  )
df_clean <- df_clean %>%
  # 1. Calculer le total des prises déclarées
  mutate(
    total_prises = prise_type_ef + prise_type_2 + prise_type_combo_ccs + 
      prise_type_chademo + prise_type_autre
  ) %>%
  
  # 2. SUPPRIMER si total_prises == 0 ET puissance_nominale > 0
  # (La condition ci-dessous ne GARDE que ce qui n'est PAS cette anomalie)
  filter(!(total_prises == 0 & puissance_nominale > 0)) %>%
  
  # 3. Supprimer la colonne temporaire
  select(-total_prises)
df_clean <- df_clean %>%
  filter(!(is.na(code_insee_commune) | is.na(consolidated_code_postal) | is.na(consolidated_commune)))
df_clean <- df_clean %>%
  mutate(date_mise_en_service = ifelse(
    as.Date(date_mise_en_service) < as.Date("2010-01-01"),
    NA,
    as.character(date_mise_en_service)
  ))
df_clean <- df_clean %>%
  mutate(restriction_gabarit = case_when(
    str_detect(restriction_gabarit, regex("aucune|aucun", ignore_case = TRUE)) ~ "Aucune restriction",
    str_detect(restriction_gabarit, regex("2[,.]3|2m3|barre", ignore_case = TRUE)) ~ "Hauteur max 2.3m",
    str_detect(restriction_gabarit, regex("2[,.]5", ignore_case = TRUE))           ~ "Hauteur max 2.5m",
    str_detect(restriction_gabarit, regex("inconnu|non pr|xx|^$", ignore_case = TRUE)) ~ "Inconnue",
    is.na(restriction_gabarit) ~ "Inconnue",
    TRUE ~ restriction_gabarit
  ))

