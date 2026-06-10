# ==============================================================================
# 03 - Uniformisation du tarif
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
    val_cts <- suppressWarnings(as.numeric(str_replace(match_cts[, 2], ",", ".")))
    is_decimal <- str_detect(match_cts[, 2], "[.,]") | (!is.na(val_cts) & val_cts < 1)
    val_cts <- ifelse(is_decimal, val_cts, val_cts / 100)
    resultat[!is.na(val_cts)] <- val_cts[!is.na(val_cts)]
  }
  
  # ----------------------------------------------------
  # RÈGLE 2 : Motif principal avec séparateur (ex: "0,38 €/kWh", "0.33334€ par kwh")
  # ----------------------------------------------------
  match1 <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(?:€|e)?\\s*(?:ttc|ht)?\\s*(?:/|par)\\s*kw\\s*/?\\s*h")
  val1 <- suppressWarnings(as.numeric(str_replace(match1[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val1)] <- val1[is.na(resultat) & !is.na(val1)]
  
  # ----------------------------------------------------
  # RÈGLE 3 : Motif sans séparateur direct (ex: "0,55€/kWh", "0.40€ kwh")
  # ----------------------------------------------------
  match2 <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*(?:€|e)\\s*(?:ttc|ht)?\\s*kw\\s*/?\\s*h")
  val2 <- suppressWarnings(as.numeric(str_replace(match2[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val2)] <- val2[is.na(resultat) & !is.na(val2)]
  
  # ----------------------------------------------------
  # RÈGLE 4 : Expressions textuelles ciblées (ex: "0.3 : prix au kwh")
  # ----------------------------------------------------
  match_au <- str_match(txt, "([0-9]+[.,][0-9]+|[0-9]+)\\s*:\\s*prix\\s*au\\s*kw\\s*/?\\s*h")
  val_au <- suppressWarnings(as.numeric(str_replace(match_au[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val_au)] <- val_au[is.na(resultat) & !is.na(val_au)]
  
  # ----------------------------------------------------
  # RÈGLE 5 : Nombre suivi de juste kw/h ou kwh (ex: "0.40 kw/h") - Seuil max de cohérence à 2€
  # ----------------------------------------------------
  match3 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*kw\\s*/?\\s*h")
  val3 <- suppressWarnings(as.numeric(str_replace(match3[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val3) & val3 < 2] <- val3[is.na(resultat) & !is.na(val3) & val3 < 2]
  
  # ----------------------------------------------------
  # RÈGLE 6 : Chiffre suivi de / kwh (ex: "0,2668 / kwh")
  # ----------------------------------------------------
  match4 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*/\\s*kw\\s*/?\\s*h")
  val4 <- suppressWarnings(as.numeric(str_replace(match4[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val4)] <- val4[is.na(resultat) & !is.na(val4)]
  
  # ----------------------------------------------------
  # RÈGLE 7 : Chiffres décimaux simples suivis de € (ex: "0.36€", "1,23€")
  # ----------------------------------------------------
  match5 <- str_match(txt, "([0-9]+[.,][0-9]+)\\s*(?:€|e)")
  val5 <- suppressWarnings(as.numeric(str_replace(match5[, 2], ",", ".")))
  resultat[is.na(resultat) & !is.na(val5) & val5 < 2] <- val5[is.na(resultat) & !is.na(val5) & val5 < 2]
  
  # ----------------------------------------------------
  # RÈGLE 8 : Chiffres décimaux isolés (ex: "0.38", "0,22") - CORRIGÉE
  # ----------------------------------------------------
  # On s'assure que txt n'est pas NA avant de chercher dedans
  is_pure_decimal <- !is.na(txt) & str_detect(txt, "^[0-9]+[.,][0-9]+$")
  val6 <- suppressWarnings(as.numeric(str_replace(txt, ",", ".")))
  
  # On s'assure que val6 est bien calculée et valide
  cond8 <- is.na(resultat) & is_pure_decimal & !is.na(val6) & (val6 < 2)
  resultat[cond8] <- val6[cond8]
  
  # ----------------------------------------------------
  # RÈGLE 9 : Bornes gratuites textuelles - CORRIGÉE
  # ----------------------------------------------------
  is_zero <- !is.na(txt) & (txt == "0" | str_detect(txt, "gratuit") | str_detect(txt, "0 pour utilisateur"))
  resultat[is.na(resultat) & is_zero] <- 0.0
  
  return(resultat)
}

# 3. Application des traitements sur le dataframe
df_clean <- df_clean %>%
  mutate(
    # A. On extrait les tarifs
    tarif_kwh_clean = extraire_tarif_kwh(tarification),
    
    # B. On force à 0 si la colonne 'gratuit' l'indique
    tarif_kwh_clean = ifelse(str_to_lower(gratuit) %in% c("1", "true", "vrai"), 0, tarif_kwh_clean)
  ) %>%
  # C. On supprime l'ancienne colonne
  select(-tarification)