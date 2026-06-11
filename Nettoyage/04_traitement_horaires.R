# ==============================================================================
# 9. NORMALISATION DES HORAIRES
# ==============================================================================


# Liste de référence pour le format officiel OSM
osm_days  <- c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")
full_days <- c("MO", "TU", "WE", "TH", "FR", "SA", "SU")

# Fonction auxiliaire : fusionner les règles jour+horaire en plages continues
compresser_regles <- function(regles) {
  if (length(regles) == 0) return(character(0))
  
  # Parser chaque règle en (liste de jours, horaire)
  parsed <- lapply(regles, function(r) {
    r <- str_squish(r)
    
    # Extraction sécurisée de la partie jours (ex: "MO", "MO-FR", "MO,TU,WE")
    day_raw  <- str_extract(r, "^[A-Z]{2}(?:[-,\\s]+[A-Z]{2})*")
    
    if (is.na(day_raw)) return(list(days = integer(0), time = r))
    
    time_raw <- str_trim(str_sub(r, nchar(day_raw) + 1))
    
    # Résoudre les plages de jours (MO-FR → indices 1:5)
    if (str_detect(day_raw, "-")) {
      bounds <- str_trim(str_split(day_raw, "-")[[1]])
      i1 <- match(bounds[1], full_days)
      i2 <- match(bounds[2], full_days)
      if (!is.na(i1) && !is.na(i2) && i1 <= i2) {
        return(list(days = i1:i2, time = time_raw))
      }
    }
    
    # Résoudre les listes de jours séparés par des virgules (MO,TU)
    day_list <- str_trim(str_split(day_raw, ",")[[1]])
    idx <- sort(na.omit(match(day_list, full_days)))
    list(days = idx, time = time_raw)
  })
  
  # Grouper par horaire identique
  times_unique <- unique(sapply(parsed, `[[`, "time"))
  result <- c()
  
  for (t in times_unique) {
    # Sécurité : On ignore les plages horaires vides ou fantômes
    if (is.na(t) || str_trim(t) == "") next
    
    # Récupérer tous les jours associés à cet horaire précis
    all_days <- sort(unique(unlist(lapply(
      parsed[sapply(parsed, function(x) x$time == t)],
      `[[`, "days"
    ))))
    
    if (length(all_days) == 0) next
    
    # Regrouper en sous-plages continues
    groups <- list()
    grp <- all_days[1]
    for (i in seq_along(all_days)[-1]) {
      if (all_days[i] == tail(grp, 1) + 1) {
        grp <- c(grp, all_days[i])
      } else {
        groups <- c(groups, list(grp))
        grp <- all_days[i]
      }
    }
    groups <- c(groups, list(grp))
    
    # Écriture finale selon la charte stricte OpenStreetMap (CamelCase)
    for (g in groups) {
      if (length(g) == 1) {
        day_str <- osm_days[g]
      } else if (length(g) == 7) {
        day_str <- "Mo-Su"
      } else if (length(g) == (max(g) - min(g) + 1)) {
        # Jours consécutifs (ex: 1,2,3 -> Mo-We)
        day_str <- paste0(osm_days[min(g)], "-", osm_days[max(g)])
      } else {
        # Jours non-consécutifs (ex: 1,3,5 -> Mo,We,Fr)
        day_str <- paste(osm_days[g], collapse = ",")
      }
      result <- c(result, paste(day_str, t))
    }
  }
  result
}

normalize_opening_hours <- function(h, tol_minutes = 10) {
  if (is.na(h) || h == "") return(NA_character_)
  
  hn <- toupper(h)
  hn <- stri_trans_general(hn, "Latin-ASCII")
  hn <- str_replace_all(hn, "\\.(?=\\d)|(?<=\\d)\\.", ":")  # 08.00 → 08:00
  hn <- str_replace_all(hn, "(?<![:\\d])h(?![\\d])|(?<=[\\d])H(?=[\\d])", ":") # 8h00 → 8:00
  hn <- str_replace_all(hn, "\\s+", " ")
  hn <- str_trim(hn)
  
  # Traduction des jours FR → EN
  replacements <- c(
    "\\bDU\\b" = "", "\\bAU\\b" = "",
    "\\bLUN(?:DI)?\\b" = "MO", "\\bMAR(?:DI)?\\b" = "TU",
    "\\bMER(?:CREDI)?\\b" = "WE", "\\bJEU(?:DI)?\\b" = "TH",
    "\\bVEN(?:DREDI)?\\b" = "FR", "\\bSAM(?:EDI)?\\b" = "SA",
    "\\bDIM(?:ANCHE)?\\b" = "SU",
    "\\bSUN\\b" = "SU", "\\bSAT\\b" = "SA",
    "(?<![A-Z])\\bT\\b(?!U|H|E)" = "TH"
  )
  for (pat in names(replacements)) {
    hn <- str_replace_all(hn, regex(pat, ignore_case = FALSE), replacements[pat])
  }
  
  # ==========================================================================
  # SMART PARSING DES VIRGULES : Distinguer les virgules de jours et de blocs
  # ==========================================================================
  # Si une virgule est entourée de noms de jours sans chiffre au milieu, on la protège
  for (i in 1:5) {
    hn <- str_replace_all(hn, "(\\b(MO|TU|WE|TH|FR|SA|SU)\\b[^0-9;]*),([^0-9;]*(?=\\b(MO|TU|WE|TH|FR|SA|SU)\\b))", "\\1@@@\\3")
  }
  # Les virgules restantes séparent de vrais blocs d'heures -> deviennent des points-virgules
  hn <- str_replace_all(hn, ",", ";")
  # On restaure nos virgules de jours légitimes
  hn <- str_replace_all(hn, "@@@", ",")
  # ==========================================================================
  
  # Normaliser les formats d'heures et de séparateurs
  hn <- str_replace_all(hn, "24H/24|24/24", "00:00-24:00")
  hn <- str_replace_all(hn, "7J/7", "00:00-24:00")
  hn <- str_replace_all(hn, "(\\d{2}) (\\d{2})(?=[-;\\s]|$)", "\\1:\\2")
  hn <- str_replace_all(hn, "(?<=[\\s;,\\-]|^)(\\d):(\\d{2})", "0\\1:\\2")
  hn <- str_squish(hn)
  
  # Détection automatique des plages quasi-24h (24/7)
  tm <- str_extract_all(hn, "\\d{1,2}:\\d{2}")[[1]]
  if (length(tm) >= 2) {
    starts <- as.integer(sub(":.*", "", tm[seq(1, length(tm), by=2)]))*60 +
      as.integer(sub(".*:", "", tm[seq(1, length(tm), by=2)]))
    ends   <- as.integer(sub(":.*", "", tm[seq(2, length(tm), by=2)]))*60 +
      as.integer(sub(".*:", "", tm[seq(2, length(tm), by=2)]))
    ends[ends == 0] <- 1440
    if (!is.na(min(starts)) && min(starts) <= tol_minutes &&
        !is.na(max(ends))   && max(ends)   >= 1440 - tol_minutes) {
      idx <- order(starts)
      ok  <- TRUE
      for (i in seq_along(idx)[-length(idx)]) {
        if (starts[idx[i+1]] - ends[idx[i]] > tol_minutes) { ok <- FALSE; break }
      }
      if (ok) return("24/7")
    }
  }
  
  # Découper en sous-règles propres et compresser
  parts  <- str_trim(str_split(hn, ";")[[1]])
  parts  <- parts[parts != ""]
  result <- compresser_regles(parts)
  
  if (length(result) == 0) return(NA_character_)
  str_squish(paste(result, collapse = "; "))
}

# --- Application de la mise à jour sur votre dataframe ---
horaires_unique <- df_clean %>%
  distinct(horaires) %>%
  rename(Origine = horaires) %>%
  mutate(Normalise = sapply(Origine, normalize_opening_hours))

df_clean <- df_clean %>%
  left_join(horaires_unique %>% select(Origine, Normalise),
            by = c("horaires" = "Origine")) %>%
  mutate(horaires = if_else(!is.na(Normalise), Normalise, horaires)) %>%
  select(-Normalise)

# Filtrer et supprimer définitivement les scories irrécupérables
df_clean <- df_clean %>%
  filter(is.na(horaires) | !str_detect(horaires, "^T;|^T "))