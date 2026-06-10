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

# Supprimer les lignes où les horaires sont corrompus (commencent par "T;")
df_clean <- df_clean %>%
  filter(is.na(horaires) | !str_detect(horaires, "^T;"))

