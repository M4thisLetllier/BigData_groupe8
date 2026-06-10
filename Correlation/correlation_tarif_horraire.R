df_tarifs_clean <- read.csv("test1.csv", stringsAsFactors = FALSE, sep = ",")

df_etude <- df_tarifs_clean %>%
  # On ne garde que les lignes où les DEUX colonnes ont une valeur
  filter(!is.na(tarif_kwh_clean) & !is.na(puissance_nominale)) %>%
  # On s'assure que R comprend bien que ce sont des nombres
  mutate(
    tarif_kwh_clean = as.numeric(tarif_kwh_clean),
    puissance_nominale = as.numeric(puissance_nominale)
  )

