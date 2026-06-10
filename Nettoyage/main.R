# ==============================================================================
# FICHIER : main.R
# ==============================================================================

# 1. On charge toutes les librairies une seule fois ici
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


cat("Début du traitement des données IRVE...\n")

# On cherche d'abord si le traitement est entièrement fini, sinon on remonte

if (file.exists("donnees_finales.rds")) {
  cat("La sauvegarde finale (Étape 4 terminée) existe. Chargement...\n")
  df_clean <- readRDS("donnees_finales.rds")
  etape_depart <- 5 # Tout est déjà fait !
  
} else if (file.exists("donnees_etape3.rds")) {
  cat("Étape 3 trouvée. Chargement et reprise à l'Étape 4 (Horaires)...\n")
  df_clean <- readRDS("donnees_etape3.rds")
  etape_depart <- 4
  
} else if (file.exists("donnees_etape2.rds")) {
  cat("Étape 2 trouvée. Chargement et reprise à l'Étape 3 (Tarifs)...\n")
  df_clean <- readRDS("donnees_etape2.rds")
  etape_depart <- 3
  
} else if (file.exists("donnees_etape1.rds")) {
  cat("Étape 1 trouvée. Chargement et reprise à l'Étape 2 (Géographie)...\n")
  df_clean <- readRDS("donnees_etape1.rds")
  etape_depart <- 2
  
} else {
  cat("Aucune sauvegarde trouvée. Lancement global depuis le début...\n")
  etape_depart <- 1
}


# ==============================================================================
# EXÉCUTION EN CASCADE DES ÉTAPES MANQUANTES
# ==============================================================================

# ---------------------------------------------------------
# ÉTAPE 1 : Nettoyage de base
# ---------------------------------------------------------
if (etape_depart <= 1) {
  cat("Lancement de l'étape 1 : Nettoyage initial...\n")
  cat("Lecture du csv... \n")
  df <- read.csv("IRVE.csv")
  source("BigData_groupe8/Nettoyage/01_import_et_nettoyage.R") 
  saveRDS(df_clean, "donnees_etape1.rds") 
}

# ---------------------------------------------------------
# ÉTAPE 2 : Géocodage / Cohérence Géographique
# ---------------------------------------------------------
if (etape_depart <= 2) {
  cat("Lancement de l'étape 2 : Vérification géographique (Centroïdes)...\n")
  source("BigData_groupe8/Nettoyage/02_traitement_geo.R") 
  saveRDS(df_clean, "donnees_etape2.rds") 
}

# ---------------------------------------------------------
# ÉTAPE 3 : Traitement des tarifs
# ---------------------------------------------------------
if (etape_depart <= 3) {
  cat("Lancement de l'étape 3 : Nettoyage de la tarification...\n")
  source("BigData_groupe8/Nettoyage/03_traitement_tarifs.R") 
  saveRDS(df_clean, "donnees_etape3.rds") 
}

# ---------------------------------------------------------
# ÉTAPE 4 : Traitement des horaires
# ---------------------------------------------------------
if (etape_depart <= 4) {
  cat("Lancement de l'étape 4 : Normalisation des horaires...\n")
  source("BigData_groupe8/Nettoyage/04_traitement_horaires.R") 
  # Sauvegardes finales du projet
  saveRDS(df_clean, "donnees_finales.rds")
  write.csv(df_clean, "IRVE_clean_FINAL.csv", row.names = FALSE)
}
