#Mathis et Gemini

source("BigData_groupe8/Nettoyage/main.R")

library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# ÉTAPE 1 : Préparation au niveau de la STATION (dédoublonnage)
# ---------------------------------------------------------
df_etude_stations <- df_clean %>%
  #On ne garde qu'une seule ligne par identifiant de station
  distinct(id_station_itinerance, .keep_all = TRUE) %>%
  
  # On filtre les valeurs manquantes comme avant
  filter(!is.na(implantation_station) & implantation_station != "" & !is.na(nbre_pdc)) %>%
  mutate(nbre_pdc = as.numeric(nbre_pdc))

# ---------------------------------------------------------
# ÉTAPE 2 : Le test statistique VÉRIFIÉ
# ---------------------------------------------------------
# Le calcul se fait désormais sur le vrai nombre de stations !
test_stat <- kruskal.test(nbre_pdc ~ as.factor(implantation_station), data = df_etude_stations)
print(test_stat)

# ---------------------------------------------------------
# ÉTAPE 3 : La visualisation (Boxplot) avec Zoom
# ---------------------------------------------------------
graphique_boxplot <- ggplot(df_etude_stations, aes(
  x = reorder(implantation_station, nbre_pdc, FUN = median), 
  y = nbre_pdc, 
  fill = implantation_station
)) +
  geom_boxplot(outlier.alpha = 0.5, outlier.color = "red") + 
  
  #On pivote le graphe et on limite l'axe Y de 0 à 70
  coord_flip(ylim = c(0, 20)) + 
  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Impact de la localisation sur la taille de la station",
    x = "Type d'implantation",
    y = "Nombre de points de charge dans la station"
    )

print(graphique_boxplot)