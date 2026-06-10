# ==============================================================================
# FONCTIONNALITÉ 2 : VISUALISATION GRAPHIQUE 
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. Chargement de la base
donnees_irve <- read.csv("IRVE_brouillon_100k.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# RUSTINE DE NETTOYAGE : Correction du piège des Watts vs KiloWatts
# Objectif : Ramener les volumes à l'échelle de la France entière
# ------------------------------------------------------------------------------
donnees_irve <- donnees_irve %>%
  mutate(puissance_nominale = ifelse(puissance_nominale > 1000, puissance_nominale / 1000, puissance_nominale)) %>%
  filter(puissance_nominale <= 350 & puissance_nominale > 0)

# ==============================================================================
# GRAPHIQUE 1 : PARTS DE MARCHÉ (Méthode de l'Annotation Explicite)
# ==============================================================================

# 1. On calcule d'abord qui sont les 10 plus gros opérateurs
top_operateurs <- donnees_irve %>%
  filter(!is.na(nom_operateur) & nom_operateur != "") %>% 
  group_by(nom_operateur) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  pull(nom_operateur)

# 2. On prépare les données pour ces 10 opérateurs
df_top10 <- donnees_irve %>%
  filter(nom_operateur %in% top_operateurs) %>%
  mutate(gratuit = ifelse(gratuit == 1, "Gratuit", "Payant")) %>%
  group_by(nom_operateur, gratuit) %>%
  summarise(total = n(), .groups = 'drop')

# 3. On génère le graphique
graph_operateurs <- ggplot(df_top10, aes(x = reorder(nom_operateur, total, sum), y = total, fill = gratuit)) +
  # Retour aux barres empilées pures, sans bordure blanche parasite
  geom_bar(stat = "identity", width = 0.6) +
  # Affichage des gros chiffres blancs à l'intérieur des barres
  geom_text(aes(label = ifelse(total > 100, format(total, big.mark = " "), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  
  # L'ARME SECRÈTE : L'annotation manuelle qui pointe directement sur le bout de la barre
  annotate("text", x = "TotalEnergies Marketing France", y = 3450, 
           label = "← 17 gratuites", color = "#00e676", fontface = "bold", size = 4.5, hjust = 0) +
  
  coord_flip() + 
  # Un vert hyper saturé (#00e676) pour survivre au vidéoprojecteur
  scale_fill_manual(values = c("Gratuit" = "#00e676", "Payant" = "#2980b9")) + 
  theme_minimal(base_size = 13) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  labs(title = "Top 10 des opérateurs du réseau public",
       subtitle = "Répartition du parc selon le modèle économique",
       x = "Opérateurs", y = "Nombre total de points de charge", fill = "Modèle :") +
  theme(legend.position = "top", 
        axis.text.y = element_text(face = "bold", color = "#2c3e50"),
        plot.title = element_text(face = "bold"))

print(graph_operateurs)
ggsave("parts_marche_operateurs_finale.png", plot = graph_operateurs, width = 10, height = 7, dpi = 300)


# ==============================================================================
# GRAPHIQUE 2 : RÉPARTITION DES PUISSANCES (Axes ultra-explicites)
# ==============================================================================
df_puissance_agg <- donnees_irve %>%
  filter(implantation_station %in% c("Parking public", "Voirie", "Station dédiée à la recharge rapide")) %>%
  filter(!is.na(puissance_nominale)) %>%
  mutate(categorie_puissance = case_when(
    puissance_nominale < 22 ~ "1. Lente (< 22 kW)",
    puissance_nominale >= 22 & puissance_nominale <= 24 ~ "2. Standard (22 kW)",
    puissance_nominale > 24 & puissance_nominale <= 149 ~ "3. Rapide (50-120 kW)",
    puissance_nominale >= 150 ~ "4. Ultra-Rapide (150+ kW)",
    TRUE ~ "Autre"
  )) %>%
  filter(categorie_puissance != "Autre") %>%
  group_by(categorie_puissance, implantation_station) %>%
  summarise(nombre = n(), .groups = 'drop')

graph_puissance <- ggplot(df_puissance_agg, aes(x = categorie_puissance, y = nombre, fill = implantation_station)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "white", width = 0.8) +
  geom_text(aes(label = format(nombre, big.mark = " ")), 
            position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 3.5, color = "#2c3e50") +
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  # LA CORRECTION : Des titres d'axes impossibles à confondre
  labs(title = "Analyse de la puissance selon le lieu d'implantation",
       subtitle = "Les catégories absorbent les variations d'encodage (ex: 22.08 kW reclassé en 22 kW)",
       x = "Puissance Nominale des bornes (en KiloWatts)", 
       y = "Nombre de bornes (Quantité installée)", 
       fill = "Type d'implantation :") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

print(graph_puissance)
ggsave("repartition_puissances_finale.png", plot = graph_puissance, width = 11, height = 7, dpi = 300)
# ==============================================================================
# GRAPHIQUE 3 : ÉVOLUTION TEMPORELLE
# ==============================================================================
evolution_stations <- donnees_irve %>%
  filter(!is.na(date_mise_en_service) & date_mise_en_service != "") %>%
  mutate(date_propre = as.Date(substr(date_mise_en_service, 1, 10), format="%Y-%m-%d")) %>%
  filter(date_propre >= as.Date("2015-01-01") & date_propre <= as.Date("2026-05-31")) %>%
  mutate(annee_mois = format(date_propre, "%Y-%m")) %>%
  count(annee_mois) %>%
  mutate(date_graphique = as.Date(paste0(annee_mois, "-01")))

graph_evolution <- ggplot(evolution_stations, aes(x = date_graphique, y = n)) +
  geom_area(fill = "#bdc3c7", alpha = 0.4) + 
  geom_line(color = "#7f8c8d", alpha = 0.5) + 
  geom_smooth(method = "loess", span = 0.1, color = "#c0392b", linewidth = 1.2, se = FALSE) + 
  theme_minimal(base_size = 12) +
  labs(title = "Dynamique de déploiement du réseau IRVE (2015 - 2026)",
       subtitle = "En rouge : Courbe de tendance (Moyenne mobile des mises en service)",
       x = NULL, y = "Nouvelles stations par mois") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(face = "bold"))

print(graph_evolution)
ggsave("evolution_mises_en_service_finale.png", plot = graph_evolution, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 4 : RÉPARTITION DES TYPES DE PRISES 
# ==============================================================================
tableau_prises <- data.frame(
  Equipement = c("Type 2 (Standard AC)", "Combo CCS (Rapide DC)", "Prise Domestique", "CHAdeMO", "Autre"),
  Nombre = c(
    sum(donnees_irve$prise_type_2 == 1, na.rm = TRUE),
    sum(donnees_irve$prise_type_combo_ccs == 1, na.rm = TRUE),
    sum(donnees_irve$prise_type_ef == 1, na.rm = TRUE),
    sum(donnees_irve$prise_type_chademo == 1, na.rm = TRUE),
    sum(donnees_irve$prise_type_autre == 1, na.rm = TRUE)
  )
)

graph_prises <- ggplot(tableau_prises, aes(x = reorder(Equipement, -Nombre), y = Nombre)) +
  geom_bar(stat = "identity", fill = "#34495e", width = 0.6) + 
  geom_text(aes(label = format(Nombre, big.mark = " ")), vjust = -1, size = 4, fontface = "bold", color = "#2c3e50") +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(title = "Équipement technologique du réseau",
       x = NULL, y = "Volume de prises installées") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

print(graph_prises)
ggsave("repartition_types_prises_finale.png", plot = graph_prises, width = 8, height = 6, dpi = 300)