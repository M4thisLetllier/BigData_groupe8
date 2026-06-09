# ==============================================================================
# FONCTIONNALITÉ 2 : VISUALISATION GRAPHIQUE 
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. Chargement de la base
donnees_irve <- read.csv("IRVE_brouillon_100k.csv", stringsAsFactors = FALSE)

# ==============================================================================
# GRAPHIQUE 1 : PARTS DE MARCHÉ DES OPÉRATEURS (Le Top 10)
# ==============================================================================
top_operateurs <- donnees_irve %>%
  filter(!is.na(nom_operateur) & nom_operateur != "") %>% 
  group_by(nom_operateur) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  pull(nom_operateur)

df_top10 <- donnees_irve %>%
  filter(nom_operateur %in% top_operateurs) %>%
  mutate(gratuit = ifelse(gratuit == 1, "Gratuit", "Payant")) %>%
  group_by(nom_operateur, gratuit) %>%
  summarise(total = n(), .groups = 'drop')

graph_operateurs <- ggplot(df_top10, aes(x = reorder(nom_operateur, total, sum), y = total, fill = gratuit)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() + 
  scale_fill_manual(values = c("Gratuit" = "#27ae60", "Payant" = "#2980b9")) + # Vert émeraude et Bleu corporate
  theme_minimal(base_size = 13) + # Police légèrement plus grande
  labs(title = "Top 10 des opérateurs du réseau public",
       subtitle = "Répartition du parc selon le modèle économique",
       x = NULL, y = "Nombre de points de charge", fill = NULL) +
  theme(legend.position = "top", 
        axis.text.y = element_text(face = "bold", color = "#2c3e50"),
        plot.title = element_text(face = "bold"))

print(graph_operateurs)
ggsave("parts_marche_operateurs_v2.png", plot = graph_operateurs, width = 9, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 2 : RÉPARTITION DES PUISSANCES (Par Catégorie métier)
# ==============================================================================
df_puissance_cat <- donnees_irve %>%
  filter(implantation_station %in% c("Parking public", "Voirie", "Station dédiée à la recharge rapide")) %>%
  filter(!is.na(puissance_nominale)) %>%
  mutate(categorie_puissance = case_when(
    puissance_nominale < 22 ~ "1. Lente (< 22 kW)",
    puissance_nominale == 22 ~ "2. Standard (22 kW)",
    puissance_nominale > 22 & puissance_nominale <= 149 ~ "3. Rapide (23-149 kW)",
    puissance_nominale >= 150 ~ "4. Ultra-Rapide (150+ kW)",
    TRUE ~ "Autre"
  )) %>%
  filter(categorie_puissance != "Autre")

graph_puissance <- ggplot(df_puissance_cat, aes(x = categorie_puissance, fill = implantation_station)) +
  # position = "dodge" met les barres côte à côte au lieu de les empiler
  geom_bar(position = "dodge", color = "white", linewidth = 0.5) +
  scale_fill_brewer(palette = "Set2") + # Palette très élégante
  theme_minimal(base_size = 12) +
  labs(title = "Analyse de la puissance selon le lieu d'implantation",
       subtitle = "La voirie est dominée par le 22kW, la recharge rapide se fait en station.",
       x = NULL, y = "Volume d'équipements", fill = "Type d'implantation :") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

print(graph_puissance)
ggsave("repartition_puissances_v2.png", plot = graph_puissance, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 3 : ÉVOLUTION TEMPORELLE (Lissée)
# ==============================================================================
evolution_stations <- donnees_irve %>%
  filter(!is.na(date_mise_en_service) & date_mise_en_service != "") %>%
  mutate(date_propre = as.Date(substr(date_mise_en_service, 1, 10), format="%Y-%m-%d")) %>%
  filter(date_propre >= as.Date("2015-01-01") & date_propre <= as.Date("2026-05-31")) %>% # Début à 2015 pour enlever le plat inutile
  mutate(annee_mois = format(date_propre, "%Y-%m")) %>%
  count(annee_mois) %>%
  mutate(date_graphique = as.Date(paste0(annee_mois, "-01")))

graph_evolution <- ggplot(evolution_stations, aes(x = date_graphique, y = n)) +
  geom_area(fill = "#bdc3c7", alpha = 0.4) + # Fond gris doux
  geom_line(color = "#7f8c8d", alpha = 0.5) + # Ligne d'origine en filigrane
  geom_smooth(method = "loess", span = 0.1, color = "#c0392b", linewidth = 1.2, se = FALSE) + # Tendance rouge forte
  theme_minimal(base_size = 12) +
  labs(title = "Dynamique de déploiement du réseau IRVE (2015 - 2026)",
       subtitle = "En rouge : Courbe de tendance (Moyenne mobile des mises en service)",
       x = NULL, y = "Nouvelles stations par mois") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(face = "bold"))

print(graph_evolution)
ggsave("evolution_mises_en_service_v2.png", plot = graph_evolution, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 4 : RÉPARTITION DES TYPES DE PRISES (Avec Chiffres)
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
  geom_bar(stat = "identity", fill = "#34495e", width = 0.6) + # Gris ardoise élégant
  # Ajout des chiffres au-dessus des barres avec formatage millier (espace)
  geom_text(aes(label = format(Nombre, big.mark = " ")), vjust = -1, size = 4, fontface = "bold", color = "#2c3e50") +
  theme_minimal(base_size = 12) +
  # On étend un peu l'axe Y pour laisser de la place aux textes
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(title = "Équipement technologique du réseau",
       x = NULL, y = "Volume de prises installées") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank()) # Enlève les lignes verticales inutiles

print(graph_prises)
ggsave("repartition_types_prises_v2.png", plot = graph_prises, width = 8, height = 6, dpi = 300)