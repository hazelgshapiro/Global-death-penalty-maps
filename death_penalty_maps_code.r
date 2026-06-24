## Data Visualization (GOVT16-QSS17) Fall 2025
## Project 2
##
## Name: Hazel Shapiro
## Date: November 14, 2025

library(tidyverse)
library(readxl)
library(sf)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(patchwork)
library(showtext)
font_add_google("Montserrat", "Montserrat")
showtext_auto()

deathpenalty <- read_excel("/Users/hazelshapiro/Downloads/project2dataset.xlsx")
view(deathpenalty)
unique(deathpenalty$Deathpenalty)
# Deathpenalty: 0 = Abolished, 
# 1 = Abolished for ordinary crimes only, 
# 2 = Abolished for ordinary crimes only but used within last 10 year, 
# 3 = Abolished in practice, 
# 4 = Retained
unique(deathpenalty$Deathpenalty_lag1)

deathpenalty_clean <- deathpenalty %>% mutate(
  Deathpenalty_label = recode_factor(Deathpenalty, '0' = "Abolished",
                                     '1' = "Abolished for ordinary crimes only",
                                     '2' = "Abolished but used in last 10 years",
                                     '3' = "Abolished in practice",
                                     '4' = "Retained"),
  Deathpenalty_label = factor(Deathpenalty_label, levels = c("Abolished",
                                                             "Abolished for ordinary crimes only",
                                                             "Abolished but used in last 10 years",
                                                             "Abolished in practice",
                                                             "Retained")),
  Flag_change = ifelse(Flag_change == 1, "Yes", "No"),
  Direction_change = recode(Direction_change, '-1' = "More restricted use",
                            '0' = "No change",
                            '1' = "Less restricted use"))

death_historic <- deathpenalty_clean %>% filter(Year == 1990)
world_hist <- ne_countries(returnclass = "sf", scale = "medium")

death_modern <- deathpenalty_clean %>% filter(Year == 2022)
world_modern <- ne_countries(returnclass = "sf", scale = "medium")

clean_names <- function(df) {df %>%
    mutate(Country = str_to_title(Country)) %>%
    mutate(Country = case_when(
      Country == "United States Of America" ~ "United States of America",
      Country == "Democratic Republic Of Congo" ~ "Dem. Rep. Congo",
      Country == "Republic Of Congo" ~ "Congo",
      Country == "Côte D'ivoire" ~ "Côte d'Ivoire",
      Country == "Equatorial G" ~ "Eq. Guinea",
      Country == "Papua N.Guinea" ~ "Papua New Guinea",
      Country == "Trinidad&Tobago" ~ "Trinidad and Tobago",
      Country == "St.Kitts&Nevis" ~ "St. Kitts and Nevis",
      Country == "St.Lucia" ~ "St. Lucia",
      Country == "St.Vincent&Gre" ~ "St. Vincent and the Grenadines",
      Country == "Czech Republic" ~ "Czechia",
      Country == "Eswatini" ~ "eSwatini",
      Country == "North Macedonia" ~ "North Macedonia",
      Country == "Korea North" ~ "North Korea",
      Country == "Korea South" ~ "South Korea",
      Country == "East Timor" ~ "Timor-Leste",
      Country == "Ussr" ~ NA_character_,
      Country == "Czechoslovakia" ~ NA_character_,
      TRUE ~ Country)) %>%
    filter(!is.na(Country))}

death_historic <- clean_names(death_historic)
death_modern   <- clean_names(death_modern)

death_historic %>% count(Deathpenalty_label)
death_modern %>% count(Deathpenalty_label)

map_hist <- world_hist %>% 
  left_join(death_historic, by = c("name" = "Country"))
map_modern <- world_modern %>% 
  left_join(death_modern, by = c("name" = "Country"))

colors <- c("Abolished" = "#4c9f70",
  "Abolished for ordinary crimes only" = "#a8ddb5",
  "Abolished but used in last 10 years" = "#fdd49e",
  "Abolished in practice" = "#fc8d59",
  "Retained" = "#d73027")

map1990 <- ggplot(map_hist) +
  geom_sf(aes(fill = Deathpenalty_label), color = "white", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "lightgray") +
  labs(title = "Global Death Penalty Status in 1990 vs. 2022",
       fill = "Death Penalty Status:",
       subtitle = "1990 map:") +
  theme_minimal()
 
map2022 <- ggplot(map_modern) +
  geom_sf(aes(fill = Deathpenalty_label), color = "white", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "lightgray") +
  labs(fill = "Death Penalty Status:",
       subtitle = "2022 map:",
       caption = "Data: CDPD (2024). Note: Some small states and territories are not shown due to missing or inconsistent country names across the datasets.") +
  theme_minimal()
 
theme <- theme(text = element_text(family = "Montserrat"),
  legend.title = element_text(size = 12),
  legend.text  = element_text(size = 12),
  legend.position = "bottom",
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
  plot.caption = element_text(size = 8, face = "italic", hjust = 0.5))

map1990 <- map1990 + theme
map2022 <- map2022 + theme

(map1990/map2022 + plot_layout(guides = "collect")) &
  theme(legend.position = "bottom")
