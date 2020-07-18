rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(viridis)
library(zoo)

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = "AT",
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")))


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "PNX", "PX")
var_labels_de <- c(c("Privater Konsum",
                     "Bruttoinvestitionen",
                     "Öffentlicher Konsum",
                     "Nettoexporte",
                     #"Exporte", "Importe",
                     "Sonstige"))
var_labels_en <- c(c("Private consumption",
                     "Gross investment",
                     "Public consumption",
                     "Net exports",
                     #"Exports", "Imports",
                     "Other"))

temp <- raw %>%
  mutate(date = as.yearqtr(time)) %>%
  select(date, na_item, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  mutate(PNX = P6 - P7) %>%
  select(-P6, -P7) %>%
  na.omit() %>%
  mutate(PX = B1GQ - (P3_S13 + P31_S14_S15 + P5G + PNX),
         #P7 = -P7,
         P3_S13 = P3_S13 - lag(P3_S13, 4),
         P31_S14_S15 = P31_S14_S15 - lag(P31_S14_S15, 4),
         P5G = P5G - lag(P5G, 4),
         PNX = PNX - lag(PNX, 4),
         #P6 = P6 - lag(P6, 4),
         #P7 = P7 - lag(P7, 4),
         PX = PX - lag(PX, 4),
         P3_S13 = P3_S13 / lag(B1GQ, 4),
         P31_S14_S15 = P31_S14_S15 / lag(B1GQ, 4),
         P5G = P5G / lag(B1GQ, 4),
         PNX = PNX / lag(B1GQ, 4),
         #P6 = P6 / lag(B1GQ, 4),
         #P7 = P7 / lag(B1GQ, 4),
         PX = PX / lag(B1GQ, 4)) %>%
  select(-B1GQ) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(date >= "2015 Q1") %>%
  filter(!is.na(value)) %>%
  mutate(name_de = factor(name, levels = var_levels,
                          labels = var_labels_de),
         name_en = factor(name, levels = var_levels,
                          labels = var_labels_en))

real_agg <- temp %>%
  filter(na_item == "B1GQ") %>%
  mutate(values = values / lag(values, 4) - 1) %>%
  filter(date >= "2015 Q1") %>%
  filter(!is.na(values)) %>%
  rename(value = values)


source("theme_instagram.R")

g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_de), alpha = 1) +
  geom_line(data = real_agg, aes(colour = "Gesamt"), size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = "#F8DE7E") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Beitrag zum realen BIP-Wachstum (Österreich)",
       subtitle = "Änderung in Prozentpunkten im Vergleich zum Vorjahreswert",
       caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten.") +
  theme_instagram

ggsave(g, filename = "pics/20200705_gdp_component_growth_de.jpeg", height = 5, width = 5)


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_en), alpha = 1) +
  geom_line(data = real_agg, aes(colour = "Total"), size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = "darkgrey") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Contribution to real GDP growth (Austria)",
       subtitle = "Percentage point change compared to value in the previous year",
       caption = "Source: Eurostat. Seasonally and calendar adjusted data.") +
  theme_instagram

ggsave(g, filename = "pics/20200705_gdp_components_growth_en.jpeg", height = 5, width = 5)

