rm(list = ls())

library(dplyr)
library(eurostat)
library(tidyr)
library(zoo)

source("theme_instagram.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = "AT",
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("NSA")))

var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PX")
var_labels_de <- c(c("Privater Konsum",
                     "Bruttoinvestitionen",
                     "Öffentlicher Konsum",
                     #"Nettoexporte",
                     "Exporte", "Importe",
                     "Sonstige"))
var_labels_en <- c(c("Private consumption",
                     "Gross investment",
                     "Public consumption",
                     #"Nettoexporte",
                     "Exports", "Imports",
                     "Other"))


temp <- raw %>%
  mutate(date = as.yearqtr(time),
         values = values / 1000) %>%
  select(date, na_item, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  #mutate(PNX = P6 - P7) %>%
  #select(-P6, -P7) %>%
  na.omit() %>%
  #mutate(PX = B1GQ - (P3_S13 + P31_S14_S15 + P5G + PNX),
  #mutate(PX = B1GQ - (P3_S13 + P31_S14_S15 + P5G + P6 - P7)) %>%
  mutate(P7 = -P7) %>%
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
  filter(date >= "2015 Q1") %>%
  filter(!is.na(values)) %>%
  rename(value = values)


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_de)) +
  geom_line(data = real_agg, aes(colour = "Gesamt"), size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_colour_manual(values = "black") +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Reales Bruttoinlandsprodukt (Österreich)",
       subtitle = "Mrd EUR",
       caption = "Quelle: Eurostat. Unbereinigte Daten. Quartalswerte.") +
  theme_instagram

ggsave(g, filename = "pics/20200704_gdp_components_de.jpeg", height = 5, width = 5)


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_en)) +
  geom_line(data = real_agg, aes(colour = "Total"), size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_colour_manual(values = "black") +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Real gross domestic product (Austria)",
       subtitle = "Bn EUR",
       caption = "Source: Eurostat. Unadjusted, quarterly data.") +
  theme_instagram

ggsave(g, filename = "pics/20200704_gdp_components_en.jpeg", height = 5, width = 5)

