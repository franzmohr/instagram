rm(list = ls())

library(dplyr)
library(oenb)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

date_title <- format(Sys.Date(), "%Y%m%d")

# Inflation
inflation <- get_eurostat(id = "prc_hicp_midx",
                              filters = list(geo = "AT",
                                             unit = "I15",
                                            coicop = "CP00"),
                              cache = FALSE) %>%
  mutate(date = as.yearqtr(time)) %>%
  arrange(date) %>%#
  rename(ctry = geo) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarise(value = mean(values),
            name = "infl",
            .groups = "drop")

# House prices
hp <- oenb_data(id = "6", pos = c("VDBPLIMOPATGEZBN", "VDBPLIMOPAT00", "VDBPLIMOPWIEN00"), freq = "Q") %>%
  arrange(period) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.yearqtr(period, "%Y-Q%q")) %>%
  mutate(name = case_when(pos == "VDBPLIMOPATGEZBN" ~ "gesamt",
                          pos == "VDBPLIMOPAT00" ~ "wo_wien",
                          pos == "VDBPLIMOPWIEN00" ~ "wien")) %>%
  select(date, value, name)

# ATX
# atx <- read.csv2("https://www.wienerborse.at/indizes/aktuelle-indexwerte/historische-daten/?ISIN=AT0000A09FJ6&ID_NOTATION=22786867&c7012%5BDATETIME_TZ_END_RANGE%5D=13.06.2024&c7012%5BDATETIME_TZ_START_RANGE%5D=01.01.2000&c7012%5BDOWNLOAD%5D=csv") %>%
#   rename(date = Datum,
#          value = Schlusspreis) %>%
#   select(date, value) %>%
#   mutate(name = "atx",
#          date = as.Date(date, "%d.%m.%Y"),
#          date = as.yearqtr(date)) %>%
#   group_by(date, name) %>%
#   summarise(value = mean(value)) %>%
#   #slice(n()) %>%
#   ungroup()
# Same as inflation between 2010 and 2016

temp <- bind_rows(inflation, hp) %>%
  pivot_wider() %>%
  arrange(date) %>%
  na.omit() %>%
  filter(date >= "2005 Q1") %>%
  pivot_longer(cols = -c("date")) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(idx = value / value[1] * 100) %>%
  ungroup() %>%
  mutate(name_de = factor(name, levels = c("infl", "gesamt", "wo_wien", "wien", "atx"),
                          labels = c("Verbraucherpreisindex", "Immopreise: Österreich",
                                     "Immopreise: Österreich ohne Wien", "Immopreise: Wien", "ATX TR")),
         name_en = factor(name, levels = c("infl", "gesamt", "wo_wien", "wien"),
                          labels = c("Consumer prices", "House prices - Austria",
                                     "House prices - Austria w/o Vienna", "House prices - Vienna")))

# Plotting

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = idx, colour = name_de)) +
  geom_hline(yintercept = 100, colour = "black") +
  geom_line(linewidth = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "Immobilienpreise und Inflation in Österreich",
       subtitle = "Index (2005Q1 = 100)",
       caption = "Quelle: Eurostat, OeNB. Eigene Berechnungen.\nCode unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = paste0("pics/", date_title, "_house_prices_and_inflation_in_at_de.jpeg"), height = 5, width = 5)

