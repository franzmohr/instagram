rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- c("AT", "EA20")
file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry,
                                       coicop = c("FOOD", "NRG", "IGD_NNRG", "SERV")),
                        cache = FALSE)

weights <- weights %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry,
                                     coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV")),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values))

comp <- index %>%
  filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight),
         values = values * weight) %>%
  ungroup() %>%
  mutate(var_de = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Dienstleistungen", "Industriegüter ohne Energie",
                                    "Energie", "Nahrungsmittel")),
         var_en = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Services", "Non-energy industrial goods",
                                    "Energy", "Food including alcohol and tobacco")),
         geo = factor(geo, levels = ctry, labels = c("Österreich", "Euroraum (20)")))

line <- index %>%
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  mutate(line_de = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HVPI-Inflation", "Kerninflation (ohne Energie, Nahrungsmittel)")),
         line_en = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HCPI-inflation", "Core inflation (w/o energy, food)")))

source("theme_instagram.R")

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_line(aes(colour = geo), alpha = 1) +
  #geom_line(data = line, aes(linetype = line_de), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "6 months", date_labels = "%YM%m") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  facet_wrap(~var_de, ncol = 2) +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Inflation",
       subtitle = "Beiträge der Komponenten zur Gesamtinflation in Prozentpunkten",
       caption = "Quelle: Eurostat. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")


ggsave(g, filename = paste0("pics/", file_date, "_infl_comp_de.jpeg"), height = 5, width = 5)

