rm(list = ls())

library(alfred)
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

# Download data
eurostat <- get_eurostat(id = "prc_hpi_q", filters = list(geo = "AT",
                                                     purchase = c("TOTAL"))) %>%
  #filter(!is.na(values)) %>%
  mutate(date = as.yearqtr(time)) %>%
  pivot_wider(names_from = "unit", values_from = "values") %>%
  mutate(value = RCH_A / 100,
         name = "eurostat") %>%
  select(date, name, value)


temp <- tempfile(fileext = "xlsx")
download.file("https://www.oenb.at/dam/jcr:0415c088-eb4d-41d6-8e2f-d6ba4b5b0bb6/Wohnimmobilienpreisdaten_1Q2020.xlsx",
              destfile = temp, mode = "wb")

oenb <- readxl::read_excel(temp, sheet = "Quartalswerte", 
                           na = "x", skip = 8)[, c(1, 2, 3, 9)]

unlink(temp)

names(oenb) <- c("date", "gesamt", "wo_wien", "wien")

oenb <- oenb %>%
  pivot_longer(cols = -c("date")) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(value = value / lag(value, 4) - 1,
         date = as.yearqtr(date))

us <- get_fred_series("CSUSHPISA", series_name = "value", observation_start = "2000-01-01") %>%
  filter(substring(date, 6, 7) %in% c("03", "06", "09", "12")) %>%
  mutate(value = value / lag(value, 4) - 1,
         date = as.yearqtr(date),
         name = "us")

temp <- bind_rows(eurostat, oenb, us) %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  na.omit() %>%
  pivot_longer(cols = -c("date")) %>%
  filter(name %in% c("eurostat", "gesamt")) %>%
  mutate(name_de = factor(name, levels = c("eurostat", "gesamt", "us",
                                           "wo_wien", "wien"),
                          labels = c("Eurostat", "OeNB", "USA",
                                     "Österreich ohne Wien (OeNB)", "Wien (OeNB)")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name_de)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "Wohnimmobilienpreise (Österreich)",
       subtitle = "Jahreswachstum in Prozent",
       caption = "Quelle: Eurostat, Oesterreichische Nationalbank (OeNB).") +
  scale_colour_insta +
  #scale_colour_viridis(discrete = TRUE) +
  theme_instagram

ggsave(g, filename = "pics/20200715_house_price_growth_de.jpeg", height = 5, width = 5)

