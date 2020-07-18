rm(list = ls())

library(dplyr)
library(ggplot2)
library(mFilter)
library(OECD) # install.packages("OECD")
library(tidyr)
library(zoo)

# OECD: Historical series, VOBARSA (volume estimates, national currency)
# Eurostat equivalent: Chain linked volumes (2010), million euro
y <- get_dataset("QNA", filter = "AUT.B1_GE.VOBARSA.Q") %>%
  select(obsTime, obsValue) %>%
  mutate(y = (obsValue / 4) / 1000) %>%
  select(-obsValue)

hp <- mFilter::hpfilter(y$y, freq = 1600)

temp <- tibble(date = as.yearqtr(y$obsTime, "%Y-Q%q"),
               trend = hp$trend[, 1],
               actual = y$y) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(date >= "2000 Q1") %>%
  mutate(name_de = factor(name, levels = c("actual", "trend"),
                          labels = c("Tatsächlicher Wert", "Geschätzter Trend")),
         name_en = factor(name, levels = c("actual", "trend"),
                          labels = c("Actual", "Estimated trend")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name_de)) +
  geom_line(size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  labs(title = "Reales Bruttoinlandsprodukt (Österreich)",
       subtitle = "Mrd EUR",
       caption = "Quelle: OECD. Quartalsdaten (2010 Preise). Hodrick-Prescott-Filter (\u03BB = 1600).") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200717_trend_cycle_de.jpeg", height = 5, width = 5)



g <- ggplot(temp, aes(x = date, y = value, colour = name_en)) +
  geom_line(size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  labs(title = "Real gross domestic product (Austria)",
       subtitle = "Bn EUR",
       caption = "Source: OECD. Quarterly data (2010 prices). Hodrick-Prescott-filter (\u03BB = 1600).") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200717_trend_cycle_en.jpeg", height = 5, width = 5)
