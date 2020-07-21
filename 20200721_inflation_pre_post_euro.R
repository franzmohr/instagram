rm(list = ls())

library(dplyr)
library(ggplot2)
library(OECD) # install.packages("OECD")
library(tidyr)
library(zoo)

#### Inflation ####
p <- get_dataset("PRICES_CPI", filter = "AUT.CPALTT01.IXOB.Q") %>%
  select(obsTime, obsValue) %>%
  rename(value = obsValue) %>%
  mutate(date = as.yearqtr(obsTime, "%Y-Q%q"),
         growth = value / lag(value, 4) - 1) %>%
  filter(!is.na(growth))

t_diff <- 20 * 4

pre <- p %>%
  filter(date < "1999 Q1") %>%
  tail(t_diff) %>%
  mutate(type = "pre")

post <- p %>%
  filter(date >= "1999 Q1") %>%
  slice(1:t_diff) %>%
  mutate(type = "post")

temp <- bind_rows(pre, post) %>%
  group_by(type) %>%
  mutate(time = 1:n() / 4,
         index = value / value[1] * 100) %>%
  ungroup() %>%
  mutate(type_de = factor(type, levels = c("pre", "post"),
                          labels = c("20 Jahre vor Einführung des Euro\n(1979Q1 bis 1998Q4)",
                                     "20 Jahre ab Einführung des Euro\n(1999Q1 bis 2018Q4)")),
         type_en = factor(type, levels = c("pre", "post"),
                          labels = c("20 years before introduction\nof the euro (1979Q1 to 1998Q4)",
                                     "20 years after introduction\nof the euro(1999Q1 to 2018Q4)")))

source("theme_instagram.R")
# Deutsch
g <- ggplot(temp, aes(x = time, y = index, colour = type_de)) +
  geom_line(size = 1.2) +
  scale_x_continuous(expand = c(.01, .1)) +
  labs(title = "Inflation vor und nach Einführung des Euro (Österreich)",
       subtitle = "Index, 1979Q1 = 100 (rot) bzw. 1999Q1 = 100 (blau)",
       caption = "Quelle: OECD. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram +
  theme(axis.text.x = element_blank())

ggsave(g, filename = "pics/20200721_inflation_pre_post_euro_de.jpeg", height = 5, width = 5)

# English
g <- ggplot(temp, aes(x = time, y = index, colour = type_en)) +
  geom_line(size = 1.2) +
  scale_x_continuous(expand = c(.01, .1)) +
  labs(title = "Inflation before and after the introduction of the Euro (Austria)",
       subtitle = "Index, 1979Q1 = 100 (red) bzw. 1999Q1 = 100 (blue)",
       caption = "Source: OECD. Code available at https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram +
  theme(axis.text.x = element_blank())

ggsave(g, filename = "pics/20200721_inflation_pre_post_euro_en.jpeg", height = 5, width = 5)

