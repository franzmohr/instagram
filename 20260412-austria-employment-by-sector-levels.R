
rm(list = ls())


# Choose language
lang <- "de"

min_date <- "2004-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(readxl)
library(ggthemes)

raw <- read_excel("D:/Data/20260412-statistik-austria-employment.xlsx", 
                  sheet = "1. Erwerbstätig", na = "-", 
                  skip = 8)[, -1]

names(raw)[1] <- "date"

temp <- raw %>%
  pivot_longer(cols = -c("date")) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(as.yearqtr(date, "%q. Quartal %Y")),
         nace = substring(name, nchar(name) - 1, nchar(name) - 1),
         name = substring(name, 1, nchar(name) - 4)) %>%
  arrange(date) %>%
  na.omit() %>%
  filter(date >= min_date)

top_name <- temp %>%
  filter(date == max(date)) %>%
  group_by(name) %>%
  summarise(value = sum(abs(value)),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  pull("name")

temp <- temp %>%
  filter(name != "Private Haushalte") %>%
  mutate(name = factor(name, levels = top_name))

if (lang == "de") {
  fig_title <- "Beschäftigung in Österreich nach Sektor"
  fig_subtitle <- "Tausend Personen"
  fig_caption <- "Quelle: STATcube - Statistische Datenbank von Statistik Austria. Letzter Wert: "
}

max_date <- format(as.yearqtr(max(temp$date)), "%YQ%q")
fig_caption <- paste0(fig_caption, max_date, ".")

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = value, fill = "a")) +
  geom_col(show.legend = FALSE) +
  scale_x_date(expand = c(0, 0), date_labels = "%Y") +
  scale_fill_insta +
  facet_wrap(~ name, ncol = 4) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-austria-employment-by-sector-levels.jpeg"), height = 7, width = 7)
