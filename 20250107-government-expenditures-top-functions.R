rm(list = ls())

# Choose language
lang <- "de" # "de" or "en"

# Choose country
ctry <- "AT"

min_date <- "1995-01-01"

# Load packages
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

source("theme_instagram.R")

raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = c("PC_GDP", "MIO_EUR")),
                    cache = FALSE) %>%
  filter(!is.na(values),
         nchar(cofog99) == 6,
         time >= min_date)

temp <- raw %>%
  rename(var = cofog99) %>%
  mutate(values = case_when(unit == "PC_GDP" ~ values,
                            unit == "MIO_EUR" ~ values / 1000))

cofog <- read.csv("cofog-mapping.csv")
if (lang == "de") {
  cofog <- select(cofog, code, name = name_de)
}
if (lang == "en") {
  cofog <- select(cofog, code, name = name_en)
}

cofog_high_level <- cofog %>%
  filter(nchar(code) == 4) %>%
  rename(high_level_code = code,
         high_level_name = name)

cofog_lower_level <- cofog %>%
  filter(nchar(code) == 6)

cofog <- cofog_lower_level %>%
  mutate(high_level_code = substring(code, 1, 4)) %>%
  left_join(cofog_high_level, by = "high_level_code") %>%
  mutate(name = paste0(high_level_name, ":\n", name)) %>%
  select(code, name)

top_var <- temp %>%
  filter(time == max(time),
         unit == max(unit)) %>%
  arrange(desc(values)) %>%
  slice(1:8) %>%
  left_join(cofog, by = c("var" = "code")) %>%
  pull("name")

if (lang == "de") {
  temp_title <- "Ausgaben des Staates nach Aufgabenbereichen (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
  temp_other <- "Andere"
  unit_name <- c("Mrd EUR", "Prozent des BIP")
}

if (lang == "en") {
  temp_title <- "General government expenditure for social protection (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
  temp_other <- "Other"
  unit_name <- c("Bn EUR", "Percent of GDP")
}

temp <- temp %>%
  left_join(cofog, by = c("var" = "code")) %>%
  mutate(hl = name %in% top_var,
         col = ifelse(name %in% top_var, name, "other"),
         col = factor(col, levels = c(top_var, "other"), labels = c(top_var, temp_other)),
         unit = factor(unit, levels = c("MIO_EUR", "PC_GDP"), labels = unit_name))

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
}

g <- ggplot(temp, aes(x = time, y = values, group = name)) +
  geom_line(aes(colour = col, alpha = hl), linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "3 years", date_labels = "%Y") +
  scale_colour_insta +
  facet_wrap(~unit, scales = "free_y", nrow = 1) +
  guides(alpha = "none", colour = guide_legend(ncol = 1)) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  theme_instagram +
  theme(legend.position = "bottom")

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-government-expenditures-top-functions-", lang, ".jpeg"), height = 7, width = 7)

