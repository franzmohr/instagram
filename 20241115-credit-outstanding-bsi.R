
rm(list = ls())

# Choose language
lang <- "en" # "de" or "en"

# Choose country
ctry <- "U2"

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

if (lang == "de") {
  temp_var_labels <- c("Monetäre Finanz-\ninstitute (MFIs): Gesamt",
                       "Nichtfinanzielle\nUnternehmen", "Private\nHaushalte",
                       "MFIs: Davon\nZentralbanken", "MFIs: Davon\nandere MFIs")
  temp_title <- "Ausstehende Kredite von Banken im Euroraum"
  temp_subtitle <- "Mrd EUR"
  temp_caption <- "Quelle: EZB-BSI. Gegenparteien im Euroraum. Unkonsolidiert."
}
if (lang == "en") {
  temp_var_labels <- c("Monetary financial\ninstitutions (MFIs): Total",
                       "Non-financial\ncorporations", "Householdes",
                       "MFIs: Of which\ncentral banks", "MFIs: Of which\nother MFIs")
  temp_title <- "Outstanding loans by sector (euro area)"
  temp_subtitle <- "Bn EUR"
  temp_caption <- "Source: ECB-BSI. Counterparties within euro area. Unconsolidated data."
}

ctry <- paste0(ctry, collapse = "+")

temp <- ecb::get_data(paste0("BSI.M.", ctry,".N.A.A20.A.1.U2.1000+1100+1200+2240+2250.Z01.E")) %>%
  filter(obstime >= "2005-01") %>%
  select(obstime, count_area, bs_count_sector, obsvalue) %>%
  pivot_wider(names_from = "bs_count_sector", values_from = "obsvalue") %>%
  mutate(date = as.Date(as.yearmon(obstime, "%Y-%m"))) %>%
  select(-obstime, -count_area) %>%
  pivot_longer(cols = -c("date"), names_to = "var", values_to = "value") %>%
  filter(!is.na(value)) %>%
  arrange(date) %>%
  mutate(value = value  / 1000,
         alph = var %in% c("1100", "1200"),
         var = factor(var, levels = c("1000", "2240", "2250", "1100", "1200"),
                      labels = temp_var_labels))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = var, alpha = alph)) +
  geom_line(linewidth = .7) +
  guides(colour = guide_legend(nrow = 2), alpha = "none") +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_x_date(expand = c(.01, 0), date_label = "%Y", date_breaks = "2 years") +
  scale_alpha_manual(values = c(1, .3)) +
  scale_colour_insta +
  theme_instagram +
  coord_cartesian(ylim = c(0, max(temp$value) * 1.06), expand = FALSE)

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-credit-outstanding-bsi.jpeg"), height = 5, width = 5)
