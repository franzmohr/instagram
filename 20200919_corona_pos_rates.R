rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)

temp <- tempfile()
download.file("https://covid.ourworldindata.org/data/owid-covid-data.xlsx",
              destfile = temp, mode = "wb")
data <- read_xlsx(temp, col_types = c("text", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
unlink(temp)

countries <- c("AUT", "DEU", "FRA", "ITA", "SWE", "BEL", "USA")

temp <- data %>%
  filter(iso_code %in% countries) %>%
  mutate(date = as.Date(date),
         value = positive_rate,
         names_de = factor(iso_code, levels = countries,
                           labels = c("Österreich", "Deutschland", "Frankreich", "Italien", "Schweden", "Belgien", "USA")),
         names_en = factor(iso_code, levels = countries,
                           labels = c("Austria", "Germany", "France", "Italy", "Sweden", "Belgium", "USA"))) %>%
  filter(iso_code != "ITA") %>%
  filter(date >= "2020-02-14") %>%
  #select(date, names_de, names_en, value1, value2) %>%
  filter(!is.na(value))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = names_de)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "2 weeks", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  labs(title = "Covid-19-Neuinfektionen",
       subtitle = "Anteil bestätigter Fälle an den gesamten Tests pro Tag",
       caption = "Quelle: https://ourworldindata.org/coronavirus.\nCode unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200919_corona_pos_rates_de.jpeg", height = 5, width = 5)

g <- ggplot(temp, aes(x = date, y = value, colour = names_en)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "2 weeks", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ".")) +
  labs(title = "New cases of Covid-19-infections",
       subtitle = "The number of confirmed cases divided by the number of tests per day",
       caption = "Source: https://ourworldindata.org/coronavirus.\nCode available at https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200919_corona_pos_rates_en.jpeg", height = 5, width = 5)
