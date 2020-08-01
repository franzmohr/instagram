rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(readxl)
library(tidyr)

used_quantiles <- c("P5", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "P95", "P99")

raw <- get_eurostat(id = "ilc_di01") %>%
  group_by(geo) %>%
  filter(currency == "EUR",
         geo %in% c("AT", "EU"),
         indic_il == "TC",
         time == max(time),
         quantile %in% used_quantiles) %>%
  mutate(geo = factor(geo, levels = c("AT", "EU"),
                      labels = c("Österreich", "EU")))

temp <- raw %>%
  mutate(quantile = ifelse(grepl("D", quantile), paste(quantile, "0", sep = ""), quantile),
         quantile = substring(quantile, 2, nchar(quantile)),
         quantile = as.numeric(quantile),
         values = values / 1000) %>%
  arrange(quantile) %>%
  mutate(quantile = as.character(quantile))

temp$quantile <- factor(temp$quantile, levels = unique(temp$quantile))

source("theme_instagram.R") 

g <- ggplot(temp, aes(x = quantile, y = values, fill = geo)) +
  geom_col(position = "dodge") +
  labs(title = "Jahresnettoeinkommen nach Einkommensgruppe",
       subtitle = "Tsd EUR (Obergrenze innerhalb der Gruppe)",
       x = "Perzentil",
       caption = "Quelle: Eurostat (ilc_di01). Die Einkommenshöhe wurde bereinigt, sodass Singlehaushalte besser\nmit Mehrpersonenhaushalten verglichen werden können.\nCode unter https://github.com/franzmohr/instagram.") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(temp$values * 1.06))) +
  scale_fill_insta +
  theme_instagram +
  theme(axis.text.x = element_text(angle = 0),
        axis.title.x = element_text(size = 10))

ggsave(g, filename = "pics/20200801_inc_dist_de.jpeg", height = 5, width = 5)

g <- ggplot(temp, aes(x = quantile, y = values, fill = geo)) +
  geom_col(position = "dodge") +
  labs(title = "Annual net income by income group",
       subtitle = "Thousand EUR (top cut-off point per group)",
       x = "Percentile",
       caption = "Source: Eurostat (ilc_di01). Incomes were adjusted so that single households can be compared\nwith households with multiple persons.\nCode available at https://github.com/franzmohr/instagram.") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(temp$values * 1.06))) +
  scale_fill_insta +
  theme_instagram +
  theme(axis.text.x = element_text(angle = 0),
        axis.title.x = element_text(size = 10))

ggsave(g, filename = "pics/20200801_inc_dist_en.jpeg", height = 5, width = 5)
