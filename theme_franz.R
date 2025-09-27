
library(ggplot2)


#font <- "Roboto Condensed"
font <- NULL
#backgroud_colour <- "#FFEBDE"##"white"# 
backgroud_colour <- "white"
weaker_lines <- rgb(139, 139, 139, maxColorValue = 255)
axis_colour <- "black"

colour <- c("#C10016", # Red
            "#0F4C81", # Blue,
            "#FC4C02", # Orange
            "#630f81", # Violet
            "#0f8144",
            "#81390f",
            "#004C45", # Green
            "#F8DE7E", # Yellow
            "#191970") # Dark Blue

scale_colour_insta <- scale_colour_manual(values = colour)
scale_fill_insta <- scale_fill_manual(values = colour)

theme_instagram <- theme_light() +
  # Background
  theme(plot.background = element_rect(fill = backgroud_colour),
        plot.title = element_text(colour = axis_colour, family = font),
        plot.subtitle = element_text(colour = axis_colour, family = font),
        plot.caption = element_text(hjust = 0, colour = axis_colour, family = font),
        #Panel
        panel.background = element_rect(fill = backgroud_colour),
        panel.grid.major = element_line(colour = weaker_lines),
        panel.grid.minor = element_line(colour = weaker_lines),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        # Strip
        strip.background = element_rect(fill = NA),
        strip.text = element_text(colour = "black", family = font),
        # Legend
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = backgroud_colour),
        legend.key = element_rect(fill = backgroud_colour),
        legend.text = element_text(size = 9, colour = axis_colour, family = font),
        legend.key.size = unit(10, "points"),
        # Axis
        axis.line = element_line(colour = axis_colour),
        axis.text = element_text(colour = axis_colour, family = font),
        axis.title = element_text(colour = axis_colour, family = font))

rm(list = c("axis_colour", "weaker_lines", "backgroud_colour"))
