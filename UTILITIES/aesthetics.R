#   Goal: Creating themes and aesthetics for plots.
library(ggplot2)
# Define colour palette
palette <- c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")
palette2 <- c("#003f5c", "#28627c", "#4b879c", "#6faebd", "#96d6de", "#c0ffff")
extended_palette <- c("#10433A", "#518378", "#2AAA74", "#8cd2ad", "#FCD362", "#f8db99", "#F2863D", "#fdb98d", "#CF4F46", "#f0a295", "#565857", "#7e9188" )

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino",vjust = -2,hjust = 0.5,face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 3,hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Margins and spacing
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 16,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 11,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "right",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",hjust = 3,face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)