#   Goal: Creating themes and aesthetics for plots.

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

# Define colour palette
palette <- c("#1F6F70", "#D98515", "#1E3364", "#75A21B", "#C93D44", "#38B0FF", "#8E22AC")

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),
    axis.line = element_line(size = 0.3, color = "black"),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino", vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 0.5, hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 14,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 12,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)