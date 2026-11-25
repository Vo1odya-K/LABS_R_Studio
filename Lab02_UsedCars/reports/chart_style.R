
library(ggplot2)

chart_palette <- c(
  primary   = "#0B6BB0",
  secondary = "#E07B00",
  accent    = "#C0392B",
  neutral   = "#9E9E9E",
  band      = "#DBE8F5",
  shade     = "#F3F4F6",
  ink       = "#262626",
  ink2      = "#5F5F5F",
  grid      = "#EBEBEB",
  axis      = "#8C8C8C"
)

theme_chart <- function(
    base_size = 22,
    base_family = "sans",
    heading_family = base_family,
    palette = chart_palette
) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      plot.background = element_rect(
        fill = "white", colour = NA
      ),
      panel.background = element_rect(
        fill = "white", colour = NA
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        colour = palette[["grid"]]
      ),
      axis.text = element_text(
        colour = palette[["ink2"]]
      ),
      axis.title = element_text(
        colour = palette[["ink2"]]
      ),
      plot.title = element_text(
        family = heading_family,
        face = "plain",
        colour = palette[["ink"]],
        size = rel(1.2),
        margin = margin(b = 14)
      ),
      plot.title.position = "plot",
      plot.caption = element_text(
        colour = palette[["ink2"]],
        hjust = 0,
        size = rel(0.8)
      ),
      legend.position = "top",
      legend.title = element_blank(),
      legend.key = element_rect(
        fill = "white", colour = NA
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        colour = palette[["ink"]],
        face = "bold"
      ),
      plot.margin = margin(16, 20, 16, 16)
    )
}

scale_colour_chart <- function(
    values = unname(chart_palette[
      c("primary", "secondary", "neutral", "accent")
    ]), ...
) {
  scale_colour_manual(values = values, ...)
}

scale_fill_chart <- function(
    values = unname(chart_palette[
      c("primary", "secondary", "neutral", "accent")
    ]), ...
) {
  scale_fill_manual(values = values, ...)
}
