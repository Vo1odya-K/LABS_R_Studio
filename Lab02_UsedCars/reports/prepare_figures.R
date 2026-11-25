
library(tidyverse)
library(scales)

source("reports/chart_style.R")

# ==================================================
# 1. Підготовка даних та налаштування
# ==================================================

cars_clean <- read_csv(
  "data/processed/clean_data.csv",
  show_col_types = FALSE
)

dir.create(
  "reports/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

theme_set(theme_chart(base_size = 24))

primary <- chart_palette[["primary"]]
secondary <- chart_palette[["secondary"]]
neutral <- chart_palette[["neutral"]]
accent <- chart_palette[["accent"]]

number_labels <- label_number(
  big.mark = " ",
  decimal.mark = ","
)

save_figure <- function(plot, filename,
                        width = 12, height = 5) {

  path <- file.path("reports/figures", filename)

  ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = 200,
    bg = "white"
  )

  message("Збережено: ", path)
}

# ==================================================
# 2. Розподіл цін — логарифмічна шкала
# ==================================================

p_price_log <- ggplot(
  cars_clean,
  aes(x = price)
) +
  geom_histogram(
    bins = 40,
    fill = primary,
    colour = "white",
    linewidth = 0.3
  ) +
  scale_x_log10(
    labels = number_labels
  ) +
  geom_vline(
    xintercept = median(cars_clean$price),
    colour = secondary,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  labs(
    x = "Ціна, USD (логарифмічна шкала)",
    y = "Кількість автомобілів"
  )

save_figure(
  p_price_log,
  "price_log_distribution.png"
)

# ==================================================
# 3. Початковий розподіл цін
# ==================================================

p_price <- ggplot(
  cars_clean,
  aes(x = price)
) +
  geom_histogram(
    bins = 60,
    fill = primary,
    colour = "white",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = median(cars_clean$price),
    colour = secondary,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    labels = number_labels
  ) +
  scale_y_continuous(
    labels = number_labels
  ) +
  labs(
    x = "Ціна, USD",
    y = "Кількість автомобілів"
  )

save_figure(
  p_price,
  "price_distribution.png"
)

# ==================================================
# 4. Залежність ціни від року випуску
# ==================================================

p_year <- ggplot(
  cars_clean,
  aes(x = model_year, y = price)
) +
  geom_point(
    colour = primary,
    alpha = 0.25,
    size = 1.7
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    colour = secondary,
    fill = chart_palette[["band"]],
    linewidth = 1.5,
    se = TRUE
  ) +
  scale_y_log10(
    labels = number_labels
  ) +
  labs(
    x = "Рік випуску",
    y = "Ціна, USD (лог. шкала)"
  )

save_figure(
  p_year,
  "price_year.png"
)

# ==================================================
# 5. Залежність ціни від пробігу
# ==================================================

p_milage <- ggplot(
  cars_clean,
  aes(x = milage, y = price)
) +
  geom_point(
    colour = primary,
    alpha = 0.25,
    size = 1.7
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    colour = secondary,
    fill = chart_palette[["band"]],
    linewidth = 1.5,
    se = TRUE
  ) +
  scale_x_continuous(
    labels = number_labels
  ) +
  scale_y_log10(
    labels = number_labels
  ) +
  labs(
    x = "Пробіг, миль",
    y = "Ціна, USD (лог. шкала)"
  )

save_figure(
  p_milage,
  "price_milage.png"
)

# ==================================================
# 6. Медіанні ціни за марками
# ==================================================

brand_stats <- cars_clean |>
  group_by(brand) |>
  summarise(
    n = n(),
    median_price = median(price),
    .groups = "drop"
  ) |>
  slice_max(
    order_by = n,
    n = 10,
    with_ties = FALSE
  )

p_brand <- ggplot(
  brand_stats,
  aes(
    x = median_price,
    y = reorder(brand, median_price)
  )
) +
  geom_col(
    aes(fill = brand == "Porsche"),
    width = 0.65
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = secondary,
      "FALSE" = primary
    ),
    guide = "none"
  ) +
  geom_text(
    aes(label = number_labels(median_price)),
    hjust = -0.15,
    size = 8
  ) +
  scale_x_continuous(
    labels = number_labels,
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    x = "Медіанна ціна, USD",
    y = NULL
  ) + 
  theme(
  axis.text.y = element_text(size = 26),
  axis.text.x = element_text(size = 24)
  )

save_figure(
  p_brand,
  "price_brand.png",
  height = 6
)

# ==================================================
# 7. Медіанні ціни за типами пального
# ==================================================

fuel_stats <- cars_clean |>
  mutate(
    fuel_label = recode(
      fuel_type,
      "Gasoline" = "Бензин",
      "Hybrid" = "Гібрид",
      "Diesel" = "Дизель",
      "Plug-In Hybrid" = "Plug-in гібрид",
      "E85 Flex Fuel" = "E85 Flex Fuel",
      "Unknown" = "Невідомо"
    )
  ) |>
  group_by(fuel_label) |>
  summarise(
    n = n(),
    median_price = median(price),
    .groups = "drop"
  )

p_fuel <- ggplot(
  fuel_stats,
  aes(
    x = median_price,
    y = reorder(fuel_label, median_price)
  )
) +
  geom_col(
    aes(fill = fuel_label == "Невідомо"),
    width = 0.65
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = neutral,
      "FALSE" = primary
    ),
    guide = "none"
  ) +
  geom_text(
    aes(label = number_labels(median_price)),
    hjust = -0.15,
    size = 6
  ) +
  scale_x_continuous(
    labels = number_labels,
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    x = "Медіанна ціна, USD",
    y = NULL
  )

save_figure(
  p_fuel,
  "price_fuel.png"
)

# ==================================================
# 8. Кореляції Пірсона та Спірмена
# ==================================================

correlation_data <- tibble(
  pair = rep(
    c(
      "Ціна — пробіг",
      "Ціна — рік випуску",
      "Пробіг — рік випуску"
    ),
    each = 2
  ),
  method = rep(
    c("Пірсон", "Спірмен"),
    times = 3
  ),
  correlation = c(
    cor(cars_clean$price,
        cars_clean$milage,
        method = "pearson"),

    cor(cars_clean$price,
        cars_clean$milage,
        method = "spearman"),

    cor(cars_clean$price,
        cars_clean$model_year,
        method = "pearson"),

    cor(cars_clean$price,
        cars_clean$model_year,
        method = "spearman"),

    cor(cars_clean$milage,
        cars_clean$model_year,
        method = "pearson"),

    cor(cars_clean$milage,
        cars_clean$model_year,
        method = "spearman")
  )
)

p_correlation <- ggplot(
  correlation_data,
  aes(
    x = correlation,
    y = pair,
    fill = method
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.65
  ) +
  geom_vline(
    xintercept = 0,
    colour = chart_palette[["axis"]]
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f", correlation),
      hjust = ifelse(correlation < 0, 1.15, -0.15)
    ),
    position = position_dodge(width = 0.8),
    size = 6
  ) +
  scale_fill_chart(
    values = c(
      "Пірсон" = primary,
      "Спірмен" = secondary
    )
  ) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5)
  ) +
  labs(
    x = "Коефіцієнт кореляції",
    y = NULL
  )

save_figure(
  p_correlation,
  "correlation_comparison.png"
)

# ==================================================
# 9. Медіанні ціни за історією ДТП
# ==================================================

accident_stats <- cars_clean |>
  mutate(
    accident_label = case_when(
      accident == "None reported" ~ "Без повідомлень про ДТП",
      accident == "At least 1 accident or damage reported" ~
        "Є повідомлення про ДТП",
      TRUE ~ "Невідомо"
    )
  ) |>
  group_by(accident_label) |>
  summarise(
    n = n(),
    median_price = median(price),
    .groups = "drop"
  )

p_accident <- ggplot(
  accident_stats,
  aes(
    x = median_price,
    y = reorder(accident_label, median_price),
    fill = accident_label
  )
) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "Без повідомлень про ДТП" = primary,
      "Є повідомлення про ДТП" = secondary,
      "Невідомо" = neutral
    ),
    guide = "none"
  ) +
  geom_text(
    aes(label = number_labels(median_price)),
    hjust = -0.15,
    size = 6
  ) +
  scale_x_continuous(
    labels = number_labels,
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    x = "Медіанна ціна, USD",
    y = NULL
  )

save_figure(
  p_accident,
  "price_accident.png"
)

# ==================================================
# 10. Спільний аналіз року, пробігу та ціни
# ==================================================

cars_multi <- cars_clean |>
  mutate(
    year_group = cut(
      model_year,
      breaks = c(-Inf, 2009, 2014, 2019, Inf),
      labels = c(
        "До 2010",
        "2010–2014",
        "2015–2019",
        "2020–2024"
      )
    )
  )

p_multi <- ggplot(
  cars_multi,
  aes(
    x = milage,
    y = price,
    colour = year_group
  )
) +
  geom_point(
    alpha = 0.4,
    size = 1.8
  ) +
  scale_colour_chart(
    values = c(
      "До 2010" = neutral,
      "2010–2014" = secondary,
      "2015–2019" = primary,
      "2020–2024" = accent
    )
  ) +
  scale_x_continuous(
    labels = number_labels
  ) +
  scale_y_log10(
    labels = number_labels
  ) +
  labs(
    x = "Пробіг, миль",
    y = "Ціна, USD (лог. шкала)",
    colour = NULL
  )

save_figure(
  p_multi,
  "multi_price_milage.png"
)


# ==================================================
# 11. Теплова карта багатофакторного аналізу
# ==================================================

multi_stats <- read_csv(
  "outputs/tables/multivariate_statistics.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

heatmap_data <- multi_stats |>
  filter(
    accident %in% c(
      "None reported",
      "At least 1 accident or damage reported"
    )
  ) |>
  mutate(
    accident_type = if_else(
      accident == "None reported",
      "no_accident",
      "with_accident"
    )
  ) |>
  select(
    year_group,
    milage_group,
    accident_type,
    count,
    median_price
  ) |>
  pivot_wider(
    names_from = accident_type,
    values_from = c(count, median_price)
  ) |>
  mutate(
    difference = if_else(
      count_no_accident >= 10 &
        count_with_accident >= 10 &
        median_price_no_accident > 0,
      (
        median_price_no_accident -
          median_price_with_accident
      ) / median_price_no_accident * 100,
      NA_real_
    ),
    year_group = factor(
      year_group,
      levels = c(
        "До 2010",
        "2010–2014",
        "2015–2019",
        "2020–2024"
      )
    ),
    milage_group = factor(
      milage_group,
      levels = c(
        "До 50 тис.",
        "50–100 тис.",
        "100–150 тис.",
        "Понад 150 тис."
      )
    ),
    label = if_else(
      is.na(difference),
      "Н/Д",
      sprintf("%.1f%%", difference)
    )
  )

heatmap_data <- heatmap_data |>
  complete(year_group, milage_group) |>
  mutate(
    label = if_else(
      is.na(difference),
      "Н/Д",
      sprintf("%.1f%%", difference)
    )
  )

p_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = milage_group,
    y = year_group,
    fill = difference
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 1.5
  ) +
  geom_text(
    aes(
      label = label,
      colour = !is.na(difference) &
        abs(difference) >= 20
    ),
    size = 7
  ) +
  scale_colour_manual(
    values = c(
      "TRUE" = "white",
      "FALSE" = chart_palette[["ink"]]
    ),
    guide = "none"
  ) +
  scale_fill_gradient2(
    low = accent,
    mid = "white",
    high = primary,
    midpoint = 0,
    limits = c(-40, 40),
    breaks = seq(-40, 40, 20),
    na.value = "#E5E5E5",
    labels = label_number(suffix = "%"),
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(14, "cm"),
      barheight = grid::unit(0.5, "cm")
    )
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    x = "Група пробігу, миль",
    y = "Рік випуску",
    fill = "Різниця медіанних цін"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 22)
  )

save_figure(
  p_heatmap,
  "multi_heatmap.png",
  width = 12,
  height = 5.5
)

# Перевірка розрахованих значень
print(
  heatmap_data |>
    select(
      year_group,
      milage_group,
      difference
    ),
  n = Inf
)


# ==================================================
# 12. Перевірка створених графіків
# ==================================================

cat("\n====================================\n")
cat("Генерацію графіків завершено\n")
cat("====================================\n")

files <- list.files(
  "reports/figures",
  pattern = "\\.png$"
)

print(files)

cat("\nКількість графіків:", length(files), "\n")
