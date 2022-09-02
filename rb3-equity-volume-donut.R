
library(rb3)
library(tidyverse)
library(ggrepel)

top_weight <- function(.data, n = 10) {
  top_10 <- .data |>
    arrange(desc(weight)) |>
    slice_head(n = n) |>
    select(symbol, weight)
  total_weight <- sum(top_10$weight)
  others <- tibble(
    symbol = "Others",
    weight = 1 - total_weight
  )
  bind_rows(top_10, others) |>
    mutate(cum_weight = cumsum(weight))
}

donut <- function(.data, title) {
  .data |>
    mutate(
      ymax = cum_weight,
      ymin = c(0, head(cum_weight, n = -1)),
      label_pos = (ymax + ymin) / 2,
      label = paste0(symbol, "\n", scales::percent(weight))
    ) |>
    ggplot(aes(
      ymax = ymax, ymin = ymin,
      xmax = 4, xmin = 3,
      fill = symbol
    )) +
    geom_rect(colour = "white") +
    geom_label(
      x = 4.5, aes(y = label_pos, label = label), size = 3
    ) +
    annotate(
      "text",
      x = 0, y = 0, label = title, size = 12, colour = "grey",
      family = "Verdana", fontface = 2
    ) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    scale_color_brewer(palette = "Set3") +
    xlim(c(0, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    labs(
      caption = "Source: B3 (data imported using \U1F4E6 rb3) - wilsonfreitas"
    )
}

refdate <- as.Date("2022-04-01") # Sys.Date() - 2
ch <- cotahist_get(refdate, "monthly")

eq <- cotahist_equity_get(ch)
eq |>
  group_by(symbol) |>
  summarise(volume = sum(volume)) |>
  mutate(weight = volume / sum(volume)) |>
  top_weight() |>
  donut(str_glue("BOVESPA\nTop Volumes\n{format(refdate, '%B %Y')}"))

fn <- cotahist_etfs_get(ch)
fn |>
  group_by(symbol) |>
  summarise(volume = sum(volume)) |>
  mutate(weight = volume / sum(volume)) |>
  top_weight(n = 6) |>
  donut(str_glue("ETFs\nTop Volumes\n{format(refdate, '%B %Y')}"))

bd <- cotahist_bdrs_get(ch)
bd |>
  group_by(symbol) |>
  summarise(volume = sum(volume)) |>
  mutate(weight = volume / sum(volume)) |>
  top_weight() |>
  donut(str_glue("BDRs\nTop Volumes\n{format(refdate, '%B %Y')}"))

fiis <- cotahist_fiis_get(ch)
fiis |>
  group_by(symbol) |>
  summarise(volume = sum(volume)) |>
  mutate(weight = volume / sum(volume)) |>
  top_weight() |>
  donut(str_glue("FIIs\nTop Volumes\n{format(refdate, '%B %Y')}"))


eq |>
  group_by(symbol) |>
  summarise(volume = sum(volume)) |>
  ggplot(aes(x = volume)) +
  geom_histogram(
    bins = 30, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9
  ) +
  scale_x_log10(labels = scales::number_format(scale = 1))

fn |>
  ggplot(aes(x = volume)) +
  geom_histogram(
    bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9
  ) +
  scale_x_log10(labels = scales::number_format(scale = 1))

bd |>
  ggplot(aes(x = volume)) +
  geom_histogram(
    bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9
  ) +
  scale_x_log10(labels = scales::number_format(scale = 1))

fiis |>
  ggplot(aes(x = volume)) +
  geom_histogram(
    bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9
  ) +
  scale_x_log10(labels = scales::number_format(scale = 1))