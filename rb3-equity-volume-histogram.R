
library(rb3)
library(tidyverse)

refdate <- as.Date("2022-04-01")
ch <- cotahist_get(refdate, "monthly")

eq <- cotahist_equity_get(ch)
fn <- cotahist_etfs_get(ch)
bd <- cotahist_bdrs_get(ch)
fiis <- cotahist_fiis_get(ch)

func <- mean

eq |>
  group_by(symbol) |>
  summarise(volume = func(volume)) |>
  mutate(type = "equities") |>
  select(type, volume) |>
  bind_rows(
    fn |>
      group_by(symbol) |>
      summarise(volume = func(volume)) |>
      mutate(type = "etfs") |>
      select(type, volume)
  ) |>
  bind_rows(
    bd |>
      group_by(symbol) |>
      summarise(volume = func(volume)) |>
      mutate(type = "bdrs") |>
      select(type, volume)
  ) |>
  bind_rows(
    fiis |>
      group_by(symbol) |>
      summarise(volume = func(volume)) |>
      mutate(type = "fiis") |>
      select(type, volume)
  ) |>
  mutate(
    type = factor(
      type,
      levels = c("equities", "etfs", "fiis", "bdrs"), ordered = TRUE
    )
  ) |>
  ggplot(aes(x = volume, fill = type)) +
  geom_histogram(
    aes(y = ..density..),
    breaks = c(
      0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, Inf
    ),
    color = "#e9ecef", alpha = 0.6, position = "identity"
  ) +
  labs(fill = "") +
  scale_x_log10(
    labels = scales::number_format(scale = 1 / 1000, suffix = " K")
  ) +
  facet_wrap(~type, nrow = 4)

fiis |>
  group_by(symbol) |>
  summarise(volume = mean(volume)) |>
  pull(volume) |>
  cut(c(0, 1e5, 1e6, 1e9, Inf)) |>
  table() |>
  proportions()