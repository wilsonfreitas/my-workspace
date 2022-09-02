
library(rb3)
library(tidyverse)

refdate <- as.Date("2022-04-01")
ch <- cotahist_get(refdate, "monthly")

eq <- cotahist_equity_get(ch)
fn <- cotahist_etfs_get(ch)
bd <- cotahist_bdrs_get(ch)
fiis <- cotahist_fiis_get(ch)

fiis |>
  group_by(symbol) |>
  summarise(volume = mean(volume)) |>
  pull(volume) |>
  cut(c(0, 1e5, 1e6, 1e9, Inf)) |>
  table() |>
  proportions()