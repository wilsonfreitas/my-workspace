
library(rb3)
library(bizdays)
library(dplyr)
library(purrr)

f <- download_marketdata("AjustesDiarios", refdate = as.Date("1991-06-06"))
read_marketdata(f, "AjustesDiarios")

date_vec <- seq(as.Date("1990-01-01"), as.Date("2000-01-01"), by = "day")

x <- purrr::map(
  cli::cli_progress_along(
    date_vec,
    format = paste0(
      "{cli::pb_spin} Fetching data points ",
      "{cli::pb_current}/{cli::pb_total}",
      " | {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}"
    )
  ),
  function(idx, date_vec) {
    download_marketdata("AjustesDiarios", refdate = date_vec[idx])
  },
  date_vec
)

dfs <- purrr::map(
  cli::cli_progress_along(
    date_vec,
    format = paste0(
      "{cli::pb_spin} Lendo ",
      "{cli::pb_current}/{cli::pb_total}",
      " | {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}"
    )
  ),
  function(idx, x) {
    read_marketdata(x[[idx]], "AjustesDiarios")
  },
  x
)

library(arrow)
library(stringr)
library(fixedincome)
library(ggplot2)

# dir.create("parquet-files/b3-futures/", recursive = TRUE)

dir <- "parquet-files/b3-futures/"

rets <- map(date_vec, function(refdate) {
  f <- download_marketdata("AjustesDiarios", refdate = refdate)
  # df <- read_marketdata(f, "AjustesDiarios")
  df <- futures_get(refdate)
  if (is.null(df)) {
    unlink(f)
    return(NULL)
  }
  fname <- file.path(dir, str_glue("{format(refdate)}.parquet"))
  write_parquet(df, fname)
})

ds <- open_dataset("parquet-files/b3-futures", format = "parquet")

ds |>
  filter(refdate == as.Date("2012-01-02"), commodity == "DI1", price != 100000) |>
  collect() |>
  mutate(
    maturity_date = maturity2date(maturity_code),
    fixing = following(maturity_date, "Brazil/ANBIMA"),
    business_days = bizdays(refdate, fixing, "Brazil/ANBIMA"),
    adjusted_tax = implied_rate("discrete", business_days / 252, 100000 / price)
  ) |>
  ggplot(aes(x = business_days, y = adjusted_tax)) +
  geom_point()

ds |>
  filter(refdate == as.Date("2007-01-02"), commodity == "DI1", price != 100000) |>
  collect() |>
  mutate(
    maturity_date = maturity2date(maturity_code),
    fixing = following(maturity_date, "Brazil/ANBIMA"),
    business_days = bizdays(refdate, fixing, "Brazil/ANBIMA"),
    adjusted_tax = implied_rate("discrete", business_days / 252, 100000 / price)
  ) |>
  group_by(refdate) |>
  summarise(curve = list(
    spotratecurve(adjusted_tax, business_days, "discrete", "business/252", "Brasil/ANBIMA")
  ))
