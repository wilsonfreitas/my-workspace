library(rb3)
library(bizdays)
library(dplyr)
library(purrr)
library(arrow)
library(stringr)
library(fixedincome)
library(ggplot2)
library(rbcb)

# TODO
# [ ] download data incrementally

ds <- open_dataset("parquet-files/b3-futures", format = "parquet")

start_date <- as.Date("2007-01-01")

fut_data <- ds |>
  filter(refdate >= start_date, commodity == "DI1", price != 100000) |>
  select(refdate, price, maturity_code) |>
  collect() |>
  mutate(
    maturity_date = maturity2date(maturity_code),
    fixing = following(maturity_date, "Brazil/ANBIMA"),
    business_days = bizdays(refdate, fixing, "Brazil/ANBIMA"),
    adjusted_tax = implied_rate("discrete", business_days / 252, 100000 / price)
  )

cdi_ <- get_series(c(CDI = 4389), start_date = start_date)

cdi <- cdi_ |>
  rename(
    refdate = date,
    rate = CDI
  ) |>
  mutate(
    maturity_code = "CDI",
    maturity_date = bizdays::offset(refdate, 1, "Brazil/ANBIMA"),
    business_days = bizdays(refdate, maturity_date, "Brazil/ANBIMA"),
    rate = rate / 100
  )

fut_curves <- fut_data |>
  select(refdate, maturity_date, maturity_code, adjusted_tax, business_days) |>
  rename(rate = adjusted_tax)

cdi_curves <- bind_rows(fut_curves, cdi)

write_dataset(
  cdi_curves |> arrange(refdate, business_days),
  "parquet-files/cdi-curves",
  format = "parquet"
)

curves_ds <- open_dataset("parquet-files/cdi-curves")

curves_df <- curves_ds |>
  collect()

curves_list <-  curves_df |>
  split(curves_df$refdate) |>
  map(function(x) {
    if (nrow(x) == 1) {
      return(NULL)
    }
    cat(x$refdate[1] |> format(), "\n")
    x <- x |> filter(!duplicated(business_days))
    crv <- spotratecurve(x$rate, x$business_days,
      "discrete", "business/252", "Brazil/ANBIMA",
      refdate = x$refdate[1]
    )
    interpolation(crv) <- interp_flatforward()
    crv
  }) |>
  keep(\(x) !is.null(x))

h_1Y <- curves_list |>
  map_dfr(function(x) {
    tibble(
      refdate = x@refdate,
      rate_1Y = as.numeric(x[[252]])
    )
  })

h_1Y |>
  ggplot(aes(x = refdate, y = rate_1Y)) +
  geom_line()

diff(h_1Y$rate_1Y) |> plot(type = "l")
as.data.frame(curves_list[[1]])
maturities(curves_list[[1]])