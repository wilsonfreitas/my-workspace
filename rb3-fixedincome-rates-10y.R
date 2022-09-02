
library(rb3)
library(fixedincome)
library(purrr)
library(ggplot2)
library(dplyr)

df_yc <- yc_mget("2004-01-01", "2022-05-20", by = 21)

crvs <- split(df_yc, df_yc$refdate) |> map(function(x) {
  crv <- spotratecurve(
    x$r_252, x$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = x$refdate[1]
  )
  interpolation(crv) <- interp_flatforward()
  crv
})

r_10y <- map_dfr(crvs, function(x) {
  r <- x[[2520]]
  data.frame(refdate = r@refdate, r_10y = as.numeric(r))
}) |>
  filter(!is.na(r_10y))

ggplot(r_10y, aes(x = refdate, y = r_10y)) +
  geom_line(colour = "#e05305", size = 1) +
  labs(
    x = NULL, y = NULL,
    title = "DI1 10Y Rates",
    subtitle = "Built using interest rates future contracts",
    caption = "Source: B3 (data imported using \U1F4E6 rb3)"
  ) +
  scale_y_continuous(labels = scales::percent)

# with futures ----

library(dplyr)

df_fut <- futures_mget("2019-01-01", "2022-05-20", by = 5)

df_di1_fut <- df_fut |>
  filter(commodity == "DI1") |>
  mutate(
    maturity_date = maturity2date(maturity_code),
    fixing = following(maturity_date, "Brazil/ANBIMA"),
    biz_days = bizdays(refdate, maturity_date, "Brazil/ANBIMA"),
    r_252 = rates("discrete", biz_days / 252, 100000 / price)
  ) |>
  filter(biz_days > 0)

fut_crvs <- split(df_di1_fut, df_di1_fut$refdate) |> map(function(x) {
  crv <- spotratecurve(
    x$r_252, x$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = x$refdate[1]
  )
  interpolation(crv) <- interp_flatforward()
  crv
})

r_10y_fut <- map_dfr(fut_crvs, function(x) {
  r <- x[[2520]]
  data.frame(refdate = r@refdate, r_10y = as.numeric(r))
})

ggplot(r_10y_fut, aes(x = refdate, y = r_10y)) +
  geom_line()