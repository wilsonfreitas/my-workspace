
library(rb3)
library(tidyverse)

source("companies-info.R")

library(RSQLite)

conn <- DBI::dbConnect(RSQLite::SQLite(), "rb3.db")

DBI::dbExecute(conn, "drop table cotahist_equity")
DBI::dbExecute(conn, "drop table cotahist_symbols")

for (year in 1990:2022) {
  cat(year, "\n")

  ch <- cotahist_get(str_glue("{year}-01-01"), "yearly")

  # eqs <- cotahist_equity_get(ch)
  # eqs <- eqs |>
  #   mutate(refdate = format(refdate))
  # DBI::dbWriteTable(conn, "cotahist_equity", eqs, append = TRUE)

  # symbols <- cotahist_equity_symbols_get(ch)
  # symbols[["year"]] <- year
  # DBI::dbWriteTable(conn, "cotahist_symbols", symbols, append = TRUE)
}

for (year in 1999:2022) {
  cat(year, "\n")

  ch <- cotahist_get(str_glue("{year}-01-01"), "yearly")

  eqs <- cotahist_etfs_get(ch)
  eqs <- eqs |>
    mutate(refdate = format(refdate))
  DBI::dbWriteTable(conn, "cotahist_etf", eqs, append = TRUE)
}

symbols_table <- DBI::dbGetQuery(conn, "
select
  symbol,
  asset_name,
  spec_type,
  isin
from cotahist_symbols cs
group by
  cs.symbol,
  cs.asset_name,
  cs.spec_type,
  cs.isin
order by symbol
;")

cash_divs_df <- company_cash_dividends_get(symbols_table$symbol, symbols_table)
divs_df <- company_stock_dividends_get(symbols_table$symbol)
subs_df <- company_subscriptions_get(symbols_table$symbol)
company_df <- company_info_get(symbols_table$symbol)


DBI::dbWriteTable(conn, "company_cash_dividends",
  cash_divs_df |> mutate(
    approved = format(approved),
    last_date_prior_ex = format(last_date_prior_ex),
  ),
  overwrite = TRUE
)
DBI::dbWriteTable(conn, "company_stock_dividends",
  divs_df |> mutate(
    approved = format(approved),
    last_date_prior_ex = format(last_date_prior_ex),
  ),
  overwrite = TRUE
)
DBI::dbWriteTable(conn, "company_subscriptions",
  subs_df |> mutate(
    approved = format(approved),
    last_date_prior_ex = format(last_date_prior_ex),
    subscription_date = format(subscription_date)
  ),
  overwrite = TRUE
)
DBI::dbWriteTable(conn, "company_infos",
  select(company_df, -codes) |>
    mutate(quoted_since = format(quoted_since)),
  overwrite = TRUE
)

f <- download_marketdata("GetStockIndex")
df <- read_marketdata(f, "GetStockIndex")

res <- lapply(split(df$Results, df$Results$code), function(x) {
  indexes <- unlist(str_split(x$indexes, ","))
  tibble(
    code = x$code,
    spec_type = x$spotlight,
    indexes = indexes
  )
})

res <- bind_rows(res)

DBI::dbWriteTable(conn, "indexes_composition",
  res,
  overwrite = TRUE
)


cash_divs_df |>
  count(symbol, sort = TRUE)

df <- DBI::dbGetQuery(conn, "
select * from cotahist_equity where symbol = 'VALE5'
order by refdate
")

df <- DBI::dbGetQuery(conn, "
select * from company_cash_dividends where symbol = 'B3SA3'
")

df <- DBI::dbGetQuery(conn, "select * from rb3_adjusted_equity where symbol = 'ITUB3'")

rets <- df |>
  filter(!is.na(oscilation)) |>
  arrange(desc(refdate))

lx <- length(rets$close)
close_price <- rets$close[1]
ox <- numeric(length = lx + 1)
ox[1] <- close_price
for (ix in seq_len(lx)) {
  ox[ix + 1] <- ox[ix] / (1 + rets$oscilation[ix])
}

tibble(
  refdate = rets$refdate,
  adj_close = ox[-lx]
) |>
  left_join(df |> select(refdate, close), by = "refdate") |>
  pivot_longer(cols = c("close", "adj_close")) |>
  mutate(refdate = as.Date(refdate)) |>
  ggplot(aes(x = refdate, y = value, colour = name, group = name)) +
  geom_line(size = 1)

tibble(
  refdate = rets$refdate,
  adj_close = ox[-lx]
) |>
  mutate(refdate = as.Date(refdate)) |>
  filter(refdate >= "2011-10-01", refdate <= "2011-11-30") |>
  ggplot(aes(x = refdate, y = adj_close)) +
  geom_line() -> p

ggplotly(p)

library(plotly)