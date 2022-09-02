
library(rb3)
library(tidyverse)

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

index_name <- "IBOV"
g <- index_weights_get(index_name) |>
  top_weight() |>
  mutate(
    ymax = cum_weight,
    ymin = c(0, head(cum_weight, n = -1)),
    label_pos = (ymax + ymin) / 2,
    label = paste0(symbol, "\n", scales::percent(weight)),
    symbol = factor(symbol, ordered = TRUE)
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
    x = 0, y = 0, label = index_name, size = 16, colour = "grey",
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

# ggsave(str_glue("{index_name}-{format(Sys.Date(), '%Y%m%d')}.png"), g,
#   units = "px", width = 750, height = 750, dpi = 75,
#   bg = "white"
# )