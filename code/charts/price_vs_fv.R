# Best bid and best ask (from quotes.csv) against the fundamental value the
# informed traders were estimating. Sessions 2, 3 and 4. Teacher-style chart:
# quoted touch, not trade prints.
#
# Each session needs its own view:
#   2  the book sat several times above value for most of the hour, so the y axis
#      is logarithmic and nothing is hidden.
#   3  quotes more than 2x from the median value are left out (the chart as approved).
#   4  quotes above twice the highest value are left out. A zoom on the median value
#      hid the stretch where STOCK1 traded near 130 against a value near 40, and
#      trade-price percentiles are swamped by fat-finger quotes up to 10,000.
# Left-out or missing quotes stay as NA rows so each line breaks there. Dropping
# them made geom_step carry the last visible price across the gap, which drew a
# bid above the ask where the real ask was simply off the chart.
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(.f)), "common.R"))

for (n in 2:4) {
  sess <- sprintf("session-%d", n)
  quotes <- read_data(sess, "quotes.csv")
  trades <- read_data(sess, "trades.csv")
  fv <- read_data(sess, "fv.csv")
  symbols <- sort(unique(quotes$symbol))

  # Last quote per second per symbol.
  q <- quotes |>
    mutate(sec = floor(t_sec)) |>
    group_by(symbol, sec) |>
    slice_max(t_sec, n = 1, with_ties = FALSE) |>
    ungroup()

  fv_long <- bind_rows(
    tibble(symbol = symbols[1], minute = fv$t_sec / 60, value = as.numeric(fv$stock1)),
    if (length(symbols) > 1) tibble(symbol = symbols[2], minute = fv$t_sec / 60, value = as.numeric(fv$stock2))
  ) |> filter(!is.na(value))

  bounds <- if (n == 3) {
    fv_long |> group_by(symbol) |> summarise(lo = 0.5 * median(value), hi = 2 * median(value))
  } else if (n == 4) {
    fv_long |> group_by(symbol) |> summarise(lo = 0, hi = 2 * max(value))
  } else {
    tibble(symbol = symbols, lo = -Inf, hi = Inf)
  }

  q <- q |>
    left_join(bounds, by = "symbol") |>
    mutate(bid = if_else(!is.na(bid) & (bid < lo | bid > hi), NA_real_, bid),
           ask = if_else(!is.na(ask) & (ask < lo | ask > hi), NA_real_, ask))

  q_long <- bind_rows(
    q |> transmute(symbol, minute = sec / 60, side = "Best bid", price = bid),
    q |> transmute(symbol, minute = sec / 60, side = "Best ask", price = ask)
  )

  p <- ggplot() +
    geom_step(data = q_long, aes(minute, price, colour = side), linewidth = 0.6, na.rm = FALSE) +
    geom_line(data = fv_long, aes(minute, value, colour = "Fundamental value"), linewidth = 0.9) +
    facet_wrap(~symbol, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(
      "Best bid" = "#2166ac", "Best ask" = "#b2182b", "Fundamental value" = "#1b7837"
    )) +
    labs(x = "minutes from the open", y = if (n == 2) "price (log scale)" else "price", colour = NULL) +
    theme_portfolio +
    (if (length(symbols) == 1) theme(strip.text = element_blank()) else NULL) +
    (if (n == 2) scale_y_log10(breaks = c(50, 100, 200, 500, 1000, 5000, 50000), labels = scales::comma) else NULL)
  save_chart(p, sprintf("session-%d-price-vs-fv", n), height = if (length(symbols) > 1) 6 else 4.5)
}
