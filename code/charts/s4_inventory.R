# My position in each stock through Session 4, against the straight line to zero and
# the +/-5,000 band the asset-exchanger role was scored on. The start is computed as
# minus the sum of my own trade deltas, which by construction always lands the path
# back at zero -- it does not independently verify that I actually started or finished
# flat, only that this is the flattest story consistent with my own fills. Net delta
# over the session is -10,000 STOCK1 / +10,000 STOCK2, i.e. the role converted one
# holding into the other; no other file in this export records my true starting
# inventory, so that starting point is not invented beyond this formula.
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(.f)), "common.R"))

trades <- read_data("session-4", "trades.csv")
end_sec <- 3600

mine <- trades |>
  filter(buyer == "ME" | seller == "ME") |>
  mutate(delta = if_else(buyer == "ME", qty, -qty)) |>
  arrange(t_sec) |>
  group_by(symbol) |>
  mutate(traded = cumsum(delta), start = -sum(delta), position = start + traded) |>
  ungroup()

starts <- mine |> distinct(symbol, start)
path <- bind_rows(
  starts |> transmute(symbol, t_sec = 0, position = start),
  mine |> select(symbol, t_sec, position),
  mine |> group_by(symbol) |> summarise(t_sec = end_sec, position = last(position), .groups = "drop")
) |> arrange(symbol, t_sec)

band <- do.call(rbind, lapply(seq_len(nrow(starts)), function(i) {
  t <- seq(0, end_sec, by = 30)
  centre <- starts$start[i] * (1 - t / end_sec)
  data.frame(symbol = starts$symbol[i], t_sec = t, centre = centre, lo = centre - 5000, hi = centre + 5000)
}))

p <- ggplot() +
  geom_ribbon(data = band, aes(t_sec / 60, ymin = lo, ymax = hi, fill = "±5,000 scoring band")) +
  geom_line(data = band, aes(t_sec / 60, centre, linetype = "Straight line to zero"), colour = "#1b7837") +
  geom_step(data = path, aes(t_sec / 60, position, colour = "My position"), linewidth = 0.9) +
  facet_wrap(~symbol, ncol = 1) +
  scale_fill_manual(values = c("±5,000 scoring band" = "#d9f0d3")) +
  scale_linetype_manual(values = c("Straight line to zero" = "dashed")) +
  scale_colour_manual(values = c("My position" = "#5e3c99")) +
  labs(x = "minutes from the open", y = "shares held", fill = NULL, linetype = NULL, colour = NULL) +
  theme_portfolio
save_chart(p, "session-4-inventory", height = 6)
