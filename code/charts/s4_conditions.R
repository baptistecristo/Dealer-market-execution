# Session 4: when I traded, against the price and the quoted spread.
# Top row: traded price (median per 30 seconds) with my own fills.
# Bottom row: quoted spread as a % of the mid (time-weighted median per 30 seconds),
# with the spread each of my fills paid. Values above the axis limit are drawn on
# the edge as triangles (legend key), not labelled: the charts carry no in-panel text.
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(.f)), "common.R"))

quotes <- read_data("session-4", "quotes.csv")
trades <- read_data("session-4", "trades.csv")
fv <- read_data("session-4", "fv.csv")

tasks <- c(STOCK1 = "STOCK1", STOCK2 = "STOCK2")
price_cap <- c(STOCK1 = 2 * max(fv$stock1, na.rm = TRUE), STOCK2 = 2 * max(fv$stock2, na.rm = TRUE))
spread_cap <- 200
rows <- c("price", "spread, % of mid")
row_labels <- c(sprintf("traded price"), sprintf("spread, %% of mid (capped at %d)", spread_cap))

spread <- quotes |>
  arrange(symbol, t_sec) |>
  group_by(symbol) |>
  mutate(dt = lead(t_sec, default = max(t_sec)) - t_sec) |>
  ungroup() |>
  filter(!is.na(bid), !is.na(ask), dt > 0) |>
  mutate(rel = (ask - bid) / ((ask + bid) / 2) * 100, bucket = floor(t_sec / 30) * 30) |>
  group_by(symbol, bucket) |>
  arrange(rel, .by_group = TRUE) |>
  summarise(value = rel[which(cumsum(dt) >= sum(dt) / 2)[1]], .groups = "drop") |>
  mutate(row = rows[2], value = pmin(value, spread_cap))

price <- trades |>
  mutate(bucket = floor(t_sec / 30) * 30) |>
  group_by(symbol, bucket) |>
  summarise(value = median(price), .groups = "drop") |>
  mutate(row = rows[1], value = pmin(value, price_cap[symbol]))

# One point per market order: its fills are grouped back to the order that
# caused them, the price is the order's average fill and the spread is the one
# its first fill paid. Point area is proportional to the order size.
orders <- read_data("session-4", "my-orders.csv") |>
  arrange(t_sec) |>
  mutate(order_id = row_number())

fills <- trades |>
  filter(buyer == "ME" | seller == "ME") |>
  mutate(side_raw = if_else(buyer == "ME", "buy", "sell"),
         rel = (ask - bid) / ((ask + bid) / 2) * 100) |>
  arrange(t_sec)
fills$order_id <- vapply(seq_len(nrow(fills)), function(i) {
  cand <- orders$order_id[orders$symbol == fills$symbol[i] & orders$side == fills$side_raw[i] &
                            abs(orders$t_sec - fills$t_sec[i]) < 0.01]
  max(cand)
}, numeric(1))

mine <- fills |>
  group_by(order_id) |>
  summarise(symbol = first(symbol), side_raw = first(side_raw), t_sec = first(t_sec),
            price = sum(price * qty) / sum(qty), qty = sum(qty), rel = first(rel), .groups = "drop") |>
  mutate(side = if_else(side_raw == "buy", "Market buy order", "Market sell order"))
stopifnot(sum(mine$qty) == sum(orders$filled_qty), nrow(mine) == nrow(orders))

mine_long <- bind_rows(
  mine |> transmute(symbol, t_sec, side, qty, raw = price, row = rows[1],
                    value = pmin(price, price_cap[symbol]), off = price > price_cap[symbol]),
  mine |> transmute(symbol, t_sec, side, qty, raw = rel, row = rows[2],
                    value = pmin(rel, spread_cap), off = rel > spread_cap)
)

mine_long <- mine_long |>
  mutate(scale = if_else(off, "Beyond axis limit, drawn at edge", "Within axis"))

lines <- bind_rows(price, spread) |> mutate(what = "Market, median per 30 s")
for (d in c("lines", "mine_long")) {
  x <- get(d)
  x$task <- factor(tasks[x$symbol], levels = tasks)
  x$row <- factor(x$row, levels = rows)
  assign(d, x)
}

p <- ggplot() +
  geom_line(data = lines, aes(bucket / 60, value, colour = what), linewidth = 0.45) +
  geom_point(data = mine_long, aes(t_sec / 60, value, fill = side, size = qty, shape = scale),
             colour = "black", stroke = 0.3) +
  facet_wrap(vars(row, task), ncol = 2, scales = "free_y",
             labeller = function(l) list(paste0(l$task, " · ", row_labels[match(l$row, rows)]))) +
  scale_colour_manual(values = c("Market, median per 30 s" = "grey35")) +
  scale_fill_manual(values = c("Market buy order" = "#1b7837", "Market sell order" = "#b2182b"),
                    guide = guide_legend(override.aes = list(shape = 21, size = 3))) +
  scale_shape_manual(values = c("Within axis" = 21, "Beyond axis limit, drawn at edge" = 24),
                     guide = guide_legend(override.aes = list(size = 3, fill = "grey70"))) +
  scale_size_area(max_size = 7, limits = c(0, 3000), breaks = c(500, 1000, 3000),
                  labels = scales::label_comma(),
                  guide = guide_legend(override.aes = list(shape = 21, fill = "grey70"))) +
  labs(x = "minutes from the open", y = NULL, fill = NULL, colour = NULL, shape = NULL, size = "Order size (shares)") +
  theme_portfolio +
  theme(legend.box = "vertical", legend.margin = margin(0, 0, 0, 0))
save_chart(p, "session-4-conditions", height = 6)
