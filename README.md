# Optimal execution in a classroom dealer market

I wrote this note for FIN11 Trading and Market Microstructure at NHH, in autumn 2026. The course
runs live trading sessions on a simulated market, and the note covers the fourth one. I was an
asset exchanger: I had to sell 10,000 shares of one stock and buy 10,000 of another within the
hour, using market orders only, while dealers set every price. Part 1 is an account of how I
traded and where it went wrong. Part 2 uses the official session data to test Bertsimas and Lo's
(1998) model of how to spread a large order over time.

**[Read the note (PDF)](FIN11-note-session-4.pdf)**

## Code

The R scripts in `code/charts/` draw Figures 1 to 3. They expect the session files in
`data/session-4/` (quotes, trades, fundamental value), which are not part of this repository.
An anonymised copy is in [Market microstructure lab](https://github.com/baptistecristo/Market-microstructure-lab).

| script | figure |
|---|---|
| `price_vs_fv.R` | Figure 1, best bid and ask against fundamental value. This version plots the quotes second by second; the figure in the note uses a median per 30 seconds. |
| `s4_conditions.R` | Figure 2, my market orders against the traded price and the quoted spread |
| `s4_inventory.R` | Figure 3, my position against the ±5,000 band |

`common.R` holds the shared setup. Figures 4 to 7 were made separately and their code is not
included.
