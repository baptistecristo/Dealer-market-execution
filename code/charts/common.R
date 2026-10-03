# Shared setup for every chart. Run scripts with Rscript from any folder.
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
})

.this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT <- normalizePath(file.path(dirname(normalizePath(.this_file)), "..", ".."))

data_file <- function(...) file.path(ROOT, "data", ...)

read_data <- function(...) read_csv(data_file(...), show_col_types = FALSE, na = "")

save_chart <- function(plot, name, height = 5) {
  out <- file.path(ROOT, "charts", paste0(name, ".png"))
  dir.create(dirname(out), showWarnings = FALSE)
  ggsave(out, plot, width = 8, height = height, dpi = 200, bg = "white")
  message("wrote ", out)
}

theme_portfolio <- theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", panel.grid.minor = element_blank(), legend.position = "bottom")
