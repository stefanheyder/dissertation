options(
    repr.plot.width = 8, 
    repr.plot.height = 5,
    # silence tidyverse messages
    tidyverse.quiet = TRUE,
    dplyr.summarise.inform = FALSE,
    dplyr.ungroup.inform = FALSE,
    readr.show_progress = FALSE,
    readr.show_col_types = FALSE
)

library(tidyverse)
library(patchwork)
library(tikzDevice)
library(ggsci)

default_width <- 8
default_height <- 5
ggsave_tikz <- function(filename, plot=last_plot(), width=default_width, height=default_height, ...) {
    tikz(filename, width=width,height=height, ...)
    print(plot)
    dev.off()
}

theme_thesis <- function() {
    theme_minimal()
}

theme_set(theme_thesis())

options(
    ggplot2.discrete.colour = scale_color_npg
)