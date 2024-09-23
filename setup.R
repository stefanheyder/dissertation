options(
    repr.plot.width = 16,
    repr.plot.height = 7,
    # silence tidyverse messages
    tidyverse.quiet = TRUE,
    dplyr.summarise.inform = FALSE,
    dplyr.ungroup.inform = FALSE,
    readr.show_progress = FALSE,
    readr.show_col_types = FALSE
)
# necessary for German XLSX files
Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressPackageStartupMessages({
    library(tidyverse)
    library(scales)
    library(patchwork)
    library(reshape2)
	library(reticulate)
    library(tikzDevice)
    library(ggsci)
    library(here)
    library(zoo)
    library(ggmagnify)
    library(ggforce)
    library(grid)
    library(gtable)
    library(cowplot)
    library(reshape2)
    library(knitr)
    library(kableExtra)
})


# try to shift legend to empty facet
shift_legend <- function(p) {

	# check if p is a valid object
	if(!"gtable" %in% class(p)){
		if("ggplot" %in% class(p)){
			gp <- ggplotGrob(p) # convert to grob
		} else {
			message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
			return(p)
		}
	} else {
		gp <- p
	}

	# check for unfilled facet panels
	facet.panels <- grep("^panel", gp[["layout"]][["name"]])
	empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
	empty.facet.panels <- facet.panels[empty.facet.panels]
	if(length(empty.facet.panels) == 0){
		message("There are no unfilled facet panels to shift legend into. Returning original plot.")
		return(p)
	}

	# establish extent of unfilled facet panels (including any axis cells in between)
	empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
	empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
							   max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
	names(empty.facet.panels) <- c("t", "l", "b", "r")

	# extract legend & copy over to location of unfilled facet panels
	guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
	if(length(guide.grob) == 0){
		message("There is no legend present. Returning original plot.")
		return(p)
	}
	gp <- gtable_add_grob(x = gp,
						  grobs = gp[["grobs"]][[guide.grob]],
						  t = empty.facet.panels[["t"]],
						  l = empty.facet.panels[["l"]],
						  b = empty.facet.panels[["b"]],
						  r = empty.facet.panels[["r"]],
						  name = "new-guide-box")

	# squash the original guide box's row / column (whichever applicable)
	# & empty its cell
	guide.grob <- gp[["layout"]][guide.grob, ]
	if(guide.grob[["l"]] == guide.grob[["r"]]){
		gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
	}
	if(guide.grob[["t"]] == guide.grob[["b"]]){
		gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
	}
	gp <- gtable_remove_grobs(gp, "guide-box")

	return(gp)
}


default_width <- 8
default_height <- 5
ggsave_tikz <- function(filename, plot = last_plot(), width = default_width, height = default_height, ...) {
    tikz(filename, width = width, height = height, ...)
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

weekly_breaks <- function(x) {
    seq(ceiling_date(x[1], "week", week_start = 7), floor_date(x[2], "week", week_start = 7), by = "1 week")
}
four_weekly_breaks <- function(x) {
    seq(ceiling_date(x[1], "week", week_start = 7), floor_date(x[2], "week", week_start = 7), by = "4 week")
}

scale_x_four_weekly <- function() {
	scale_x_date(breaks = four_weekly_breaks, minor_breaks = weekly_breaks, date_labels = "%d %b %y", expand = expansion(mult= c(0.01, 0.01)))
}