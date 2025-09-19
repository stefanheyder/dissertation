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

n_weekly_breaks <- function(n = 1, week_start = 7) {
    function(x) {
        seq(ceiling_date(x[1], "week", week_start = week_start), floor_date(x[2], "week", week_start = week_start), by = paste(n, "week"))
    }
}
weekly_breaks <- function(x, week_start=7) {
    seq(ceiling_date(x[1], "week", week_start = week_start), floor_date(x[2], "week", week_start = week_start), by = "1 week")
}
four_weekly_breaks <- function(x, week_start=7) {
    seq(ceiling_date(x[1], "week", week_start = week_start), floor_date(x[2], "week", week_start = week_start), by = "4 week")
}

scale_x_four_weekly <- function(week_start=7) {
	list(
		scale_x_date(
			breaks = function(x) four_weekly_breaks(x, week_start = week_start),
			minor_breaks = function(x) weekly_breaks(x, week_start = week_start),
			date_labels = "%d %b %y",
			expand = expansion(mult = c(0.01, 0.01))
		),
		theme(
			panel.grid.major.x = element_line(linewidth = 1.2),
			panel.grid.minor.x = element_line(linewidth = 0.5)
		)
	)
}

default_probs <- c(0.01, 0.025, 0.05 * 1:19, 0.975, 0.99)
read_predictions <- function(fname, dates, variables, probs = default_probs) {
    np <- import("numpy")
    predictions <- np$load(fname)

    dimnames(predictions) <- list(
        "type" = c("mean", "sd", probs),
        "date" = as.character(dates),
        "variable" = variables
    )

    df_predictions <- melt(predictions) %>%
        mutate(date = ymd(date)) %>%
        pivot_wider(id_cols = c(date, variable), names_from = "type", values_from = "value")
    df_predictions
}

ordered_age_group <- function(age_group) {
	ordered(age_group, levels = c("A00-04", "A05-14", "A15-34", "A35-59", "A60-79", "A80+", "A00+"))
}

# plotting helpers for age groups
age_group_labels <- c(
    `00+` = "all ages",
    `00-04` = "ages 00-04",
    `05-14` = "ages 05-14",
    `15-34` = "ages 15-34",
    `35-59` = "ages 35-59",
    `60-79` = "ages 60-79",
    `80+` = "ages 80+"
)

age_group_labeller <- as_labeller(age_group_labels)
age_order_4cols <- c("00-04", "05-14", "15-34", "00+", "35-59", "60-79", "80+")


WIS_decompose <- function(prob, quant, actual) {
  # Identify the median forecast
  median_idx <- which(prob == 0.5)
  median_forecast <- quant[median_idx]
  mean_error <- abs(actual - median_forecast)
  
  # Remove median for interval processing
  interval_idx <- setdiff(seq_along(prob), median_idx)
  prob_intervals <- prob[interval_idx]
  quant_intervals <- quant[interval_idx]
  
  K <- length(prob_intervals) / 2
  total_weight <- 0.5 + K  # 0.5 for median, alpha/2 per interval
  
  sharpness <- 0
  overprediction <- 0
  underprediction <- 0
  
  for (i in seq_len(K)) {
    lower_idx <- i
    upper_idx <- length(prob_intervals) - i + 1
    alpha <- 2 * prob_intervals[lower_idx]
    lower <- quant_intervals[lower_idx]
    upper <- quant_intervals[upper_idx]
    weight <- alpha / 2
    
    # Interval score components
    sharpness <- sharpness + weight * (upper - lower)

    underprediction <- underprediction + weight * ifelse(actual > upper, (2/alpha)*(actual - upper), 0)
    overprediction <- overprediction + weight * ifelse(actual < lower, (2/alpha)*(lower - actual), 0)
  }
  
  normalized_sharpness <- sharpness / total_weight
  normalized_underprediction <- underprediction / total_weight
  normalized_overprediction <- overprediction / total_weight
  normalized_mean_error <- (0.5 * mean_error) / total_weight
  
  wis <- normalized_sharpness + normalized_overprediction + normalized_underprediction + normalized_mean_error
  return(list(
    wis = wis,
    sharpness = normalized_sharpness,
    underprediction = normalized_underprediction,
    overprediction = normalized_overprediction,
    mean_error = normalized_mean_error
  ))
}

# Example usage:
# prob <- c(0.025, 0.25, 0.5, 0.75, 0.975)
# quant <- c(10, 15, 20, 25, 30)
# actual <- 18
# result <- calculate_wis_components(prob, quant, actual)
# print(result)
