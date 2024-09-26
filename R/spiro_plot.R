#' Plot data from cardiopulmonary exercise data files
#'
#' \code{spiro_plot()} returns a \code{ggplot2} graph visualizing data from
#' cardiopulmonary exercise testing.
#'
#' This function provides a shortcut for visualizing data from metabolic carts
#' processed by the \code{\link{spiro}} function.
#'
#' ## Customization
#' There are three ways to customize the appearance of plots in
#' \code{spiro_plot}. First, you can control the color and size of points and
#' lines with the \code{style_args} argument. For a list of available arguments
#' that should be passed in form of a list, see below. Second, you can change
#' the appearance of axis and plot elements (e.g, axis titles, panel lines) by
#' passing arguments over to \code{ggplot2::theme()} via the \code{style_args}
#' argument. Third, you can modify the arrangement of plots by the \code{which}
#' argument and customize the arrangement by passing arguments to
#' \code{cowplot::plot_grid()} via the \code{grid_args} argument.
#'
#' ### Style arguments
#' \describe{
#'   \item{\code{size = 2}}{Defines the size of all points}
#'   \item{\code{linewidth = 1}}{Defines the width of all lines}
#'   \item{\code{color_VO2 = "#c00000"}, \code{color_VCO2 = "#0053a4"},
#'     \code{color_VE = "#003300"}, \code{color_VT = "grey30"},
#'     \code{color_RER = "#003300"}, \code{color_HR = "red"},
#'     \code{color_pulse = "pink"}
#'   }{Define the color of lines and points in the following plot panels: VO2
#'     (panel 3,6,9), VCO2 (3,4,5,6,9), VE (1), VT (7), RER (8), HR (2,5),
#'     pulse (2)}
#'   \item{Additional arguments}{Are passed to \code{ggplot2::theme()}}
#' }
#'
#' @param which A numeric integer setting the plot panels to be displayed. The
#'   panels are numbered in the order of the traditional Wasserman 9-Panel
#'   Plot:
#' * 1: VE over time
#' * 2: HR and oxygen pulse over time
#' * 3: VO2, VCO2 and load over time
#' * 4: VE over VCO2
#' * 5: V-Slope: HR and VCO2 over VO2
#' * 6: EQVO2 and EQVCO2 over time
#' * 7: VT over VE
#' * 8: RER over time
#' * 9: PetO2 and PetCO2 over time
#' @param smooth Parameter giving the filter methods for smoothing the data.
#'   Default is \code{fz} for a zero phase Butterworth filter. See
#'   \code{\link{spiro_smooth}} for more details and other filter methods (e.g.
#'   time based averages)
#' @param vert_lines Whether vertical lines should be displayed at the
#'   time points of the first warm-up load, first load, and last load.
#'   Defaults to FALSE.
#' @param base_size An integer controlling the base size of the plots (in pts).
#' @param style_args A list of arguments controlling the color and size of lines
#'   and points. See the section \strong{'Customization'} for possible
#'   arguments. Additional arguments are passed to ggplot2::theme() to modify
#'   the appearance of the plots.
#' @param grid_args A list of arguments passed to \code{cowplot::plot_grid()} to
#'   modify the arrangement of the plots.
#'
#' @inheritParams spiro_max
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' # Import and process example data
#' ramp_data <- spiro(
#'   file = spiro_example("zan_ramp"),
#'   hr_file = spiro_example("hr_ramp.tcx")
#' )
#'
#' # Display the traditional Wasserman 9-Panel Plot
#' spiro_plot(ramp_data)
#'
#' # Display selected panels, here V-Slope
#' spiro_plot(ramp_data, which = 5)
#'
#' # Modify the arrangement of plots by passing arguments to
#' # cowplot::plot_grid() via the grid_args argument
#' spiro_plot(ramp_data, which = c(4, 5, 6, 8), grid_args = list(nrow = 1))
#'
#' # Modify the appearance of plots using the style_args argument
#' spiro_plot(ramp_data, style_args = list(size = 0.3, color_VCO2 = "black"))
#'
#' # Modify the appearance of plots by passing arguments to ggplot2::theme() via
#' # the style_args argument
#' spiro_plot(ramp_data,
#'   style_args = list(axis.title.x = ggplot2::element_text(colour = "green"))
#' )
#' }
#' @export

spiro_plot <- function(data,
                       which = 1:9,
                       smooth = "fz",
                       base_size = 13,
                       style_args = list(),
                       grid_args = list(),
                       vert_lines = FALSE) {
  # input validation for `which` argument
  if (!is.numeric(which) || !all(which %in% 1:9)) {
    stop("'which' must be a numeric vector containing integers between 1 and 9")
  }
  # input validation for `grid_args` argument
  if (!is.list(grid_args)) {
    stop("'grid_args' must be a list")
  }
  # input validation for `style_args` argument
  if (!is.list(style_args)) {
    stop("'style_args' must be a list")
  }
  # input validation for `vert_lines` argument
  if (!is.logical(vert_lines)) {
    stop("'vert_lines' must be either TRUE or FALSE")
  }

  style_args$data <- data
  style_args$smooth <- smooth
  style_args$base_size <- base_size
  style_args$vert_lines <- vert_lines

  l <- lapply(which, run_spiro_plot, args = style_args)
  grid_args$plotlist <- l
  do.call(cowplot::plot_grid, args = grid_args)
}

run_spiro_plot <- function(which, args) {
  args$which <- which
  do.call(spiro_plot.internal, args = args)
}

spiro_plot.internal <- function(which,
                                data,
                                smooth,
                                base_size = 15,
                                linewidth = 1,
                                size = 2,
                                color_VO2 = "#c00000",
                                color_VCO2 = "#0053a4",
                                color_VE = "#003300",
                                color_VT = "grey30",
                                color_RER = "#003300",
                                color_HR = "red",
                                color_pulse = "pink",
                                vert_lines = FALSE,
                                ...) {
  p <- switch(which,
    `1` = spiro_plot_VE(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_VE = color_VE,
      ...
    ),
    `2` = spiro_plot_HR(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_HR = color_HR, color_pulse = color_pulse,
      ...
    ),
    `3` = spiro_plot_VO2(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_VO2 = color_VO2, color_VCO2 = color_VCO2,
      ...
    ),
    `4` = spiro_plot_EQCO2(
      data,
      base_size = base_size,
      size = size, color_VCO2 = color_VCO2,
      ...
    ),
    `5` = spiro_plot_vslope(
      data,
      base_size = base_size,
      size = size, color_HR = color_HR, color_VCO2 = color_VCO2,
      ...
    ),
    `6` = spiro_plot_EQ(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_VO2 = color_VO2, color_VCO2 = color_VCO2,
      ...
    ),
    `7` = spiro_plot_vent(
      data,
      base_size = base_size,
      size = size, color_VT = color_VT,
      ...
    ),
    `8` = spiro_plot_RER(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_RER = color_RER,
      ...
    ),
    `9` = spiro_plot_Pet(
      data, smooth,
      base_size = base_size, vert_lines = vert_lines,
      linewidth = linewidth, color_VO2 = color_VO2, color_VCO2 = color_VCO2,
      ...
    )
  )
  p
}


#' Plot ventilation over time
#'
#' @noRd
spiro_plot_VE <- function(data,
                          smooth = "fz",
                          base_size = 13,
                          linewidth = 1,
                          color_VE = "#003300",
                          vert_lines = FALSE,
                          ...) {
  d <- spiro_smooth(data, smooth = smooth, columns = "VE")
  # use raw breath time data if smoothing method is breath-based
  if (nrow(spiro_raw(data)) == nrow(d)) {
    d$t <- spiro_raw(data)$time
  } else {
    d$t <- data$time
  }

  ggplot2::ggplot(
    data = d,
    ggplot2::aes(x = d$t, y = d$VE, colour = "VE (l/min)")
  ) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(linewidth = linewidth) +
    ggplot2::scale_colour_manual(values = color_VE) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}
#' Plot heartrate and oxygen pulse over time
#'
#' @noRd
spiro_plot_HR <- function(data,
                          smooth = "fz",
                          base_size = 13,
                          linewidth = 1,
                          color_HR = "red",
                          color_pulse = "pink",
                          vert_lines = FALSE,
                          ...) {
  sec_factor <- 5

  # Rewrite null values from heart rate to NAs
  data$HR[which(data$HR == 0)] <- NA

  if (!all(is.na(data$HR))) {
    d <- spiro_smooth(data, smooth = smooth, columns = c("VO2", "HR"))
    # use raw breath time data if smoothing method is breath-based
    if (nrow(spiro_raw(data)) == nrow(d)) {
      d$t <- spiro_raw(data)$time
    } else {
      d$t <- data$time
    }

    # if a breath-based average is chosen but the raw breath data does not
    # contain HR data this will yield only NAs. In this case the time-based
    # average will be calculated displaying a message.
    if (all(is.na(d$HR))) {
      hr <- spiro_smooth(
        data = data,
        smooth = smooth,
        columns = c("HR", "RER"),
        quiet = TRUE
      )
      # scale heart rate data to
      d$HR <- stats::approx(seq_along(hr$HR), hr$HR, xout = d$t)$y
      message(
        "For heart rate data, smoothing was based on interpolated values."
      )
    }
  } else {
    d <- data.frame(
      t = data$time,
      VO2 = data$VO2,
      HR = NA
    )
  }

  d$pulse <- sec_factor * d$VO2 / d$HR

  # find and handle time duplicates
  # in some rare cases raw time data may contain duplicates
  d$t <- dupl(d$t)

  d_long <- stats::reshape(d,
    direction = "long",
    varying = c("pulse", "HR"),
    v.names = "value",
    idvar = c("t"),
    times = c("pulse", "HR"),
    timevar = "measure"
  )
  d_long$measure <- factor(d_long$measure,
    levels = c("HR", "pulse"),
    labels = c("HR (bpm)", "VO2/HR (ml)")
  )

  ggplot2::ggplot(data = d_long, ggplot2::aes(x = d_long$t)) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(
      mapping = ggplot2::aes(y = d_long$value, colour = d_long$measure),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_manual(values = c(color_HR, color_pulse)) +
    list(
      # create a second y-axis only if data values are available as ggplot2
      # returns an error if sec_axis() is applied to all NAs
      if (!all(is.na(d_long$value))) {
        ggplot2::scale_y_continuous(
          limits = c(0, 225),
          sec.axis = ggplot2::sec_axis(~ . / sec_factor)
        )
      } else {
        ggplot2::scale_y_continuous(
          limits = c(0, 225)
        )
      }
    ) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Plot oxygen uptake, carbon dioxide output and load over time
#'
#' @noRd
spiro_plot_VO2 <- function(data,
                           smooth = "fz",
                           base_size = 13,
                           linewidth = 1,
                           color_VO2 = "#c00000",
                           color_VCO2 = "#0053a4",
                           vert_lines = FALSE,
                           ...) {
  yl <- spiro_plot.guess_units(data)

  # create data frame with smoothed values
  v_smooth <- spiro_smooth(data, smooth, c("VO2", "VCO2"))
  bodymass <- attr(data, "info")$bodymass

  tl_data <- data.frame(
    time = data$time,
    load = data$load,
    load_scaled = data$load * yl[[1]]
  )

  # use raw breath time data if smoothing method is breath-based
  if (nrow(spiro_raw(data)) == nrow(v_smooth)) {
    t_data <- spiro_raw(data)$time
  } else {
    t_data <- data$time
  }

  # create data frame with smoothed data
  v_data <- data.frame(
    time = t_data,
    VO2_rel = v_smooth$VO2 / bodymass,
    VCO2_rel = v_smooth$VCO2 / bodymass
  )

  # find and handle time duplicates
  # in some rare cases raw time data may contain duplicates
  v_data$time <- dupl(v_data$time)

  # reshape data into long format
  v_data_long <- stats::reshape(v_data,
    direction = "long",
    varying = c("VO2_rel", "VCO2_rel"),
    v.names = "value",
    idvar = c("time"),
    times = c("VO2_rel", "VCO2_rel"),
    timevar = "measure"
  )

  v_data_long$measure <- factor(v_data_long$measure,
    levels = c("VO2_rel", "VCO2_rel"),
    labels = c("VO2 (ml/min/kg)", "VCO2 (ml/min/kg)")
  )

  ggplot2::ggplot(NULL) +
    ggplot2::geom_area(
      data = NULL,
      ggplot2::aes(x = tl_data$time, y = tl_data$load_scaled),
      fill = "black", alpha = 0.2, position = "identity"
    ) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(
      data = v_data_long,
      mapping = ggplot2::aes(
        x = v_data_long$time,
        y = v_data_long$value,
        colour = v_data_long$measure
      ),
      linewidth = linewidth
    ) +
    list(
      if (!all(tl_data$load_scaled == 0)) {
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(~ . / yl[[1]], name = yl[[2]])
        )
      } else {
        NULL
      }
    ) +
    ggplot2::scale_color_manual(values = c(color_VO2, color_VCO2)) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Plot VCO2 vs. VE
#'
#' @noRd
spiro_plot_EQCO2 <- function(data,
                             base_size = 13,
                             size = 2,
                             color_VCO2 = "#0053a4",
                             ...) {
  raw <- spiro_raw(data)
  # bring VCO2 data into desired unit (l/min)
  raw$VCO2 <- raw$VCO2 / 1000

  ggplot2::ggplot(data = raw, ggplot2::aes(x = raw$VCO2, y = raw$VE)) +
    ggplot2::geom_point(
      size = size,
      shape = 21,
      fill = color_VCO2,
      colour = "white",
      na.rm = TRUE
    ) +
    ggplot2::labs(x = "VCO2 (l/min)", y = "VE (l/min)") +
    theme_spiro(base_size, ...)
}

#' Plot V-Slope graph
#'
#' @noRd
spiro_plot_vslope <- function(data,
                              base_size = 13,
                              size = 2,
                              color_HR = "red",
                              color_VCO2 = "#0053a4",
                              ...) {
  raw <- spiro_raw(data)
  # remove rows without time stamp
  raw <- raw[!is.na(raw$time), ]

  # match HR to breath-by-breath raw data if no raw heartrate data is available
  if (!(any(raw$HR != 0, na.rm = TRUE))) {
    raw$HR <- data$HR[replace(round(raw$time), round(raw$time) == 0, 1)]
  }
  # bring VO2 data into desired unit (l/min)
  raw$VO2 <- raw$VO2 / 1000
  # scale VCO2 data for being displayed on second y-axis
  raw$VCO2 <- raw$VCO2 / 20
  raw <- raw[, c("time", "HR", "VO2", "VCO2")]

  # find and handle time duplicates
  # in some rare cases raw time data may contain duplicates
  raw$time <- dupl(raw$time)

  raw_long <- stats::reshape(raw,
    direction = "long",
    varying = c("HR", "VCO2"),
    v.names = "value",
    idvar = c("time"),
    times = c("HR", "VCO2"),
    timevar = "measure"
  )
  raw_long$measure <- factor(raw_long$measure,
    levels = c("HR", "VCO2"),
    labels = c("HR (bpm)", "VCO2 (l/min)")
  )

  ggplot2::ggplot(
    data = raw_long,
    mapping = ggplot2::aes(
      x = raw_long$VO2,
      y = raw_long$value,
      fill = raw_long$measure
    )
  ) +
    ggplot2::geom_point(
      size = size,
      shape = 21,
      colour = "white",
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(values = c(color_HR, color_VCO2)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / 50)) +
    ggplot2::labs(x = "VO2 (l/min)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Plot EQVO2 and EQCO2 over time
#'
#' @noRd
spiro_plot_EQ <- function(data,
                          smooth = "fz",
                          base_size = 13,
                          linewidth = 1,
                          color_VO2 = "#c00000",
                          color_VCO2 = "#0053a4",
                          vert_lines = FALSE,
                          ...) {
  # use calculated EQ data for smoothing if measurement method is not
  # breath-by-breath
  if (check_bb(spiro_raw(data)$time)) {
    d <- spiro_smooth(data, smooth = smooth, columns = c("VO2", "VCO2", "VE"))
    d$EQ_O2 <- 1000 * d$VE / d$VO2
    d$EQ_CO2 <- 1000 * d$VE / d$VCO2
  } else {
    data$EQ_O2 <- 1000 * data$VE / data$VO2
    data$EQ_CO2 <- 1000 * data$VE / data$VCO2
    d <- spiro_smooth(data, smooth = smooth, columns = c("EQ_O2", "EQ_CO2"))
    # Remove implausible values
    d$EQ_O2[which(d$EQ_O2 > 50 | d$EQ_O2 < 10)] <- NA
    d$EQ_CO2[which(d$EQ_CO2 > 50 | d$EQ_CO2 < 10)] <- NA
  }

  # use raw breath time data if smoothing method is breath-based
  if (nrow(spiro_raw(data)) == nrow(d)) {
    d$t <- spiro_raw(data)$time
  } else {
    d$t <- data$time
  }

  # find and handle time duplicates
  # in some rare cases raw time data may contain duplicates
  d$t <- dupl(d$t)

  d_long <- stats::reshape(d,
    direction = "long",
    varying = c("EQ_O2", "EQ_CO2"),
    v.names = "value",
    idvar = c("t"),
    times = c("EQ_O2", "EQ_CO2"),
    timevar = "measure"
  )
  d_long$measure <- factor(d_long$measure, levels = c("EQ_O2", "EQ_CO2"))

  ggplot2::ggplot(data = d_long, ggplot2::aes(x = d_long$t)) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(
      mapping = ggplot2::aes(y = d_long$value, colour = d_long$measure),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_manual(values = c(color_VO2, color_VCO2)) +
    ggplot2::scale_y_continuous(limits = function(x) c(x[[1]] - 5, x[[2]])) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Plot VE vs. RR
#'
#' @noRd
spiro_plot_vent <- function(data,
                            base_size = 13,
                            size = 2,
                            color_VT = "grey30",
                            ...) {
  raw <- spiro_raw(data)

  ggplot2::ggplot(data = raw, ggplot2::aes(x = raw$VE, y = raw$VT)) +
    ggplot2::geom_point(
      size = size,
      shape = 21,
      fill = color_VT,
      colour = "white",
      na.rm = TRUE
    ) +
    ggplot2::labs(x = "VE (l/min)", y = "VT (l)") +
    theme_spiro(base_size, ...)
}

#' Plot RER over time
#'
#' @noRd
spiro_plot_RER <- function(data,
                           smooth = "fz",
                           base_size = 13,
                           linewidth = 1,
                           color_RER = "#003300",
                           vert_lines = FALSE,
                           ...) {
  # use calculated RER data for smoothing if measurement method is not
  # breath-by-breath
  if (check_bb(spiro_raw(data)$time)) {
    d <- spiro_smooth(data, smooth = smooth, columns = c("VO2", "VCO2"))
    d$RER <- d$VCO2 / d$VO2
  } else {
    d <- spiro_smooth(data, smooth = smooth, columns = "RER")
  }

  # use raw breath time data if smoothing method is breath-based
  if (nrow(spiro_raw(data)) == nrow(d)) {
    d$t <- spiro_raw(data)$time
  } else {
    d$t <- data$time
  }

  ggplot2::ggplot(data = d, ggplot2::aes(x = d$t)) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(
      mapping = ggplot2::aes(y = d$RER, colour = "RER"),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_manual(values = color_RER) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Plot PetO2 and PetCO2 over time
#'
#' @noRd
spiro_plot_Pet <- function(data,
                           smooth = "fz",
                           base_size = 13,
                           linewidth = 1,
                           color_VO2 = "#c00000",
                           color_VCO2 = "#0053a4",
                           vert_lines = FALSE,
                           ...) {
  if (!all(is.na(data$PetO2))) {
    d <- spiro_smooth(data, smooth = smooth, columns = c("PetO2", "PetCO2"))

    # use raw breath time data if smoothing method is breath-based
    if (nrow(spiro_raw(data)) == nrow(d)) {
      d$time <- spiro_raw(data)$time
    } else {
      d$time <- data$time
    }
  } else {
    d <- data.frame(
      time = data$time,
      # returns error if NAs are interpreted as logical
      PetO2 = as.numeric(NA),
      PetCO2 = as.numeric(NA)
    )
  }

  # find and handle time duplicates
  # in some rare cases raw time data may contain duplicates
  d$time <- dupl(d$time)

  d_long <- stats::reshape(d,
    direction = "long",
    varying = c("PetO2", "PetCO2"),
    v.names = "value",
    idvar = c("time"),
    times = c("PetO2", "PetCO2"),
    timevar = "measure"
  )
  d_long$measure <- factor(d_long$measure,
    levels = c("PetO2", "PetCO2"),
    labels = c("PetO2 (mmHG)", "PetCO2 (mmHg)")
  )

  ggplot2::ggplot(data = d_long, ggplot2::aes(x = d_long$time)) +
    vert_lines(data = data, plot = vert_lines) +
    plot_lines(
      mapping = ggplot2::aes(y = d_long$value, colour = d_long$measure),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_manual(values = c(color_VO2, color_VCO2)) +
    ggplot2::scale_y_continuous(limits = c(0, 150)) +
    ggplot2::labs(x = "Duration (s)", y = NULL) +
    theme_spiro(base_size, ...)
}

#' Adjust axes in spiroergometric data plot
#'
#' Internal function to \code{?link{spiro_plot}}
#'
#' @param data A \code{data.frame} of the class \code{spiro_*}.
#' @noRd
spiro_plot.guess_units <- function(data) {
  ymax <- max(data$load, na.rm = TRUE)
  if (ymax <= 8) {
    yscale <- 5
    ylabel <- "Velocity (m/s)"
  } else if (ymax <= 30) {
    yscale <- 2
    ylabel <- "Velocity (km/h)"
  } else {
    yscale <- 0.1
    ylabel <- "Power (W)"
  }
  out <- list(yscale, ylabel)
  out
}

#' Internal ggplot2 theme for spiro plots
#'
#' @param base_size An integer, giving the base size for the theme.
#' @param ... Arguments passed to ggplot2::theme()
#'
#' @noRd
theme_spiro <- function(base_size = 13,
                        panel.grid.minor.x = ggplot2::element_blank(),
                        legend.title = ggplot2::element_blank(),
                        legend.position = c(1, 0),
                        legend.position.inside = c(1, 0),
                        legend.justification = c(1, 0),
                        legend.justification.inside = c(1, 0),
                        ...) {
  # ggplot version 3.5 or newer has a legend.position.inside argument in theme()
  # with legend.position = "inside" instead of a numeric vector as input
  # also legend.justification is replaced by legend.justification.inside
  if (utils::packageVersion("ggplot2") >= "3.5") {
    list(
      ggplot2::theme_minimal(base_size = base_size),
      ggplot2::theme(
        panel.grid.minor.x = panel.grid.minor.x,
        legend.title = legend.title,
        legend.position = "inside",
        legend.position.inside = legend.position.inside,
        legend.justification.inside = legend.justification.inside,
        ...
      )
    )
  } else {
    list(
      ggplot2::theme_minimal(base_size = base_size),
      ggplot2::theme(
        panel.grid.minor.x = panel.grid.minor.x,
        legend.title = legend.title,
        legend.position = legend.position,
        legend.justification = legend.justification,
        ...
      )
    )
  }
}

#' Plot lines in spiro_plot() functions depending on ggborderline availability
#' and ggplot2 version
#'
#' Uses the ggborderline package if available to plot lines. Use the linewidth
#' aesthetic for ggplot2 version >= 3.4 and size aesthetic for older versions.
#'
#' @param data Passed to ggplot2::geom_line() or ggborderline::geom_borderline()
#' @param mapping Passed to ggplot2::geom_line() or
#'   ggborderline::geom_borderline()
#' @param linewidth Passed as linewidth or size depending on the available
#'   ggplot2 version. Defaults to 1.
#' @param na.rm Passed to ggplot2::geom_line() or
#'   ggborderline::geom_borderline(). Defaults to TRUE.
#'
#' @noRd
plot_lines <- function(data = NULL,
                       mapping = NULL,
                       linewidth = 1,
                       na.rm = TRUE) {
  list(
    if (!requireNamespace("ggborderline", quietly = TRUE)) {
      if (utils::packageVersion("ggplot2") >= "3.4") {
        ggplot2::geom_line(
          data = data,
          mapping = mapping,
          linewidth = linewidth,
          na.rm = na.rm
        )
      } else {
        ggplot2::geom_line(
          data = data,
          mapping = mapping,
          size = linewidth,
          na.rm = na.rm
        )
      }
    } else {
      if (utils::packageVersion("ggplot2") >= "3.4") {
        ggborderline::geom_borderline(
          data = data,
          mapping = mapping,
          linewidth = linewidth,
          na.rm = na.rm
        )
      } else {
        ggborderline::geom_borderline(
          data = data,
          mapping = mapping,
          size = linewidth,
          na.rm = na.rm
        )
      }
    }
  )
}

#' Plot vertical lines in spiro_plot() functions at selected time points of the
#' exercise protocol
#'
#' @param data A data.frame of the class `spiro`, usually the output of
#'   `spiro()`
#'
#' @noRd
vert_lines <- function(data, plot = TRUE) {
  ptcl <- attr(data, "protocol")
  list(
    # check protocol availability
    if (is.null(ptcl) | isFALSE(plot)) {
      NULL
    } else {
      cs <- cumsum(ptcl$duration)
      # set empty time points
      t1 <- NULL
      t2 <- NULL
      t3 <- NULL
      # time point 1: first load
      if (any(ptcl$type == "load")) {
        t1i <- min(which(ptcl$type == "load"))
        if (t1i >= 2) { # requires measurements prior to first load
          t1 <- cs[t1i-1]
        }
      }
      # time point 2: first warm-up load
      if (any(ptcl$type == "warm up")) {
        t2i <- min(which(ptcl$type == "warm up"))
        if (t2i >= 2) { # requires measurements prior to first load
          t2 <- cs[t2i-1]
        }
      }
      # time point 3: last load
      if (any(ptcl$type == "load")) {
        t3i <- max(which(ptcl$type == "load"))
        # only apply if there are measurements after the last load
        if (t3i != nrow(ptcl)) {
          t3 <- cs[t3i]
        }
      }
      ggplot2::geom_vline(
        xintercept = c(t1, t2, t3),
        colour = "#e3ad0f"
      )
    }
  )
}
