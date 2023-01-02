data <- spiro(spiro_example("zan_ramp"))
data_hr <- add_hr(data, spiro_example("hr_ramp.tcx"))

p <- spiro_plot(data)
p_vertline <- spiro_plot(data, vert_lines = TRUE)
pt <- spiro_plot(data, smooth = 15)
pb <- spiro_plot(data, smooth = "15b")
phr <- spiro_plot(data_hr, smooth = "15b")

p_style <- spiro_plot(data, style_args = list(size = 1, linewidth = 2))
p_color <- spiro_plot(
  data_hr,
  style_args = list(
    color_VO2 = "black", color_VCO2 = "purple", color_RER = "pink",
    color_VE = "royalblue", color_VT = "orange", color_HR = "lightblue",
    color_pulse = "grey")
)
p_theme <- spiro_plot(
  data,
  style_args = list(axis.title = ggplot2::element_text(color = "blue"))
)
p_grid <- spiro_plot(data, which = 1:4, grid_args = list(nrow = 1))

test_that("input is validated", {
  expect_snapshot_error(spiro_plot(data, which = 20))
  expect_snapshot_error(spiro_plot(data, style_args = "nolist"))
  expect_snapshot_error(spiro_plot(data, grid_args = "nolist"))
  expect_snapshot_error(spiro_plot(data, vert_lines = 2))
})

test_that("visualization works", {
  vdiffr::expect_doppelganger("basic", p)
  vdiffr::expect_doppelganger("tsmooth", pt)
  vdiffr::expect_doppelganger("bsmooth", pb)
  vdiffr::expect_doppelganger("hr", phr)
  vdiffr::expect_doppelganger("vertline", p_vertline)
})

test_that("customization works", {
  vdiffr::expect_doppelganger("style", p_style)
  vdiffr::expect_doppelganger("color", p_color)
  vdiffr::expect_doppelganger("theme", p_theme)
  vdiffr::expect_doppelganger("grid", p_grid)
})
