context("Graphic functions return without error")

timeline_plot <- quaketimeline_plot(eq_clean_testdata[8:10,],
                                    "1800-01-01","2014-01-01")

timeline_labelplot <- quaketimelinelabel_plot(eq_china_cleandata,
                                              "1800-01-01","2015-01-01", 3)

test_that("timelineplots works without error", {
  expect_is(timeline_plot, "ggplot")
  expect_is(timeline_labelplot, "ggplot")
})

test_that("Html label returns character",{
  expect_is(eq_create_label(eq_china_cleandata), "character")
})

test_that("Epicenter plots work without error",{
  expect_is(eq_map(eq_china_cleandata, "DATE"), "htmlwidget")
})
