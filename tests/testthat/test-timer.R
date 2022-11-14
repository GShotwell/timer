test_that("time display conversion", {
  expect_equal(seconds_to_display(0), "00:00")
  expect_equal(seconds_to_display(59), "00:59")
  expect_equal(seconds_to_display(60), "01:00")
  expect_equal(seconds_to_display(61), "01:01")
  expect_equal(seconds_to_display(500), "08:20")

  expect_equal(display_to_seconds("00:00"), 0)
  expect_equal(display_to_seconds("00:59"), 59)
  expect_equal(display_to_seconds("01:01"), 61)
  expect_equal(display_to_seconds("10:00"), 600)
  expect_equal(display_to_seconds("99:99"), 6039)
  })
