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


test_that("add_time", {
  expect_equal(add_time(1, "00:00"), "00:01")
  expect_equal(add_time(1, "00:01"), "00:11")
  expect_equal(add_time(1, "01:10"), "11:01")
  expect_equal(add_time(0, "01:10"), "11:00")
  expect_equal(add_time(1, "11:11"), "11:11")
})


test_that("calc_difftime", {
  t1 <- lubridate::ymd_hms("2022-01-01 12:34:56")
  t2 <- lubridate::ymd_hms("2022-01-01 12:34:59")
  expect_equal(calc_diff_time(t1, t2), -3)
  expect_equal(calc_diff_time(t2, t1), 3)
})
