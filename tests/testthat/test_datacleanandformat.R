context("Data Clean and Format")

cldata <- eq_clean_data(eq_test_data)
clldata <- eq_location_clean(cldata)

test_that("DATE field is of the date class", {
  expect_is(cldata$DATE, "Date")
})

test_that("DATE field is made up of year month and day", {

  expect_false( any(is.na(lubridate::year(cldata$DATE))))
  expect_false( any(is.na(lubridate::month(cldata$DATE))))
  expect_false( any(is.na(lubridate::day(cldata$DATE))))
})

test_that("LONGITUDE and LATITUDE fields are numeric", {
  expect_is(cldata$LONGITUDE, "numeric")
  expect_is(cldata$LATITUDE, "numeric")
})

test_that(" LOCATION_NAME does not include a colon", {
  expect_false(any(grepl(":", clldata)))
})
