context("arcgisapi")

token <- generate_token(Sys.getenv("ARCGIS_CLIENT_ID"),
                        Sys.getenv("ARCGIS_CLIENT_SECRET"),
                        1)

test_that("a token is generated", {
  expect_true(class(token) == "character")
  expect_true(nchar(token) > 100)
})

demographics <- get_drive_time_demographics(token, -86.157963, 39.768454)

test_that("demographic data is returned", {
  expect_true(sum(names(demographics) %in% c("features", "geometry")) == 2)
  expect_equal(demographics$features[["Area type"]], "NetworkServiceArea")
  expect_equal(demographics$features[["Buffer units"]], "Minutes")
  expect_equal(demographics$features[["Buffer radii"]], 10)
})
