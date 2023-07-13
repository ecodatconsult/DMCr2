test_that("downloading ereignisse works", {
  dl <- downloadDB(data = "ereignisse", species = "Rothirsch", project = "Rotwildmanagement_BW")
  expect_equal(unique(dl$tierart_1), "Rothirsch")
})

test_that("downloading bilder works", {
  dl <- downloadDB(data = "bilder", species = "Rothirsch", project = "Rotwildmanagement_BW")
  expect_s3_class(dl, "data.frame")
})

test_that("downloading standorte works", {
  dl <- downloadDB(data = "standorte", species = "Rothirsch", project = "Rotwildmanagement_BW")
  expect_s3_class(dl, "sf")
})
