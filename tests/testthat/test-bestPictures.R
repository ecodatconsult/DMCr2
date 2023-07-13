test_that("best Pictures successfully copied", {
  bestPictures(file_path = system.file("test_data", package = "DMCr2"))
  expect_true(dir.exists(system.file("test_data/bestPictures", package = "DMCr2")))
  expect_length(list.files(system.file("test_data/bestPictures", package = "DMCr2")), 6)
  filesstrings::dir.remove(system.file("test_data/bestPictures", package = "DMCr2"))
})
