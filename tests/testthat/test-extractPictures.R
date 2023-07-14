test_that("successfully extracted picture", {

  DMCr2::extractPictures(
    bilder_table = read.csv2(system.file("/test_data/FVA_Fotofallendatenbank_bilder_alle Arten_alle Projekte.csv", package = "DMCr2")),
    save_path = system.file("test_data", package = "DMCr2"),
    scale = TRUE,
    size = "x480",
    test_mode = TRUE)

  expect_true(file.exists(paste0(system.file("test_data", package = "DMCr2"), .Platform$file.sep, "RWGBW_10522_211110-I_00001a.JPG")))

  file.remove(paste0(system.file("test_data", package = "DMCr2"), .Platform$file.sep, "RWGBW_10522_211110-I_00001a.JPG"))

})
