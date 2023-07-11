# fails with devtools::install() probably due to nested functions but works when run manually

test_that("daten read as data.table", {
  expect_s3_class(processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis")))$daten, "data.table")
  expect_s3_class(processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis")))$ereignis, "data.table")
})


test_that("correct number of columns", {
  expect_equal(as.vector(unlist(lapply(processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis"))), ncol))), c(48,49))
})
