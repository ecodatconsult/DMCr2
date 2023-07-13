# fails with devtools::install() probably due to nested functions but works when run manually

test_that("runs and returns data.table", {
  out <- processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis")))
  expect_s3_class(out$daten, "data.table")
  expect_s3_class(out$ereignis, "data.table")
})


test_that("correct number of columns", {
  expect_equal(as.vector(unlist(lapply(processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis"))), ncol))), c(48,49))
})
