test_that("daten read as data.table", {
  expect_s3_class(processImportTables(importTables("inst/test_data/"))$daten, "data.table")
})

test_that("ereignis read as data.table", {
  expect_s3_class(processImportTables(importTables("inst/test_data/"))$ereignis, "data.table")
})

test_that("correct number of columns", {
  expect_equal(as.vector(unlist(lapply(processImportTables(importTables("inst/test_data/")), ncol))), c(48,49))
})

