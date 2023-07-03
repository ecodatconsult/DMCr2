test_that("list of length 2", {
  expect_length(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis")), 2)
})

test_that("empty lists with wrong directory", {
  expect_null(unlist(importTables(file_path = "data/", data = c("daten", "ereignis"))))
})
