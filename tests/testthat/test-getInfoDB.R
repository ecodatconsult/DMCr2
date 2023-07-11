test_that("22 Spaltennamen in fotofallen.fotofallen_standorte_import", {
  expect_length(getInfoDB(type = "column_name", schema = "fotofallen", table = "fotofallen_standorte_import"), 22)
})
