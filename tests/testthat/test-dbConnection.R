test_that("db connection works", {
  expect_s4_class(dbConnection("test"), "PostgreSQLConnection")
})
