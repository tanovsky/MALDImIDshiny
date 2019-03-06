# Only necessary by some bug
context("testing the complete execution")
library(testthat)
test_package("MALDImID")
# To be able to load the non public functions for testing
devtools::load_all()

condition <- "HSIL"
peptidesFile <- system.file("exampleData", "peptides_light.txt", package="MALDImID")
conditionFile <- system.file("exampleData", "members_HSIL_corrected.xlsx", package="MALDImID")

test_that("Gene Present at condition", {
  mzValues <- 1485.5864
  results <- GetResult("HSIL",
                       peptidesFile,
                       conditionFile,
                       mzValues,
                       tempfile())
  columnName <- paste0("genePresentAt", condition)
  expect_true(results[[columnName]])
})

test_that("Correctly detected mz values on bigger file (Within tolerance...)", {
  skip('\nReason: Just because it takes a bit long.\nRealistic file..\n')
  mzValues <- c(865.39, 987.57, 1905.95)
  peptidesFile <- system.file("exampleData", "peptides.txt", package="MALDImID")
  results <- GetResult("HSIL",
                       peptidesFile,
                       conditionFile,
                       mzValues,
                       tempfile())
  expectedMassValues <-   c(864.4341,
                            864.4593,
                            986.5397,
                            986.5185,
                            1904.9401,
                            1904.9125,
                            1904.8112)
  expect_equal(results[["Mass"]], expectedMassValues, tolerance=1e-7)
})
