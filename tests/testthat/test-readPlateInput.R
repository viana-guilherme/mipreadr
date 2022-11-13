test_that("x3 import works", {

  # loads the test files
  x3input <- readPlateInput("testfiles/x3_input.txt", mode = "x3")

  expect_s3_class(x3input, "data.frame")
  expect_equal(nrow(x3input), 1632) # tests for number of rows
  expect_equal(ncol(x3input), 8) # tests for number of columns
  expect_type(x3input$Well, "character") # tests specific types for columns
  expect_type(x3input$`GFP (Counts)`, "double") # tests specific types for columns

})

# to do: nivo import works
