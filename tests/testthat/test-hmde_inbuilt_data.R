#Script to test the pre-loaded data.
test_that("Datasets: existence", {
  expect_visible(Lizard_Size_Data)
  expect_visible(Trout_Size_Data)
  expect_visible(Tree_Size_Data)
  expect_visible(Tree_Size_Ests)
})

test_that("Datasets: size", {
  expect_equal(object = c(nrow(Lizard_Size_Data),
                          ncol(Lizard_Size_Data)),
               expected = c(328, 4))
  expect_equal(object = c(nrow(Trout_Size_Data),
                          ncol(Trout_Size_Data)),
               expected = c(135, 4))
  expect_equal(object = c(nrow(Tree_Size_Data),
                          ncol(Tree_Size_Data)),
               expected = c(300, 4))
  expect_equal(object = length(Tree_Size_Ests),
               expected = 4)
})
