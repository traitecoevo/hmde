#Script to test the pre-loaded data.
test_that("Datasets: existence", {
  expect_visible(Lizard_Mass_Data)
  expect_visible(Lizard_Metabolic_Data)
  expect_visible(Trout_Size_Data)
  expect_visible(Tree_Size_Data)
})

test_that("Datasets: size", {
  expect_equal(object = c(nrow(Lizard_Mass_Data),
                          ncol(Lizard_Mass_Data)),
               expected = c(336, 4))
  expect_equal(object = c(nrow(Lizard_Metabolic_Data),
                          ncol(Lizard_Metabolic_Data)),
               expected = c(443, 4))
  expect_equal(object = c(nrow(Trout_Size_Data),
                          ncol(Trout_Size_Data)),
               expected = c(135, 4))
  expect_equal(object = c(nrow(Tree_Size_Data),
                          ncol(Tree_Size_Data)),
               expected = c(300, 4))
})
