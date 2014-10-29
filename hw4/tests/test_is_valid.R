context("Test is_valid")

test_that("valid graphs", {
  g1 = list(list(edges   = c(1L, 2L),
                 weights = c(1, 1)),
            list(edges   = c(1L, 2L),
                 weights = c(1, 1)))
  
  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))
  
  g3 = list(list(edges   = c(2L),
                 weights = c(1)),
            list(edges   = c(1L),
                 weights = c(1)))
  
  g4 = list(list(edges   = integer(),
                 weights = numeric()))
  
  expect_true(is_valid(g1))
  expect_true(is_valid(g2))
  expect_true(is_valid(g3))
  expect_true(is_valid(g4))
})


test_that("Bad structure", {
  bad_g1_1 = list(list())
  bad_g1_2 = list(list(edges = 1L))
  bad_g1_3 = list(list(weights = 1))
  
  expect_false(is_valid(bad_g1_1))
  expect_false(is_valid(bad_g1_2))
  expect_false(is_valid(bad_g1_3))
})


test_that("Invalid vertex reference", {
  bad_g2_1 = list(list(edges   = c(1L, 2L),
                       weights = c(1, 1)))
  bad_g2_2 = list(list(edges   = c(1L, 2L),
                       weights = c(1, 1)),
                  list(edges   = c(1L, 3L),
                       weights = c(1, 1)))
  
  expect_false(is_valid(bad_g2_1))
  expect_false(is_valid(bad_g2_2))
})


test_that("Duplicate vertex labels", {
  bad_g3 = list(A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)),
                A = list(edges   = c(1L, 2L),
                         weights = c(1, 1)))
  
  expect_false(is_valid(bad_g3))
})


test_that("Edge type", {
  bad_g4_1 = list(list(edges   = c(1),
                       weights = c(1)))  
  bad_g4_2 = list(list(edges   = c("A"),
                       weights = c(1)))  
  bad_g4_3 = list(list(edges   = c(NA+1L),
                       weights = c(1)))  
  
  expect_false(is_valid(bad_g4_1))
  expect_false(is_valid(bad_g4_2))
  expect_false(is_valid(bad_g4_3))
})


test_that("Weight type and value", {
  bad_g5_1 = list(list(edges   = c(1L),
                       weights = c(-1)))
  bad_g5_2 = list(list(edges   = c(1L),
                       weights = c(0)))
  bad_g5_3 = list(list(edges   = c(1L),
                       weights = c(NA+1)))
  
  expect_false(is_valid(bad_g5_1))
  expect_false(is_valid(bad_g5_2))
  expect_false(is_valid(bad_g5_3))
})


test_that("Duplicated edges", {
  bad_g6_1 = list(list(edges   = c(1L, 1L),
                       weights = c(1, 1)))
  
  expect_false(is_valid(bad_g6_1))
})


test_that("Edge and weight length mismatch", {
  bad_g7_1 = list(list(edges   = c(1L),
                       weights = c(1, 1)))
  bad_g7_2 = list(list(edges   = c(1L, 2L),
                       weights = c(1)))
  bad_g7_3 = list(list(edges   = c(1L, 2L),
                       weights = c(1, 1)),
                  list(edges   = c(1L, 2L),
                       weights = c(1, 1, 1)))
  bad_g7_4 = list(list(edges   = c(1L, 2L),
                       weights = c(1, 1)),
                  list(edges   = c(1L, 2L, 3L),
                       weights = c(1, 1)))
  
  expect_false(is_valid(bad_g7_1))
  expect_false(is_valid(bad_g7_2))
  expect_false(is_valid(bad_g7_3))
  expect_false(is_valid(bad_g7_4))
})