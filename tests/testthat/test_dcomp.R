context("dcomp")

test_that("dcomp matches dpois with nu = 1", {    
    expect_equal(dpois(-5:5, 2.5, FALSE), dcomp(-5:5, 2.5, 1, 100L, FALSE))
    expect_equal(dpois(1:2, 2:5, FALSE), dcomp(1:2, 2:5, 1, 100L, FALSE))
    expect_equal(dpois(2:5, 1:2, FALSE), dcomp(2:5, 1:2, 1, 100L, FALSE))

    expect_equal(dpois(-5:5, 2.5, TRUE), dcomp(-5:5, 2.5, 1, 100L, TRUE))
    expect_equal(dpois(1:2, 2:5, TRUE), dcomp(1:2, 2:5, 1, 100L, TRUE))
    expect_equal(dpois(2:5, 1:2, TRUE), dcomp(2:5, 1:2, 1, 100L, TRUE))
})

test_that("dcomp throws", {
    expect_error(dcomp(1, -1, 2), silent = TRUE)
    expect_error(dcomp(1, 1, -2), silent = TRUE)
})
