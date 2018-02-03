context("pcomp")

test_that("pcomp matches ppois with nu = 1", {
    expect_equals(ppois(-5:5, 2.5, TRUE, FALSE), pcomp(-5:5, 2.5, 1, 100L, TRUE, FALSE))
    expect_equals(ppois(1:2, 2:5, TRUE, FALSE), pcomp(1:2, 2:5, 1, 100L, TRUE, FALSE))
    expect_equals(ppois(2:5, 1:2, TRUE, FALSE), pcomp(2:5, 1:2, 1, 100L, TRUE, FALSE))

    expect_equals(ppois(-5:5, 2.5, FALSE, TRUE), pcomp(-5:5, 2.5, 1, 100L, FALSE, TRUE))
    expect_equals(ppois(1:2, 2:5, FALSE, FALSE), pcomp(1:2, 2:5, 1, 100L, FALSE, FALSE))
    expect_equals(ppois(1:2, 2:5, TRUE, TRUE), pcomp(1:2, 2:5, 1, 100L, TRUE, TRUE))
}

test_that("pcomp throws", {
    expect_error(pcomp(1, -1, 2), silent = TRUE)
    expect_error(pcomp(1, 1, -2), silent = TRUE)
}
