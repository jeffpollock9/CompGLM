context("glm.comp")

test_that("glm.comp is simular to glm", {
    set.seed(1)
    n <- 5000
    x1 <- rnorm(n, -1.0, 0.5)
    x2 <- rnorm(n, 1.0, 0.7)
    x3 <- rnorm(n, 2.0, 0.4)
    y <- rpois(n, exp(-0.5 + 0.3 * x1 + 0.8 * x2 + 0.2 * x3))

    data <- data.frame(y, x1, x2, x3)

    poissonModel <- glm(y ~ ., poisson, data)
    compModel <- glm.comp(y ~ ., data = data)

    expect_equal(coef(poissonModel), coef(compModel)$beta, tol = 0.1)
    expect_equal(0.0, unname(coef(compModel)$zeta), tol = 0.1)
})
