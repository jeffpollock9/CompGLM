if (requireNamespace("lintr", quietly = TRUE)) {

    context("code style")
    test_that("check that package has google style", {
        lints <- lintr::lint_package()
        if (length(lints) > 0) {
            print(Reduce(rbind, lapply(lints, function(x) {
                data.frame(file = x$filename, line_number = x$line_number, msg = x$message)
            })))
        }
        expect_true(length(lints) == 0)
    })
}
