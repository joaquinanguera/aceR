context("package information")

expect_that(aceR_description(), is_a("packageDescription"))
expect_that(aceR_version(), is_a("character"))