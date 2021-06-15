

test_that("mandatory parameters are checked", {
  expect_error(validate_parameters ( param = NULL, param_name = "test_param"))
})

test_that("assertions for closed vocabulary parameters", {
  expect_error(validate_paramters( typology = "wrong_typology"))
})

