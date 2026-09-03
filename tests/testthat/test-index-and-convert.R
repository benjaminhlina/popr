# Unit tests for .valid_idx(), .convert_spr_to_vec(), and error_spatrast()
#
# Assumes these tests live in tests/testthat/ of the package that defines
# these internal functions, so they're visible without `:::`.

test_that(".convert_spr_to_vec() converts a SpatRaster to a vector in column-major order", {
  skip_if_not_installed("terra")

  # 2 rows x 3 cols, values filled row-wise by terra::rast()
  r <- terra::rast(nrows = 2, ncols = 3, vals = 1:6)

  out <- .convert_spr_to_vec(r)

  expect_type(out, "integer")
  expect_length(out, 6)
  # terra::as.matrix(r, wide = TRUE) reshapes to the [row, col] grid,
  # then as.vector() reads that matrix column-major.
  expected <- as.vector(terra::as.matrix(r, wide = TRUE))
  expect_equal(out, expected)
})

test_that(".convert_spr_to_vec() preserves NAs", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, NA, 3, NA))

  out <- .convert_spr_to_vec(r)

  expect_equal(sum(is.na(out)), 2)
})

test_that(".convert_spr_to_vec() errors on non-SpatRaster input via error_spatrast()", {
  expect_error(
    .convert_spr_to_vec(matrix(1:4, nrow = 2)),
    regexp = "SpatRaster"
  )
  expect_error(
    .convert_spr_to_vec("not a raster"),
    regexp = "SpatRaster"
  )
})

test_that(".valid_idx() returns indices of non-NA values by default", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, NA, 3, NA))

  idx <- .valid_idx(r)

  expect_equal(idx, which(!is.na(.convert_spr_to_vec(r))))
})

test_that(".valid_idx(na = TRUE) returns indices of NA values", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, NA, 3, NA))

  idx_na <- .valid_idx(r, na = TRUE)

  expect_equal(idx_na, which(is.na(.convert_spr_to_vec(r))))
  expect_length(idx_na, 2)
})

test_that(".valid_idx() returns all indices when there are no NAs", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 3, ncols = 3, vals = 1:9)

  expect_equal(.valid_idx(r), 1:9)
  expect_equal(.valid_idx(r, na = TRUE), integer(0))
})

test_that(".valid_idx() propagates error_spatrast() for invalid input", {
  expect_error(
    .valid_idx(list(1, 2, 3)),
    regexp = "SpatRaster"
  )
})

test_that("error_spatrast() uses the correct arg_name when not supplied", {
  # Regression test: error_spatrast() previously did
  # rlang::enexpr(df) instead of rlang::enexpr(spatrast), so calling it
  # without an explicit arg_name errored with "object 'df' not found"
  # rather than naming the actual argument passed in.
  my_input <- matrix(1:4, nrow = 2)

  err <- tryCatch(
    error_spatrast(my_input),
    error = function(e) e
  )

  expect_no_match(conditionMessage(err), "object 'df' not found")
  expect_match(conditionMessage(err), "my_input")
})

test_that("error_spatrast() passes for actual SpatRaster input", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)

  expect_no_error(error_spatrast(r))
})

test_that("error_spatrast() respects an explicit arg_name override", {
  expect_error(
    error_spatrast(matrix(1:4, nrow = 2), arg_name = "custom_name"),
    regexp = "custom_name"
  )
})
