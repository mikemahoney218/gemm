test_that("gemm_curves works", {
  filepath <- paste0(tempdir(), "/tmp.png")
  gemm_curves(-1, 1, seed = 125, polar = FALSE, save_args = filepath)
  expect_snapshot_file(filepath)
})
