# check if a supplied hash matches
testthat::test_that("hash supplied matches", {
  expect_equal(check_md5_file(file_loc = 'resources/mdfiver_nonemtpy.txt',
                              md5_hash = '7aedffa4687d37d4007bbd8e7fcf000d'), T)
})
# check if hash file matches
testthat::test_that("hash file matches", {
  expect_equal(check_md5_file(file_loc = 'resources/mdfiver_nonemtpy.txt',
                              md5_file_loc =  'resources/mdfiver_nonemtpy.txt.md5'), T)
})
# check if inferred hash file matches
testthat::test_that("hash file matches", {
  expect_equal(check_md5_file(file_loc = 'resources/mdfiver_nonemtpy.txt'), T)
})
# check if wrong match fails
testthat::test_that("wrong hash supplied fails", {
  expect_equal(check_md5_file(file_loc = 'resources/mdfiver_nonemtpy.txt',
                              md5_hash = 'not_a_hash'), F)
})
# check if a supplied hash matches for sha256
testthat::test_that("hash supplied matches", {
  expect_equal(check_sha256_file(file_loc = 'resources/mdfiver_nonemtpy.txt',
                              sha256_hash = '5a800fc66fb4400c2388835f75a831a794b6428d75b5cc385683958e4b45914f'), T)
})
# check if hash file matches
testthat::test_that("hash file matches", {
  expect_equal(check_sha256_file(file_loc = 'resources/mdfiver_nonemtpy.txt',
                              sha256_file_loc =  'resources/mdfiver_nonemtpy.txt.sha256'), T)
})
# test errors that might be thrown
testthat::test_that("errors", {
  # check for non-existant file
  testthat::expect_error(
    check_md5_file(file_loc = 'resources/mdfiver_nonexistant.txt'),
    "file, resources/mdfiver_nonexistant.txt does not exist",
    fixed=TRUE
  )
  # check for empty file
  testthat::expect_error(
    check_md5_file(file_loc = 'resources/mdfiver_empty.txt'),
    "file, resources/mdfiver_empty.txt has size of zero",
    fixed=TRUE
  )
  # check for file with no md5
  testthat::expect_error(
    check_md5_file(file_loc = 'resources/mdfiver_no_md5.txt'),
    "no md5 supplied and md5 file location supplied or inferred (file_loc+'.md5') file location don't exist",
    fixed=TRUE
  )
})