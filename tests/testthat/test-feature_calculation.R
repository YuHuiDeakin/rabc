test_that("calculate_feature_time",{
  # correct calls
  expect_length(calculate_feature_time(whitestork_acc[1:50, ], winlen_dba = 10, axis_num = 3), 19)
  expect_length(calculate_feature_time(whitestork_acc[1:50, ], winlen_dba = 10, axis_num = 2), 13)
  expect_length(calculate_feature_time(whitestork_acc[1:50, ], winlen_dba = 10, axis_num = 1), 7)

  # false calls
  expect_error(calculate_feature_time()) # lack argument
  expect_error(calculate_feature_time(whitestork_acc)) # lack argument
  expect_error(calculate_feature_time(whitestork_acc, winlen_dba = "1")) # false argument type
  expect_error(calculate_feature_time(whitestork_acc[, -120], winlen_dba = 10, axis_num = 3)) # false data format
  expect_error(calculate_feature_time(whitestork_acc[, -120], winlen_dba = 10, axis_num = 2)) # false data format

  # warning call
  expect_warning(calculate_feature_time(whitestork_acc, winlen_dba = 10, axis_num = 3)) # data not sorted by labels
})


test_that("calculate_feature_freq", {
  # correct calls
  expect_length(calculate_feature_freq(whitestork_acc[1:50, ], samp_freq = 10.54, axis_num = 3), 9)
  expect_length(calculate_feature_freq(whitestork_acc[1:50, ], samp_freq = 10.54, axis_num = 2), 6)
  expect_length(calculate_feature_freq(whitestork_acc[1:50, ], samp_freq = 10.54, axis_num = 1), 3)

  # false calls
  expect_error(calculate_feature_freq()) # lack argument
  expect_error(calculate_feature_freq(whitestork_acc)) # lack argument
  expect_error(calculate_feature_freq(whitestork_acc, samp_freq = "good")) # false argument type
  expect_error(calculate_feature_freq(whitestork_acc[, -120], samp_freq = 10.54, axis_num = 3)) # false data format
  expect_error(calculate_feature_freq(whitestork_acc[, -120], samp_freq = 10.54, axis_num = 2)) # false data format

  # warning call
  expect_warning(calculate_feature_freq(whitestork_acc, samp_freq = 10.54, axis_num = 3)) # data not sorted by labels
})
