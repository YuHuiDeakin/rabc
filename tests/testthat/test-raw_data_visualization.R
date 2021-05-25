
test_that("order_acc", {
  # correct calls
  expect_type(order_acc(whitestork_acc), "list")
  expect_equal(order_acc(whitestork_acc)[, ncol(whitestork_acc)],
               sort(whitestork_acc[, ncol(whitestork_acc)]))

  # false calls
  expect_error(order_acc()) # no input
  expect_error(order_acc(as.matrix(whitestork_acc))) # false input type
  expect_error(order_acc(whitestork_acc[, ncol(whitestork_acc):1])) # false data format
})


test_that("plot_acc",{
  # correct calls
  expect_type(plot_acc(whitestork_acc), "list")

  # false calls
  expect_error(plot_acc(whitestork_acc[, -120], axis_num = 3)) # false data format
  expect_error(plot_acc(whitestork_acc[, -120], axis_num = 2)) # false data format
})


test_that("plot_feature", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_unsorted <- calculate_feature_time(whitestork_acc[subset_index, ], winlen_dba = 10, axis_num = 3)
  label_unsorted <- whitestork_acc[subset_index, 121]

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # correct calls
  expect_type(plot_feature(df_feature = df_time_sorted, vec_label = label_sorted), "list")

  # false calls
  expect_error(plot_feature(df_feature = df_time_sorted, vec_label = whitestork_acc$V121)) # data and label unmatch

  # warning calls
  expect_warning(plot_feature(df_feature = df_time_unsorted, vec_label = label_unsorted)) # data unsorted
})


test_that("plot_grouped_feature", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_unsorted <- calculate_feature_time(whitestork_acc[subset_index, ], winlen_dba = 10, axis_num = 3)
  label_unsorted <- whitestork_acc[subset_index, 121]

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # false calls
  expect_error(plot_grouped_feature(df_feature = df_time_sorted, vec_label = whitestork_acc$V121,
                                    geom = "boxplot")) # data and label unmatch
  expect_error(plot_grouped_feature(df_feature = df_time_sorted, vec_label = label_sorted,
                                    geom = "circle")) # false geom

  # warning calls
  expect_warning(plot_grouped_feature(df_feature = df_time_unsorted, vec_label = label_unsorted)) # data unsorted
})
