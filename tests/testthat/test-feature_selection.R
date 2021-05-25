test_that("select_features", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # correct calls
  expect_length(select_features(df_feature = df_time_sorted, vec_label = label_sorted, filter = FALSE,
                                no_features = 2), 2)

  # false calls
  expect_error(select_features(df_feature = df_time_sorted, vec_label = label_sorted, filter = TRUE,
                              cutoff = 1.5, no_features = 2)) # false cutoff
  expect_error(select_features(df_feature = df_time_sorted, vec_label = label_sorted, filter = FALSE,
                               no_features = 40))
})


test_that("plot_selection_accuracy", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  selected_features <- select_features(df_feature = df_time_sorted, vec_label = label_sorted, filter = FALSE,
                                       no_features = 2)

  # correct calls
  expect_silent(plot_selection_accuracy(results = selected_features))

  # false calls
  expect_error(plot_selection_accuracy(results = "features"))
})
