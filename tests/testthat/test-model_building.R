test_that("select_features", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # correct calls
  expect_type(train_model(df = df_time_sorted, vec_label = label_sorted, train_ratio = 0.7), "list")

  # false calls
  expect_error(train_model(df = df_time_sorted, vec_label = label_sorted, train_ratio = 1.2)) # false train_ration
  expect_error(train_model(df = df_time_sorted, vec_label = label_sorted, hyper_choice = "hyper")) # false hyper_choice
})


test_that("plot_confusion_matrix", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # correct calls
  expect_type(plot_confusion_matrix(df = df_time_sorted, vec_label = label_sorted), "list")

  # false calls
  expect_error(plot_confusion_matrix(df = df_time_sorted)) # lack input
})


test_that("plot_wrong_classifications", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  predictions <- plot_confusion_matrix(df = df_time_sorted, vec_label = label_sorted)

  # correct calls
  expect_type(plot_wrong_classifications(df_raw = order_acc(whitestork_acc)[subset_index,],
                                         df_result = predictions), "list")

  # false calls
  expect_error(plot_wrong_classifications(df_raw = order_acc(whitestork_acc)[subset_index,],
                                          df_result = rbind(predictions, predictions))) # false input
})
