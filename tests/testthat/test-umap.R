test_that("plot_UMAP", {
  subset_index <- seq(1, nrow(whitestork_acc), by = 5)

  df_time_sorted <- calculate_feature_time(order_acc(whitestork_acc)[subset_index,], winlen_dba = 10, axis_num = 3)
  df_freq_sorted <- calculate_feature_freq(order_acc(whitestork_acc)[subset_index,], samp_freq = 10.54, axis_num = 3)
  label_sorted <- order_acc(whitestork_acc)[subset_index, 121]

  # false calls
  expect_error(plot_UMAP()) # false input
  expect_error(plot_UMAP(df_time = df_time_sorted,
                         df_freq = rbind(df_freq, df_freq), label_vec = label_sorted)) # input not match
  expect_error(plot_UMAP(df_time = df_time_sorted,
                         df_freq = df_freq, label_vec = c(label_sorted, label_sorted))) # input not match
})
