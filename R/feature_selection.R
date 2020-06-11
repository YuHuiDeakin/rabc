#'Feature selection
#'
#'Select a subset of relevant features for use in behaviour classification.
#'
#'@param df_feature A data.frame or tibble calculated by \code{calculate_feature_time} or
#'  \code{calculate_feature_freq}.
#'@param vec_label A character vector that contains all behaviour type labels.
#'@param filter A logical value indicating whether to include filter function in
#'  feature selection. It has default value "FALSE".
#'@param cutoff A double value setting threshold for correlation coefficient with
#'  default value 0.9.
#'@param wrapper Character. The machine learning method for feature selection. In
#'  this version, it's set to "XGBoost" and only accept this method.
#'@param no_features An integer value indicating how many features to be selected.
#'
#'@return A list. List[[1]] contains a matrix providing the classification accuracy
#'  for each of the features across all steps. Once a feature is selected into the
#'  selected feature set, the remaining values in this feature's column are set to
#'  zero. List[[2]] contains a character recording the names of the selected features
#'  in the order in which they were selected in the SFS process.
#'
#'@details This is a combination of a filter and a wrapper feature selection method
#' for feature selection. The filter part has the purpose of removing redundant features.
#' The filtering method uses the absolute values of pair-wise correlation coefficients
#' between features. If two variables have a high correlation, the function looks at
#' the mean absolute correlation of each variable and removes the variable with the
#' largest mean absolute correlation. The purpose of the wrapper is to select most
#' relevant features. The wrapper part uses stepwise forward selection (SFS) using extreme
#' boosting machine (XGBoost) method. The number of features to select (no_features)
#' can be set by the user, having a default of "no_features = 5". This parameter also
#' determines how many rounds of SFS are being conducted. In the first round, each
#' feature is individually used to train a classification model by XGBoost. The
#' feature with highest overall accuracy will be kept into the selected feature set.
#' Then, in every following round, each remaining feature will be combined with the
#' selected feature set to train a classification model and the one with the highest
#' accuracy will be kept into the selected feature set. The process will stop when
#' the number of rounds equals the no_features setting.
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_time <- calculate_feature_time(df_raw = whitestork_acc_sorted, winlen_dba = 11)
#'results <- select_features(df_feature = df_time[,1:6],
#'                          vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)],
#'                          no_features = 3)
select_features <- function(df_feature = NULL, vec_label = NULL,
                           filter = FALSE, cutoff = 0.9, wrapper = "XGBoost",
                           no_features = 5
                           ) {
  if (is.null(df_feature)) {
    stop("Please provide a valid feature data.frame!")
  }
  if (is.null(vec_label)) {
    stop("Please provide a valid label vector!")
  }

  if (filter == T) {
    high_cor <- caret::findCorrelation(cor(df_feature), cutoff = cutoff)
    print(high_cor)
    df_feature <- df_feature[, !colnames(df_feature) %in% colnames(df_feature)[high_cor], drop = F]
  }

  result_mat <- matrix(0, ncol = length(colnames(df_feature)),
                       nrow = no_features,
                       dimnames = list(c(1:no_features), colnames(df_feature)))
  name_temp <- vector(mode = "character", length = 0L)

  Y_train <- as.factor(vec_label)
  grid_default <- expand.grid(
    nrounds = 5,
    max_depth = 6,
    eta = 0.2,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  xgb_trcontrol <- caret::trainControl(
    method = "cv",
    number = 10,
    allowParallel = T,
    verboseIter = F,
    returnData = F,
    summaryFunction = caret::multiClassSummary
  )

  for (i in 1:no_features) {
    feature_pool <- colnames(df_feature)[!colnames(df_feature) %in% name_temp]
    #print(feature_pool)
    for (name in feature_pool) {
      set.seed(123)
      X_train <- as.matrix(df_feature[, c(name_temp, name), drop = F])
      #print(head(X_train))
      xgb_model <- caret::train(X_train, Y_train,
                         trControl = xgb_trcontrol,
                         tuneGrid = grid_default,
                         method = "xgbTree")
      result_mat[i, name] <- unclass(xgb_model$results["Accuracy"])$Accuracy
    }
    index <- which(result_mat[i, ] == max(result_mat[i, ]))[1]

    name_temp <- c(name_temp, colnames(result_mat)[index])
  }

  result_mat <- cbind(result_mat[,name_temp], result_mat[,!(colnames(result_mat) %in% name_temp)])
  rownames(result_mat) <- paste0("round", 1:no_features)
  return(list(result_mat = result_mat, features = name_temp))
}



#'Feature selection accuracy plot
#'
#'Plot accuracies of selected features.
#'
#'@param results A list. It should be the returned list of function \code{select_features}.
#'
#'@details Red line and dots in the plot denotes classification accuracies of accumulated
#'  selected features. Grey bars indicate accuracy increase (labeled with numbers) of each
#'  selected feature.
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_time <- calculate_feature_time(df_raw = whitestork_acc_sorted, winlen_dba = 11)
#'results <- select_features(df_feature = df_time[,1:6],
#'                          vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)],
#'                          no_features = 3)
#'plot_selection_accuracy(results = results)
plot_selection_accuracy <- function(results = results) {
  accuracy <- vector(mode = "double", length = length(results$features))
  for (i in 1:length(results$features)) {
    accuracy[i] <- results$result_mat[i, results$features[i]]
  }
  feature <- 1:length(results$features)
  df <- data.frame(feature, accuracy)
  df$accuracy_diff <- df$accuracy
  df$accuracy_diff[-1] <- diff(df$accuracy)
  ggplot2::ggplot(df, ggplot2::aes(x = feature, y = accuracy)) +
    ggplot2::geom_point(shape = 1, size = 2, color = "red") +
    ggplot2::geom_line(color = "red") +
    ggplot2::geom_col(ggplot2::aes(x = feature, y = accuracy_diff), alpha = 0.5) +
    ggplot2::scale_x_continuous(breaks = 1:length(results$features),
                     labels = results$features) +
    ggplot2::xlab("\nSelected features") +
    ggplot2::ylab("Accuracy\n") +
    ggplot2::labs(title = "Classification accuracy") +
    ggplot2::ylim(-0.05,1) +
    ggplot2::geom_text(ggplot2::aes(x = feature, y = accuracy_diff + 0.03,
                           label = ifelse(accuracy_diff > 0.001,
                                          sprintf("%0.3f", round(accuracy_diff, digits = 3)),
                                          sprintf("%0.1e", accuracy_diff)))) +
    ggplot2::theme(axis.text = element_text(size = 10),
                   axis.title = element_text(size = 12, face = "bold"),
                   plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
}
