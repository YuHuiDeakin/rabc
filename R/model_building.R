#' Model training and validation
#'
#' Function for machine learning model training and validation. Usually, the training
#' and validation of supervised machine learning models includes three steps:
#' (1) machine learning model hyperparameter tuning by cross-validation; (2) model
#' training with the optimal hyperparameter set, and (3) evaluating model performance
#' with a test dataset. \code{train_model} is a wrapper function that utilizes relavant
#' functions from the "caret" package to automatically conduct the three steps for
#' model training and validation.
#'
#'@param df A data.frame or tibble contains features for classification.
#'@param vec_label A character vector that contains all behaviour type labels.
#'@param hyper_choice A character value. The default value is set to "defaults",
#'  which will let the XGBoost model use a default hyperparameter set without further
#'  hyperparameter tuning. With value "tune", the model will do hyperparameter tuning
#'  with a predefined hyperparameter group. See details below.
#'@param train_ratio A double value determins the percentage of data used to train
#'  the model, the remainder of the data being used for model validation.
#'
#'@return An XGBoost classifier for new data prediction.
#'
#'@details When the argument hyper_choice is set to "defaults", the arguments for
#'  XGBoost model are set as: nrounds = 10, max_depth = 6, eta = 0.3, gamma = 0,
#'  colsample_bytree = 1, min_child_weight = 1, subsample = 1. Value "tune" will
#'  let the function run hyperparameter tuning with the choices: nrounds = c(5, 10,
#'  50, 100), max_depth = c(2, 3, 4, 5, 6), eta = c(0.01, 0.1, 0.2, 0.3), gamma =
#'  c(0, 0.1, 0.5), colsample_bytree = 1, min_child_weight = 1, subsample = 1.
#'
#'  The returned XGBoost classifier used all data in argument df without partition.
#'  When hyper_choice = "defaults", this returned classifier will use the default
#'  hyperparameters. When hyper_choice = "tune", this returned classifier will use
#'  best tuned hyperparameters.
#'@examples
#'final_model <- train_model(df_time, vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)])
train_model <- function(df = NULL, vec_label = NULL, hyper_choice = "defaults",
                        train_ratio = 0.75) {
  if (is.null(df)) {
    stop("Please provide a valid feature data.frame!")
  }
  if (is.null(vec_label)) {
    stop("Please provide a valid label vector!")
  }

  vec_label <- as.factor(vec_label)
  set.seed(12321)
  train_index <- caret::createDataPartition(vec_label, p = train_ratio,
                                     list = FALSE)
  train_data <- as.matrix(df[train_index, ])
  train_label <- vec_label[train_index]
  test_data <- as.matrix(df[-train_index, ])
  test_label <- vec_label[-train_index]

  if (hyper_choice == "defaults") {
    grid_default <- expand.grid(
      nrounds = 10,
      max_depth = 6,
      eta = 0.3,
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    xgb_trcontrol <- caret::trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 3,
      allowParallel = T,
      verboseIter = F,
      returnData = F,
      summaryFunction = caret::multiClassSummary
    )
    xgb_model <- caret::train(train_data, train_label,
                       trControl = xgb_trcontrol,
                       tuneGrid = grid_default,
                       method = "xgbTree")

    predictions <- predict(xgb_model, test_data)
    confusion <- caret::confusionMatrix(predictions, test_label)
    print(confusion)
    print(caret::varImp(xgb_model))
    xgb_return <- caret::train(as.matrix(df), vec_label,
                               trControl = xgb_trcontrol,
                               tuneGrid = grid_default,
                               method = "xgbTree")
    return(xgb_return)
  } else if (hyper_choice == "tune") {
    grids <- expand.grid(
      nrounds = c(5, 10, 50, 100),
      max_depth = c(2, 3, 4, 5, 6),
      eta = c(0.01, 0.1, 0.2, 0.3),
      gamma = c(0, 0.1, 0.5),
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    xgb_trcontrol <- caret::trainControl(
      method = "cv",
      number = 5,
      allowParallel = T,
      verboseIter = F,
      returnData = F,
      summaryFunction = caret::multiClassSummary
    )
    xgb_model <- caret::train(train_data, train_label,
                       trControl = xgb_trcontrol,
                       tuneGrid = grids,
                       method = "xgbTree")

    predictions <- predict(xgb_model, test_data)
    confusion <- caret::confusionMatrix(predictions, test_label)
    print(confusion)
    print(xgb_model$bestTune)
    print(caret::varImp(xgb_model))
    grids_full <- expand.grid(
      nrounds = xgb_model$bestTune$nrounds,
      max_depth = xgb_model$bestTune$max_depth,
      eta = xgb_model$bestTune$eta,
      gamma = xgb_model$bestTune$gamma,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    xgb_return <- caret::train(as.matrix(df), vec_label,
                               trControl = xgb_trcontrol,
                               tuneGrid = grids_full,
                               method = "xgbTree")
    return(xgb_return)
  }

}



#'Classification result check
#'
#'Plot classification result confusion table and return classification result
#'data.frame.
#'
#'@param df A data.frame or tibble contains the feature set for classification
#'@param vec_label A character vector that contains all behaviour type labels.
#'
#'@details In this function, input dataset will be randomly partitioned into 5
#'folds without replacement according to levels of behaviour labels (see \code{\link{createFolds}}
#'for more details). Each fold of the 5 folds will be used as a test set while the
#'rest data used as the training set. The classification model used in this function
#'is XGBoost. Parameters of the XGBoost model are set like the following:
#'n_rounds = 10, max_depth = 6, eta = 0.3, gamma = 0, colsample_bytree = 1,
#'min_child_weight = 1, subsample = 1. After each training and testing round of the
#'5 rounds, all original behaviour labels (observation) and predicted behaviours will be recorded.
#'In the confusion table plot, x axis "obs" denotes observations and y axis "pre"
#'denotes predictions. Dots are colored according to classification results. The
#'annotated numbers are number of cases in each observation and prediction group.
#'
#'@return A data.frame with 3 columns. Column 1 "obs": factor of observations.
#'Column 2 "pre": factor of predictions. Column 3 "id": sequence of observations same
#'as the input "vec_label".
#'
#'@examples
#'pred <- plot_confusion_matrix(df = df_time, vec_label = label_vec)
#'head(pred)
plot_confusion_matrix <- function(df = NULL, vec_label = NULL) {
  if (is.null(df)) {
    stop("Please provide a valid feature data.frame!")
  }
  if (is.null(vec_label)) {
    stop("Please provide a valid label vector!")
  }

  vec_label <- as.factor(vec_label)

  No_folds = 5
  set.seed(11)
  cv_splits <- caret::createFolds(vec_label, k = No_folds)

  grid_default <- expand.grid(
    nrounds = 10,
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  xgb_trcontrol <- caret::trainControl(
    method = "none",
    allowParallel = T,
    verboseIter = F,
    returnData = F,
    summaryFunction = caret::multiClassSummary
  )

  for (i in 1:No_folds) {
    train_data <- as.matrix(df[-cv_splits[[i]], ])
    train_label <- vec_label[-cv_splits[[i]]]
    test_data <- as.matrix(df[cv_splits[[i]], ])
    test_label <- vec_label[cv_splits[[i]]]

    xgb_model <- caret::train(train_data, train_label,
                       trControl = xgb_trcontrol,
                       tuneGrid = grid_default,
                       method = "xgbTree")

    predictions <- predict(xgb_model, test_data)

    if (i == 1) {
      result_frame <- data.frame(obs = test_label, pre = predictions,
                                 id = cv_splits[[i]])
    } else {
      result_frame <- dplyr::bind_rows(result_frame,
                                data.frame(obs = test_label, pre = predictions,
                                           id = cv_splits[[i]]))
    }
  }
  result_frame <- dplyr::arrange(result_frame, id)
  con_frame <- result_frame
  con_frame$flag <- "incorrect"
  con_frame$flag[con_frame$obs == con_frame$pre] <- "correct"
  con_frame$flag <- as.factor(con_frame$flag)
  tf <- con_frame %>%
    dplyr::group_by(obs, pre) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::complete(obs, pre, fill = list(N = 0))
  con_frame <- dplyr::left_join(con_frame, tf, by = c("obs", "pre"))
  tempdf <- con_frame %>%
    dplyr::group_by(obs, pre) %>%
    dplyr::summarise(N = dplyr::n())
  diagvalue <- tempdf$N[which(tempdf$obs == tempdf$pre)]
  recallvalue <- (tempdf %>% dplyr::group_by(obs) %>% dplyr::summarise(total = sum(N)))$total
  precisionvalue <- (tempdf %>% dplyr::group_by(pre) %>% dplyr::summarise(total = sum(N)))$total
  print(ggplot2::ggplot(con_frame, ggplot2::aes(x = as.numeric(obs), y = as.numeric(pre),
                                       color = flag, label = N)) +
          ggplot2::geom_jitter(width = 0.2, height = 0.2, size = 0.7) +
          ggplot2::geom_text(nudge_x = 0.3, nudge_y = -0.3, show.legend = FALSE) +
          ggplot2::scale_x_continuous(name = "Observations",
                                      breaks = 1:length(levels(con_frame$obs)),
                                      labels = unique(as.character(con_frame$obs)),
                                      sec.axis = ggplot2::dup_axis(labels = round(diagvalue*100/recallvalue,2),
                                                          name = "Recall rate (%)")) +
          ggplot2::scale_y_continuous(name = "Predictions",
                                      breaks = 1:length(levels(con_frame$pre)),
                                      labels = unique(as.character(con_frame$obs)),
                                      sec.axis = ggplot2::dup_axis(labels = round(diagvalue*100/precisionvalue,2),
                                                          name = "Precision (%)")) +
          ggplot2::labs(title = "Classificaiton confusion table plot") +
          ggplot2::theme(legend.title = ggplot2::element_blank()))
  return(result_frame)
}




#'Wrong classification visualization
#'
#'Use dygraph to plot wrong classification bouts on all acceleration data.
#'
#'@param df_raw A data.frame or tibble contains accelerometer data. In this
#'  version, it should be arranged as x,y,z,x,y,z,...,label for each row and all
#'  rows should have the same length. For dual-axial accelerometer data, it should
#'  be arranged as x,y,x,y,...,label. And for single-axial, it's arranged as x,x,...,
#'  label.
#'@param axis_num An integer scalar indicating number of axies in the dataset.
#'@param df_result A data.frame resulted from \code{result_check}
#'
#'@details The base dygraph is the same with plot by \code{ACC_visual}. All wrong
#'predictions in the data.frame from \code{result_check} are denoted with dotted
#'line annotated with the predicted behaviour label on top.
#'
#'@return NULL
#'
#'@examples
#'pred <- plot_confusion_matrix(df = df_time, vec_label = label_vec)
#'plot_wrong_classifications(whitestork_acc_sorted, df_result = pred)
plot_wrong_classifications <- function(df_raw = NULL, axis_num = 3, df_result = NULL) {
  if (is.null(df_raw)) {
    stop("Please provide a valid data.frame!")
  }
  if (is.null(df_result)) {
    stop("Please provide a valid data.frame!")
  }

  row_num <- nrow(df_raw)
  col_num <- ncol(df_raw)
  val_range <- max(as.numeric(as.matrix(df_raw[, -col_num]))) -
    min(as.numeric(as.matrix(df_raw[, -col_num])))
  if (val_range > 16 | val_range < 2) {
    warning("Suggestion: transform raw data into 1g = 9.8 m/s2 for consistency")
  }

  wrong_ind <- which(df_result[[1]] != df_result[[2]])

  if (axis_num == 3) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 3)]
    sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 3)]
    sub_z <- df_raw[, seq(from = 3, to = col_num - 1, by = 3)]

    vecsub_x <- as.vector(t(as.matrix(sub_x)))
    vecsub_y <- as.vector(t(as.matrix(sub_y)))
    vecsub_z <- as.vector(t(as.matrix(sub_z)))

    value_range <- max(abs(range(c(vecsub_x, vecsub_y, vecsub_z))))

    label <- as.factor(rep(df_raw[, col_num], each = length(sub_x)))
    index <- seq(1/length(sub_x), row_num, 1/length(sub_x))

    df_plot <- data.frame(index, x = vecsub_x, y = vecsub_y, z = vecsub_z, label)

    seplabel <- summary(df_plot$label)[unique(df_plot$label)]
    dygraphs::dygraph(df_plot[,1:4], group = df_plot$label,
                      main = "All acceleration data") %>%
      dygraphs::dyAxis("y", label = "ACC",
                       valueRange = c(-value_range * 1.2, value_range * 1.2),
                       drawGrid = FALSE) %>%
      dygraphs::dyAxis("x", drawGrid = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyEvent(cumsum(seplabel)/length(sub_x),
                        unique(as.character(df_plot$label)), labelLoc = "bottom") %>%
      dygraphs::dyOptions(colors = c("#B22222", "#228B22", "#4169E1"), colorSaturation = 0.5) %>%
      dygraphs::dyEvent(wrong_ind, as.character(df_result[[2]][wrong_ind]),
                        labelLoc = "top", strokePattern = "dotted", color = "grey")
  } else if (axis_num == 2) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 2)]
    sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 2)]

    vecsub_x <- as.vector(t(as.matrix(sub_x)))
    vecsub_y <- as.vector(t(as.matrix(sub_y)))

    value_range <- max(abs(range(c(vecsub_x, vecsub_y))))

    label <- as.factor(rep(df_raw[, col_num], each = length(sub_x)))
    index <- seq(1/length(sub_x), row_num, 1/length(sub_x))

    df_plot <- data.frame(index, x = vecsub_x, y = vecsub_y, label)

    seplabel <- summary(df_plot$label)[unique(df_plot$label)]
    dygraphs::dygraph(df_plot[,1:3], group = df_plot$label,
                      main = "All acceleration data") %>%
      dygraphs::dyAxis("y", label = "ACC",
                       valueRange = c(-value_range * 1.2, value_range * 1.2),
                       drawGrid = FALSE) %>%
      dygraphs::dyAxis("x", drawGrid = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyEvent(cumsum(seplabel)/length(sub_x),
                        unique(as.character(df_plot$label)), labelLoc = "bottom") %>%
      dygraphs::dyOptions(colors = c("#B22222", "#228B22"), colorSaturation = 0.5) %>%
      dygraphs::dyEvent(wrong_ind, as.character(df_result[[2]][wrong_ind]),
                        labelLoc = "top", strokePattern = "dotted", color = "grey")
  } else if (axis_num == 1) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 1)]

    vecsub_x <- as.vector(t(as.matrix(sub_x)))

    value_range <- max(abs(range(c(vecsub_x))))

    label <- as.factor(rep(df_raw[, col_num], each = length(sub_x)))
    index <- seq(1/length(sub_x), row_num, 1/length(sub_x))

    df_plot <- data.frame(index, x = vecsub_x, label)

    seplabel <- summary(df_plot$label)[unique(df_plot$label)]
    dygraphs::dygraph(df_plot[,1:2], group = df_plot$label,
                      main = "All acceleration data") %>%
      dygraphs::dyAxis("y", label = "ACC",
                       valueRange = c(-value_range * 1.2, value_range * 1.2),
                       drawGrid = FALSE) %>%
      dygraphs::dyAxis("x", drawGrid = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyEvent(cumsum(seplabel)/length(sub_x),
                        unique(as.character(df_plot$label)), labelLoc = "bottom") %>%
      dygraphs::dyOptions(colors = c("#B22222"), colorSaturation = 0.5) %>%
      dygraphs::dyEvent(wrong_ind, as.character(df_result[[2]][wrong_ind]),
                        labelLoc = "top", strokePattern = "dotted", color = "grey")
  } else {
    stop("Please provide valid number of axis from 1, 2, or 3.")
  }
}
