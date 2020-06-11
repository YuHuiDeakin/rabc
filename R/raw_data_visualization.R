#'Sort rows by behaviour label
#'
#'Arrange the rows according to behaviour labels. Some
#'dataset might arrange same behaviours in different batches. For all other
#'functions in the rabc package data are expected to be presented in this ordered format.
#'
#'@param df_raw A data.frame or tibble contains raw accelerometer data. In this
#'  version, it should be arranged as x,y,z,x,y,z,...,label for each row and all
#'  rows should have the same length. For dual-axial accelerometer data, it should
#'  be arranged as x,y,x,y,...,label. And for single-axial, it's arranged as x,x,...,
#'  label.
#'@return A data.frame or tibble contains sorted raw accelerometer data.
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_muddled <- whitestork_acc[sample(1:nrow(whitestork_acc)),]
#'head(whitestork_acc_muddled[, ncol(whitestork_acc)], n = 15)
#'whitestork_acc_sorted <- order_acc(whitestork_acc_muddled)
#'head(whitestork_acc_sorted[, ncol(whitestork_acc)], n = 15)
order_acc <- function(df_raw = NULL) {
  if (is.null(df_raw)) {
    stop("Please provide a valid data.frame!")
  }

  df_ordered <- dplyr::arrange(df_raw, as.character(df_raw[[ncol(df_raw)]]))

  return(df_ordered)
}



#'Accelerometer data visualization
#'
#'Use dygraph to plot all accelerameter data grouped by behaviour types. It provides
#'the user a convenient impression of how the ACC signal relates to different behaviours
#'and also could be used for data quality control.
#'
#'@param df_raw A data.frame or tibble contains accelerometer data. In this
#'  version, it should be arranged as x,y,z,x,y,z,...,label for each row and all
#'  rows should have the same length. For dual-axial accelerometer data, it should
#'  be arranged as x,y,x,y,...,label. And for single-axial, it's arranged as x,x,...,
#'  label.
#'@param axis_num An integer scalar indicating number of axis in the dataset.
#'  Default value is 3, which represents tri-axial accelerometer data. It only
#'  accepts 1, 2 or 3.
#'@details It is highly recommended to input sorted accelerometer data by
#'\code{order_acc}. The x axis of this dygraph indicate the row sequence number
#'of sorted dataset. In this case, if any data is not consistent with its neighbours
#'which have the same behaviour labels, user can check observation record and decide
#'if this is caused by label mistake.
#'@return NULL
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'plot_acc(df_raw = whitestork_acc_sorted)
#'
#'plot_acc(df_raw = cbind(whitestork_acc_sorted[, seq(3,121,by = 3)],
#'                        whitestork_acc_sorted[,121,drop = FALSE]),
#'           axis_num = 1) #plot z axis data, column 121 contains behaviour labels
plot_acc <- function(df_raw = NULL, axis_num = 3) {
  if (is.null(df_raw)) {
    stop("Please provide a valid data.frame!")
  }

  row_num <- nrow(df_raw)
  col_num <- ncol(df_raw)
  val_range <- max(as.numeric(as.matrix(df_raw[, -col_num]))) -
    min(as.numeric(as.matrix(df_raw[, -col_num])))
  if (val_range > 16 | val_range < 2) {
    warning("Suggestion: transform raw data into 1g = 9.8 m/s2 for consistency")
  }

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
    dygraphs::dyOptions(colors = c("#B22222", "#228B22", "#4169E1"), colorSaturation = 0.5)
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
      dygraphs::dyOptions(colors = c("#B22222", "#228B22"), colorSaturation = 0.5)
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
      dygraphs::dyOptions(colors = c("#B22222"), colorSaturation = 0.5)
  } else {
    stop("Please provide valid number of axis from 1, 2, or 3.")
  }
}



#'Features sequential visualization
#'
#'Use dygraph to plot sequential feature(s) grouped by behaviour types. This function
#'allows users to generate a visual impression of the feature values by behaviour types.
#'
#'@param df_feature A data.frame or tibble calculated by \code{calculate_feature_time}
#'or \code{calculate_feature_freq}.
#'@param vec_label A character vector that contains all behaviour labels.
#'
#'@details It is recommended to input labels from sorted acclerometer data by
#'\code{order_acc}.  The x axis of this dygraph indicate the row sequence number
#'of sorted dataset.
#'
#'@return NULL
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_time <- calculate_feature_time(whitestork_acc_sorted, winlen_dba = 11)
#'plot_feature(df_feature = df_time[, "ODBA", drop = FALSE],
#'               vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)])
#'plot_feature(df_feature = df_time,
#'               vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)])
plot_feature <- function(df_feature = NULL, vec_label = NULL) {
  if (is.null(df_feature)) {
    stop("Please provide a valid feature data.frame!")
  }
  if (is.null(vec_label)) {
    stop("Please provide a valid label vector!")
  }

  df_feature$label <- as.factor(vec_label)
  df_feature <- dplyr::bind_cols(data.frame(index = 1:dim(df_feature)[1]), df_feature)

  seplabel <- summary(df_feature$label)[unique(df_feature$label)]
  dygraphs::dygraph(df_feature[, -dim(df_feature)[2]], group = df_feature$label,
          main = "Feature plot") %>%
    dygraphs::dyAxis("y", label = "feature values",
           #valueRange = c(-value_range * 1.2, value_range * 1.2),
           drawGrid = FALSE) %>%
    dygraphs::dyAxis("x", drawGrid = FALSE) %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyEvent(cumsum(seplabel),
            unique(as.character(df_feature$label)), labelLoc = "bottom") %>%
    dygraphs::dyOptions(colors = rainbow(dim(df_feature)[2] - 2)) %>%
    dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

}



#'Features group visualization
#'
#'This function generates feature distribution grouped by behaviour types.
#'
#'@param df_feature A data.frame or tibble calculated by \code{calculate_feature_time} or
#'  \code{calculate_feature_freq}.
#'@param vec_label A character vector that contains all behaviour type labels.
#'@param geom A character scalar to indicate type of plot. The default is "boxplot",
#'  it also supports "density" and "violin".
#'
#'@details In this function, plots are arranged by \code{facet_wrap}. In each plot
#'window, plots are arranged into 3 rows multiply by 3 columns. If the input total
#'number of features is not 9 multiples, the plots in final plot window will
#'automatically zoom to fill the whole window. It will be necessary to use
#'"Previous plot" and "Next plot" buttons to navigate between plots if the input
#'number of features is greater than 9.
#'
#'@return NULL
#'
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_time <- calculate_feature_time(whitestork_acc_sorted, winlen_dba = 11)
#'plot_feature_grouped(df_feature = df_time,
#'                    vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)])
#'
#'plot_feature_grouped(df_feature = df_time[, 1:15],
#'                    vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)])
plot_feature_grouped <- function(df_feature = NULL, vec_label = NULL,
                                geom = "boxplot") {
  if (is.null(df_feature)) {
    stop("Please provide a valid feature data.frame!")
  }
  if (is.null(vec_label)) {
    stop("Please provide a valid label vector!")
  }

  name_order <- names(df_feature)
  df_feature$label <- as.factor(vec_label)

  df_tran <- tidyr::pivot_longer(df_feature, -label, names_to = "features",
                            values_to = "Values")

  df_tran$features <- factor(df_tran$features, levels = name_order)

  rounds <- ceiling(length(name_order) / 9)

  for (i in 1:rounds) {
    if (i == rounds) {
      factor_ind <- ((i - 1) * 9 + 1):length(name_order)
    } else {
      factor_ind <- ((i - 1) * 9 + 1):(i * 9)
    }
    df_sub <- df_tran[unclass(df_tran$features) %in% factor_ind, ]

    if (geom == "boxplot") {
      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = label, y = Values)) +
        ggplot2::geom_boxplot() +
        ggplot2::facet_wrap("features", nrow = 3, ncol = 3, scales = "free_y")
      print(p) } else if (geom == "density") {
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = Values, fill = label)) +
          ggplot2::geom_density(alpha = 0.6) +
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::facet_wrap("features", nrow = 3, ncol = 3, scales = "free")
        print(p)
      } else if (geom == "violin") {
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(y = Values, x = label)) +
          ggplot2::geom_violin() +
          ggplot2::facet_wrap("features", nrow = 3, ncol = 3, scales = "free_y")
        print(p)
      } else {
        stop("Provide a valid geometry type!")
      }
  }
}
