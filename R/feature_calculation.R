#'Time domain feature calculation
#'
#'\code{calculate_feature_time} calculates accelerometer data into time domain mathemetical
#'features.
#'
#'Please see \url{https://en.wikipedia.org/wiki/Feature_engineering} for the
#'definition of feature engineering or feature extraction. In this function, basic
#'time domain features were calculated including: mean, variance, standard deviation,
#'max, min, range and ODBA. These features are calculated for each segment and each
#'ACC axis separately (denoted with prefix x,y,z), except for ODBA, which is calculated
#'using all available axes. ODBA is short for Overall Dynamic Body
#'Acceleration. This value was proved to be correlated with animal energy expenditure
#'in many studies, see Wilson et al. 2006 doi: 10.1111/j.1365-2656.2006.01127.x for
#'more details. Here, ODBA were calculated into mean value of the behaviour
#'segementation. Note that this function doesn't give inclusive calculations of all
#'potentially valuable time domain features and since it is asserted that feature
#'engineering plays an important part of machine learning model's success, more
#'customized features could also be calculated by users. A review reference - Brown
#'et al. 2013 doi:10.1186/2050-3385-1-20 - provides more potential features. One could
#'also reference feature engineering for human wearable devices which partly use
#'tri-axial accelerometer data since this is also a popular research and commercialized
#'field.
#'
#'@param df_raw A data.frame or tibble contains raw accelerometer data. In this
#'  version, it should be arranged as x,y,z,x,y,z,...,label for each row and all
#'  rows should have the same length.For dual-axial accelerometer data, it should
#'  be arranged as x,y,x,y,...,label. And for single-axial, it's arranged as x,x,...,
#'  label.
#'@param winlen_dba An integer scalar defining the length of running window for
#'  ODBA calculation. It is suggested that the length be derived over a period of
#'  no less than 1 stroke cycle. See Shepard et al. 2008 doi:10.3354/ab00104.
#'@param axis_num An integer scalar indicating number of axis in the dataset.
#'
#'@return A data.frame contains all caculated time domain features with 19
#'  variables (columns) named x_mean, y_mean, z_mean, x_variance, y_variance,
#'  z_variance, x_sd, y_sd, z_sd, x_max, y_max, z_max, x_min, y_min, z_min,
#'  x_range, y_range, z_range, ODBA. The cases (rows) in the returned data.frame
#'  equal to the cases of df_raw. For dual-axial accelerometer data, the returned
#'  data.frame will contain columns start with character "x" and "y", as well as
#'  ODBA. For single-axis, the returned data.frame will contain columns start with
#'  character "x" as well as ODBA.
#'
#'@details It is recommended to input sorted acclerometer data by \code{order_acc}.
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_time <- calculate_feature_time(df_raw = whitestork_acc_sorted, winlen_dba = 11)
#'df_time_1d <- calculate_feature_time(df_raw = whitestork_acc_sorted[, seq(3, 121, by = 3)],
#'                          winlen_dba = 11, axis_num = 1) #example for single-axial ACC
#'df_time_2d <- calculate_feature_time(df_raw = whitestork_acc_sorted[, -seq(1, 120, by = 3)],
#'                          winlen_dba = 11, axis_num = 2) #example for dual-axial ACC
calculate_feature_time <- function(df_raw = NULL, winlen_dba, axis_num = 3) {
  if (is.null(df_raw)) {
    stop("Please provide a valid data.frame!")
  }

  if (!exists("winlen_dba")) {
    stop("Provide a valid winlen_dba")
  }

  if (!is.numeric(winlen_dba)) {
    stop("Winlen_dba should be a number")
  }

  if (axis_num == 3 & (ncol(df_raw) - 1) %% 3 != 0 ) {
    stop("Number of data column (not including the label column) should be three multiples.")
  }

  if (axis_num == 2 & (ncol(df_raw) - 1) %% 2 != 0 ) {
    stop("Number of data column (not including the label column) should be two multiples.")
  }

  if (is.unsorted(as.character(df_raw[, ncol(df_raw)]))) {
    warning("This function expect the input data sorted by function order_acc.")
  }


  row_num <- dim(df_raw)[[1]]
  col_num <- dim(df_raw)[[2]]
  val_range <- max(as.numeric(as.matrix(df_raw[, -col_num]))) -
    min(as.numeric(as.matrix(df_raw[, -col_num])))
  # if (val_range > 16 | val_range < 2) {
  #   warning("Suggestion: transform raw data into 1g = 9.8 m/s2 for consistency")
  # }

  df_raw <- as.data.frame(df_raw)

  if (axis_num == 3) {
  sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 3)]
  sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 3)]
  sub_z <- df_raw[, seq(from = 3, to = col_num - 1, by = 3)]

  matsub_x <- as.matrix(sub_x)
  matsub_y <- as.matrix(sub_y)
  matsub_z <- as.matrix(sub_z)

  x_mean <- rowMeans(matsub_x)
  y_mean <- rowMeans(matsub_y)
  z_mean <- rowMeans(matsub_z)

  x_variance <- apply(matsub_x, 1, var)
  y_variance <- apply(matsub_y, 1, var)
  z_variance <- apply(matsub_z, 1, var)

  x_sd <- apply(matsub_x, 1, sd)
  y_sd <- apply(matsub_y, 1, sd)
  z_sd <- apply(matsub_z, 1, sd)

  x_max <- apply(matsub_x, 1, max)
  y_max <- apply(matsub_y, 1, max)
  z_max <- apply(matsub_z, 1, max)

  x_min <- apply(matsub_x, 1, min)
  y_min <- apply(matsub_y, 1, min)
  z_min <- apply(matsub_z, 1, min)

  x_range <- x_max - x_min
  y_range <- y_max - y_min
  z_range <- z_max - z_min

  # tic("running mean")
  sba_x <- t(zoo::rollapply(t(matsub_x), width = winlen_dba, FUN = mean,
                          fill = NA, align = "center"))
  sba_y <- t(zoo::rollapply(t(matsub_y), width = winlen_dba, FUN = mean,
                            fill = NA, align = "center"))
  sba_z <- t(zoo::rollapply(t(matsub_z), width = winlen_dba, FUN = mean,
                            fill = NA, align = "center"))
  # toc()

  dba_x <- abs(matsub_x - sba_x)
  dba_y <- abs(matsub_y - sba_y)
  dba_z <- abs(matsub_z - sba_z)

  ODBA <- apply(dba_x + dba_y + dba_z, 1, mean, na.rm = T)

  df_feature <- data.frame(x_mean, y_mean, z_mean,
                           x_variance, y_variance, z_variance,
                           x_sd, y_sd, z_sd,
                           x_max, y_max, z_max,
                           x_min, y_min, z_min,
                           x_range, y_range, z_range,
                           ODBA)
  } else if (axis_num == 2) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 2)]
    sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 2)]

    matsub_x <- as.matrix(sub_x)
    matsub_y <- as.matrix(sub_y)

    x_mean <- rowMeans(matsub_x)
    y_mean <- rowMeans(matsub_y)

    x_variance <- apply(matsub_x, 1, var)
    y_variance <- apply(matsub_y, 1, var)

    x_sd <- apply(matsub_x, 1, sd)
    y_sd <- apply(matsub_y, 1, sd)

    x_max <- apply(matsub_x, 1, max)
    y_max <- apply(matsub_y, 1, max)

    x_min <- apply(matsub_x, 1, min)
    y_min <- apply(matsub_y, 1, min)

    x_range <- x_max - x_min
    y_range <- y_max - y_min

    # tic("running mean")
    sba_x <- t(zoo::rollapply(t(matsub_x), width = winlen_dba, FUN = mean,
                              fill = NA, align = "center"))
    sba_y <- t(zoo::rollapply(t(matsub_y), width = winlen_dba, FUN = mean,
                              fill = NA, align = "center"))

    # toc()

    dba_x <- abs(matsub_x - sba_x)
    dba_y <- abs(matsub_y - sba_y)

    ODBA <- apply(dba_x + dba_y, 1, mean, na.rm = T)

    df_feature <- data.frame(x_mean, y_mean,
                             x_variance, y_variance,
                             x_sd, y_sd,
                             x_max, y_max,
                             x_min, y_min,
                             x_range, y_range,
                             ODBA)
  } else if (axis_num == 1) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 1)]

    matsub_x <- as.matrix(sub_x)

    x_mean <- rowMeans(matsub_x)

    x_variance <- apply(matsub_x, 1, var)

    x_sd <- apply(matsub_x, 1, sd)

    x_max <- apply(matsub_x, 1, max)

    x_min <- apply(matsub_x, 1, min)

    x_range <- x_max - x_min

    # tic("running mean")
    sba_x <- t(zoo::rollapply(t(matsub_x), width = winlen_dba, FUN = mean,
                              fill = NA, align = "center"))

    # toc()

    dba_x <- abs(matsub_x - sba_x)

    ODBA <- apply(dba_x, 1, mean, na.rm = T)

    df_feature <- data.frame(x_mean,
                             x_variance,
                             x_sd,
                             x_max,
                             x_min,
                             x_range,
                             ODBA)
  } else {
    stop("Please provide valid number of axis from 1, 2, or 3.")
  }
  return(df_feature)
}



#'Calculate main frequency, amplitude and entropy
#'@param data_vec a double vector contains one bout of one ACC axis
#'@noRd
max_freq_amp <- function(data_vec) {
  freq_vec <- abs(fft(lm(as.numeric(data_vec) ~
                           c(1:length(data_vec)))$residuals))
  half_ind <- floor(length(data_vec)/2)
  ind <- which(freq_vec[1:half_ind] == max(freq_vec[1:half_ind]))
  results <- c(as.numeric(ind), max(freq_vec[1:half_ind]),
               entropy::entropy(freq_vec[1:half_ind])^2/half_ind)
  return(results)
}



#'Frequency domain feature calculation
#'
#'\code{calculate_feature_freq} calculates accelerometer data into frequency domain
#'mathemetical features
#'
#'In this function, basic frequency domain features were calculated including:
#'main frequency, main amplitude and frequency entropy. All these features act on
#'each accelerometer axis, denoted by x, y, z. The extraction of these features
#'rely on fast Fourier transform (FFT) of raw accelerometer data. Frequency entropy
#'here measures unpredictability of the signal. See
#'\url{https://en.wikipedia.org/wiki/Entropy_(information_theory)}
#'for more information.
#'
#'@param df_raw A data.frame or tibble contains raw accelerometer data. In this
#'  version, it should be arranged as x,y,z,x,y,z,...,label for each row and all
#'  rows should have the same length. For dual-axial accelerometer data, it should
#'  be arranged as x,y,x,y,...,label. And for single-axial, it's arranged as x,x,...,
#'  label.
#'@param samp_freq A double scalar indicate sampling frequency of the accelerometer
#'@param axis_num An integer scalar indicating number of axies in the dataset.
#'@return A data.frame contains all caculated frequency domain features with 9
#'  variables (columns) named x_freqmain, y_freqmain, z_freqmain, x_freqamp,
#'  y_freqamp, z_freqamp, x_entropy, y_entropy, z_entropy. The cases (rows) in
#'  the returned data.frame equal to the cases of df_raw. For dual-axial accelerometer
#'  data, the returned data.frame will contain columns with prefix "x" and "y".
#'  For single-axis, the returned data.frame will contain columns with prefix "x".
#'
#'@details It is recommended to input sorted acclerometer data by \code{order_acc}.
#'@examples
#'data(whitestork_acc)
#'whitestork_acc_sorted <- order_acc(whitestork_acc)
#'df_freq <- calculate_feature_freq(df_raw = whitestork_acc_sorted, samp_freq = 10.54)
#'df_freq_1d <- calculate_feature_freq(df_raw = whitestork_acc_sorted[, seq(3, 121, by = 3)],
#'                          samp_freq = 10.54, axis_num = 1) #example for single-axial ACC
#'df_freq_2d <- calculate_feature_freq(df_raw = whitestork_acc_sorted[, -seq(1, 120, by = 3)],
#'                          samp_freq = 10.54, axis_num = 2) #example for dual-axial ACC

calculate_feature_freq <- function(df_raw = NULL, samp_freq, axis_num = 3) {
  if (is.null(df_raw)) {
    stop("Please provide a valid data.frame!")
  }

  if (!exists("samp_freq")) {
    stop("Provide a valid samp_freq")
  }

  if (!is.numeric(samp_freq)) {
    stop("Samp_freq should be a number")
  }

  if (axis_num == 3 & (ncol(df_raw) - 1) %% 3 != 0 ) {
    stop("Number of data column (not including the label column) should be three multiples.")
  }

  if (axis_num == 2 & (ncol(df_raw) - 1) %% 2 != 0 ) {
    stop("Number of data column (not including the label column) should be two multiples.")
  }

  if (is.unsorted(as.character(df_raw[, ncol(df_raw)]))) {
    warning("This function expect the input data sorted by function order_acc.")
  }

  row_num <- dim(df_raw)[[1]]
  col_num <- dim(df_raw)[[2]]
  val_range <- max(as.numeric(as.matrix(df_raw[, -col_num]))) -
    min(as.numeric(as.matrix(df_raw[, -col_num])))
  # if (val_range > 16 | val_range < 2) {
  #   warning("Suggestion: transform raw data into 1g = 9.8 m/s2 for consistency")
  # }

  df_raw <- as.data.frame(df_raw)

  if (axis_num == 3) {
  sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 3)]
  sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 3)]
  sub_z <- df_raw[, seq(from = 3, to = col_num - 1, by = 3)]

  matsub_x <- as.matrix(sub_x)
  matsub_y <- as.matrix(sub_y)
  matsub_z <- as.matrix(sub_z)

  freq_index <- samp_freq / length(sub_x)

  freq_x <- apply(matsub_x, 1, max_freq_amp)
  freq_y <- apply(matsub_y, 1, max_freq_amp)
  freq_z <- apply(matsub_z, 1, max_freq_amp)

  x_freqmain <- freq_x[1, ] * freq_index
  y_freqmain <- freq_y[1, ] * freq_index
  z_freqmain <- freq_z[1, ] * freq_index

  x_freqamp <- freq_x[2, ]
  y_freqamp <- freq_y[2, ]
  z_freqamp <- freq_z[2, ]

  x_entropy <- freq_x[3, ]
  y_entropy <- freq_y[3, ]
  z_entropy <- freq_z[3, ]

  df_feature <- data.frame(
    x_freqmain, y_freqmain, z_freqmain,
    x_freqamp, y_freqamp, z_freqamp,
    x_entropy, y_entropy, z_entropy
  )
  } else if (axis_num == 2) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 2)]
    sub_y <- df_raw[, seq(from = 2, to = col_num - 1, by = 2)]

    matsub_x <- as.matrix(sub_x)
    matsub_y <- as.matrix(sub_y)

    freq_index <- samp_freq / length(sub_x)

    freq_x <- apply(matsub_x, 1, max_freq_amp)
    freq_y <- apply(matsub_y, 1, max_freq_amp)

    x_freqmain <- freq_x[1, ] * freq_index
    y_freqmain <- freq_y[1, ] * freq_index

    x_freqamp <- freq_x[2, ]
    y_freqamp <- freq_y[2, ]

    x_entropy <- freq_x[3, ]
    y_entropy <- freq_y[3, ]

    df_feature <- data.frame(
      x_freqmain, y_freqmain,
      x_freqamp, y_freqamp,
      x_entropy, y_entropy
    )
  } else if (axis_num == 1) {
    sub_x <- df_raw[, seq(from = 1, to = col_num - 1, by = 1)]

    matsub_x <- as.matrix(sub_x)

    freq_index <- samp_freq / length(sub_x)

    freq_x <- apply(matsub_x, 1, max_freq_amp)

    x_freqmain <- freq_x[1, ] * freq_index

    x_freqamp <- freq_x[2, ]

    x_entropy <- freq_x[3, ]

    df_feature <- data.frame(
      x_freqmain,
      x_freqamp,
      x_entropy
    )
  } else {
    stop("Please provide valid number of axis from 1, 2, or 3.")
  }
  return(df_feature)
}
