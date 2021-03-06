% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_selection.R
\name{select_features}
\alias{select_features}
\title{Feature selection}
\usage{
select_features(
  df_feature = NULL,
  vec_label = NULL,
  filter = FALSE,
  cutoff = 0.9,
  wrapper = "XGBoost",
  no_features = 5
)
}
\arguments{
\item{df_feature}{A data.frame or tibble calculated by \code{calculate_feature_time} and/or
\code{calculate_feature_freq}.}

\item{vec_label}{A character vector that contains all behaviour type labels.}

\item{filter}{A logical value indicating whether to include filter function in
feature selection. It has default value "FALSE".}

\item{cutoff}{A double value setting threshold for correlation coefficient with
default value 0.9.}

\item{wrapper}{Character. The machine learning method for feature selection. In
this version, it's set to "XGBoost" and only accept this method.}

\item{no_features}{An integer value indicating how many features to be selected.}
}
\value{
A list. List[[1]] contains a matrix providing the classification accuracy
 for each of the features across all steps. Once a feature is selected into the
 selected feature set, the remaining values in this feature's column are set to
 zero. List[[2]] contains a character recording the names of the selected features
 in the order in which they were selected in the SFS process.
}
\description{
Select a subset of relevant features for use in behaviour classification.
}
\details{
This is a combination of a filter and a wrapper feature selection method
for feature selection. The filter part has the purpose of removing redundant features.
The filtering method uses the absolute values of pair-wise correlation coefficients
between features. If two variables have a high correlation, the function looks at
the mean absolute correlation of each variable and removes the variable with the
largest mean absolute correlation. The purpose of the wrapper is to select most
relevant features. The wrapper part uses stepwise forward selection (SFS) using extreme
boosting machine (XGBoost) method. The number of features to select (no_features)
can be set by the user, having a default of "no_features = 5". This parameter also
determines how many rounds of SFS are being conducted. In the first round, each
feature is individually used to train a classification model by XGBoost. The
feature with highest overall accuracy will be kept into the selected feature set.
Then, in every following round, each remaining feature will be combined with the
selected feature set to train a classification model and the one with the highest
accuracy will be kept into the selected feature set. The process will stop when
the number of rounds equals the no_features setting.
}
\examples{
data(whitestork_acc)
whitestork_acc_sorted <- order_acc(whitestork_acc)
df_time <- calculate_feature_time(df_raw = whitestork_acc_sorted, winlen_dba = 11)
results <- select_features(df_feature = df_time[,1:6],
                         vec_label = whitestork_acc_sorted[,ncol(whitestork_acc_sorted)],
                         no_features = 3)
}
