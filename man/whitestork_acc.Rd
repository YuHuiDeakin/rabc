% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/whitestork_acc-data.R
\docType{data}
\name{whitestork_acc}
\alias{whitestork_acc}
\title{Demo dataset for all examples in this package}
\format{
A data.frame
}
\source{
\link{http://accapp.move-ecol-minerva.huji.ac.il/demo/}
}
\usage{
data(whitestork_acc)
}
\description{
White stork (Ciconia ciconia) accelerometer data. This dataset includes 1746
behaviour bouts (each row is one bout with one behaviour type). Each row contains
121 columns. The first 120 columns are double accelerometer measurements from three
orhtogonal axes, arraged as x,y,z,x,y,z,...,x,y,z. The final column is character
that contains behaviour type label. The 5 different labels include "A_FLIGHT" -
active flight (77 cases), "P_FLIGHT" - passive filght (96), "WALK" -
walking (437), "STND" - standing (863), "SITTING" - resting (273). Sampling frequency
is 10.54 Hz.
}
\examples{
data(whitestork_acc)
dim(whitestork_acc)
summary(as.factor(whitestork_acc$V121))
}
\keyword{datasets}
