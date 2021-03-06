% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{violinplot}
\alias{violinplot}
\title{boxplot + half of a violin plot}
\usage{
violinplot(y, colors = "viridis", lab = NA, point_size = 1,
  height = 0.4, ...)
}
\arguments{
\item{y}{either a list where len(list) == # of groups, or a dataframe}

\item{colors}{colors to use for the violinplots. defaults to viridis colors}

\item{lab}{labels for the groups. Each must be unique.}

\item{point_size}{size of the plotting characters for the data}

\item{height}{controls the size of the violinplots. defaults to 0.4 but your mileage will vary}
}
\value{
a list of length 2. First element of the list is your data coerced into a data.table. Second element is the group names.
}
\description{
plots the distribution of each group as a boxplot + half a violinplot with the data jittered
}
\examples{
violinplot(PlantGrowth)
violinplot(iris \%>\% select(Species, Sepal.Length), xlab= "species", ylab="sepal length")

}

