% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimensionality.R
\name{unscale}
\alias{unscale}
\title{Unscaling and uncentering an object}
\usage{
unscale(x)
}
\arguments{
\item{x}{A numerical object from scale function.}
}
\value{
A numerical matrix that is unscaled and uncentered.
}
\description{
Takes in an object that was returned by the scale function, and reverses the scaling and centering, outputting the original matrix.
}
\examples{
example_mat=matrix(rnorm(25),nrow=5)
example_mat_scaled=scale(example_mat)
example_mat_unscaled=unscale(example_mat_scaled)

}
