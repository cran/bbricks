% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaussian_Inference.r
\name{rPosteriorPredictive.GaussianNIW}
\alias{rPosteriorPredictive.GaussianNIW}
\title{Posterior predictive random generation of a "GaussianNIW" object}
\usage{
\method{rPosteriorPredictive}{GaussianNIW}(obj, n, ...)
}
\arguments{
\item{obj}{A "GaussianNIW" object.}

\item{n}{integer, number of samples.}

\item{...}{Additional arguments to be passed to other inherited types.}
}
\value{
A matrix of n rows, each row is a sample.
}
\description{
Generate random samples from the posterior predictive distribution of the following structure: \cr
     theta|gamma ~ NIW(gamma) \cr
     x|theta ~ Gaussian(theta) \cr
where theta = (mu,Sigma) is the Gaussian parameter, gamma = (m,k,v,S) is the Normal-Inverse-Wishart(NIW) parameter. \cr
The model structure and prior parameters are stored in a "GaussianNIW" object. \cr
Posterior predictive is a distribution of x|gamma.
}
\examples{
obj <- GaussianNIW(gamma=list(m=c(0,0),k=1,v=2,S=diag(2)))
rPosteriorPredictive(obj=obj,20)
}
\references{
Murphy, Kevin P. "Conjugate Bayesian analysis of the Gaussian distribution." def 1.22 (2007): 16.

Gelman, Andrew, et al. "Bayesian Data Analysis Chapman & Hall." CRC Texts in Statistical Science (2004).
}
\seealso{
@seealso \code{\link{GaussianNIW}}, \code{\link{dPosteriorPredictive.GaussianNIW}}
}