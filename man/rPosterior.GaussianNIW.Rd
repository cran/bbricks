% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaussian_Inference.r
\name{rPosterior.GaussianNIW}
\alias{rPosterior.GaussianNIW}
\title{Generate ramdom samples from the posterior distribution of a "GaussianNIW" object}
\usage{
\method{rPosterior}{GaussianNIW}(obj, ...)
}
\arguments{
\item{obj}{A "GaussianNIW" object.}

\item{...}{Additional arguments to be passed to other inherited types.}
}
\value{
list(mu,Sigma), where mu is a numeric vector, Sigma is a symmetric positive definite matrix.
}
\description{
Generate random samples from the posterior distribution of the following structure:
     \deqn{mu,Sigma|m,k,v,S \sim NIW(m,k,v,S)}
     \deqn{x|mu,Sigma \sim Gaussian(mu,Sigma)}
Where NIW() is the Normal-Inverse-Wishart distribution, Gaussian() is the Gaussian distribution. See \code{?dNIW} and \code{dGaussian} for the definitions of these distribution.\cr
The model structure and prior parameters are stored in a "GaussianNIW" object. \cr
Posterior distribution is NIW(mu,Sigma|m,k,v,S).
}
\examples{
obj <- GaussianNIW(gamma=list(m=c(0,0),k=1,v=2,S=diag(2)))
rPosterior(obj = obj)
}
\seealso{
\code{\link{GaussianNIW}}, \code{\link{dPosterior.GaussianNIW}}
}
