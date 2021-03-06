% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaussian_Inference.r
\name{MPE.GaussianNIG}
\alias{MPE.GaussianNIG}
\title{Mean Posterior Estimate (MPE) of a "GaussianNIG" object}
\usage{
\method{MPE}{GaussianNIG}(obj, ...)
}
\arguments{
\item{obj}{A "GaussianNIG" object.}

\item{...}{Additional arguments to be passed to other inherited types.}
}
\value{
A named list, the MPE estimate of beta and sigma^2.
}
\description{
Generate the MPE estimate of (beta,sigma^2) in following Gaussian-NIG structure:
   \deqn{x \sim Gaussian(X beta,sigma^2)}
   \deqn{sigma^2 \sim InvGamma(a,b)}
   \deqn{beta \sim Gaussian(m,sigma^2 V)}
Where X is a row vector, or a design matrix where each row is an obervation. InvGamma() is the Inverse-Gamma distribution, Gaussian() is the Gaussian distribution. See \code{?dInvGamma} and \code{dGaussian} for the definitions of these distribution.\cr
The model structure and prior parameters are stored in a "GaussianNIG" object. \cr
The MPEs are E(beta,sigma^2|m,V,a,b,X,x)
}
\references{
Banerjee, Sudipto. "Bayesian Linear Model: Gory Details." Downloaded from http://www. biostat. umn. edu/~ph7440 (2008).
}
\seealso{
\code{\link{GaussianNIG}}
}
