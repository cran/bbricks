% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaussian_Inference.r
\name{rPosteriorPredictive.GaussianInvWishart}
\alias{rPosteriorPredictive.GaussianInvWishart}
\title{Generate random samples from the posterior predictive distribution of a "GaussianInvWishart" object}
\usage{
\method{rPosteriorPredictive}{GaussianInvWishart}(obj, n, ...)
}
\arguments{
\item{obj}{A "GaussianInvWishart" object.}

\item{n}{integer, number of samples.}

\item{...}{Additional arguments to be passed to other inherited types.}
}
\value{
A matrix of n rows, each row is a sample.
}
\description{
Generate random samples from the posterior predictive distribution of the following structure:
    \deqn{x \sim Gaussian(mu,Sigma)}
    \deqn{Sigma \sim InvWishart(v,S)}
mu is known. Gaussian() is the Gaussian distribution. See \code{?dGaussian} and \code{?dInvWishart} for the definition of the distributions.\cr
The model structure and prior parameters are stored in a "GaussianInvWishart" object. \cr
Posterior predictive is a distribution of x|v,S,mu.
}
\examples{
obj <- GaussianInvWishart(gamma=list(mu=c(-1.5,1.5),v=3,S=diag(2)))
x <- rGaussian(100,mu = c(-1.5,1.5),Sigma = matrix(c(0.1,0.03,0.03,0.1),2,2))
ss <- sufficientStatistics(obj=obj,x=x,foreach = FALSE)
## use x to update the prior informatoin
posterior(obj=obj,ss = ss)
## use the posterior to generate new samples
rPosteriorPredictive(obj = obj,n=20)
}
\references{
Gelman, Andrew, et al. Bayesian data analysis. CRC press, 2013.

MARolA, K. V., JT KBNT, and J. M. Bibly. Multivariate analysis. AcadeInic Press, Londres, 1979.
}
\seealso{
\code{\link{GaussianInvWishart}}, \code{\link{dPosteriorPredictive.GaussianInvWishart}}
}
