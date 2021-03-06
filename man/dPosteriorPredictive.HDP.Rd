% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dirichlet_Process.r
\name{dPosteriorPredictive.HDP}
\alias{dPosteriorPredictive.HDP}
\title{Posterior predictive density function of a "HDP" object}
\usage{
\method{dPosteriorPredictive}{HDP}(obj, x = NULL, z, k, j, LOG = TRUE, ...)
}
\arguments{
\item{obj}{A "HDP" object.}

\item{x}{Random samples of the "BasicBayesian" object.}

\item{z}{integer.}

\item{k}{integer, the partition label of the parameter space where the observation x is drawn from.}

\item{j}{integer, group label.}

\item{LOG}{Return the log density if set to "TRUE".}

\item{...}{Additional arguments to be passed to other inherited types.}
}
\value{
A numeric vector, the posterior predictive density.
}
\description{
Generate the the density value of the posterior predictive distribution of the following structure:
     \deqn{G|gamma \sim DP(gamma,U)}
     \deqn{pi_j|G,alpha \sim DP(alpha,G), j = 1:J}
     \deqn{z|pi_j \sim Categorical(pi_j)}
     \deqn{k|z,G \sim Categorical(G), \textrm{ if z is a sample from the base measure G}}
     \deqn{theta_k|psi \sim H0(psi)}
     \deqn{x|theta_k,k \sim F(theta_k)}
where DP(gamma,U) is a Dirichlet Process on positive integers, gamma is the "concentration parameter", U is the "base measure" of this Dirichlet process, U is an uniform distribution on all positive integers.  DP(alpha,G) is a Dirichlet Process on integers with concentration parameter alpha and base measure G. The choice of F() and H0() can be described by an arbitrary "BasicBayesian" object such as "GaussianGaussian","GaussianInvWishart","GaussianNIW", "GaussianNIG", "CatDirichlet", and "CatDP". See \code{?BasicBayesian} for definition of "BasicBayesian" objects, and see for example \code{?GaussianGaussian} for specific "BasicBayesian" instances. As a summary, An "HDP" object is simply a combination of a "CatHDP" object (see \code{?CatHDP}) and an object of any "BasicBayesian" type.\cr
In the case of HDP, z and k can only be positive integers. \cr
The model structure and prior parameters are stored in a "HDP" object. \cr
Posterior predictive density = p(x,z,k|gamma,alpha,psi) when x is not NULL, or p(z,k|gamma,alpha,psi) when x is NULL.
}
\references{
Teh, Yee W., et al. "Sharing clusters among related groups: Hierarchical Dirichlet processes." Advances in neural information processing systems. 2005.
}
\seealso{
\code{\link{HDP}}, \code{\link{dPosteriorPredictive.HDP}}, \code{\link{marginalLikelihood.HDP}}
}
