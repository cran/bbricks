## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # install from CRAN:
#  install.packages("bbricks")
#  # or install development version from GitHub:
#  # install.packages("devtools")
#  devtools::install_github("chenhaotian/Bayesian-Bricks")

## ----eval=FALSE---------------------------------------------------------------
#  ## Gibbs sampling for hierarchical linear regression
#  
#  library(bbricks)
#  ## load some hierarchical linear regression data
#  ## hlrData is a list of 3 numeric vectors
#  ## see ?hlrData for details
#  data(hlrData)
#  x <- hlrData$mathScore                    #math score as the dependent variable
#  X <- cbind(1,hlrData$socioeconomicStatus) #socioeconomic status as the independt variable
#  js <- hlrData$schoolID                    #school ID as the group IDs.
#  J <- max(js)
#  ## Initialization----------------------------------------------
#  ## initialize the Markov blanket of mu and Sigma
#  ## the prior parameters are: m=0, k=0.0001, v=3, S=diag(1)
#  objmS <- GaussianNIW(gamma = list(m =c(mean(hlrData$mathScore),0),k=0.0001,v=3,S=diag(2)))
#  ## initialize the Markov blanket of sigma^2
#  ## the prior parameters are: vs=2, Ss=diag(1)
#  objs <- GaussianInvWishart(gamma = list(mu=0,v=2,S=diag(1)))
#  ## initialize mu and Sigma by sampling from the prior
#  muSigma <- rPosterior(objmS)
#  ## initialize sigma^2 by sampling from the prior
#  sigma2 <- rPosterior(objs)
#  betaJ <- matrix(0,J,2)                  #place-holder the beta_j, j=1:J
#  epsilon <- x                            #place-holder for the random noises
#  ## Main Gibbs loop---------------------------------------------
#  maxit <- 100                           #number of sampling iterations
#  burnin <- 50                           #number of burn-in samples
#  meanBeta <- betaJ                      #place-hoder for the sample means of beta
#  it <- 1
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      ## Step1: sample beta_j, j in 1:100
#      for(j in 1L:J){
#          objb <- LinearGaussianGaussian(gamma=list(Sigma=sigma2,m=muSigma$mu,S=muSigma$Sigma))
#          idx <- js == j
#          ss <- sufficientStatistics(obj = objb,x=x[idx],A=X[idx,,drop=FALSE])
#          posterior(obj = objb,ss = ss)
#          betaJ[j,] <- rPosterior(objb)
#      }
#      ## calculate the sample mean
#      if(it>burnin) meanBeta <- meanBeta+betaJ/(maxit-burnin)
#      ## Step2: sample mu and Sigma
#      ssmS <- sufficientStatistics(obj = objmS,x=betaJ)
#      posterior(obj = objmS,ss = ssmS)
#      muSigma <- rPosterior(obj = objmS)
#      ## Step3: sample sigma^2
#      for(j in 1L:J){
#          idx <- js == j
#          epsilon[idx] <- x[idx]-X[idx,,drop=FALSE]%*%betaJ[j,]
#      }
#      sss <- sufficientStatistics(obj = objs,x=epsilon)
#      posterior(obj = objs,ss = sss)
#      sigma2 <- rPosterior(objs)
#      ## increase iteration counter
#      it <- it+1
#      setTxtProgressBar(pb,it)
#      ## if continue sampling, then discard the information in objmS and objs
#      ## to make room for the new information in the next iteration.
#      if(it < maxit){
#          posteriorDiscard(obj = objmS,ss = ssmS)
#          posteriorDiscard(obj = objs,ss = sss)
#      }
#  }
#  ## plot the result
#  ## gray lines are the betas of each school
#  ## black line is the beta for all the data as a whole
#  plot(x=0, xlim = range(0.2,0.8),ylim = c(20,35),xlab = "socioeconomic status",ylab = "math score")
#  for(j in 1L:J)
#      abline(a=betaJ[j,2],b=betaJ[j,1],col="gray")
#  allSchools <- lm(x~X-1)$coefficients
#  abline(a=allSchools[2],b=allSchools[1],lwd=3)
#  

## ----eval=FALSE---------------------------------------------------------------
#  ## Estimate cancer mortality rates using Gibbs sampling
#  
#  library(bbricks)
#  
#  ## see ?cancerData for details
#  data(cancerData)
#  ## Step1: Initialization----------------------------------------------
#  K <- length(cancerData)                          #then number of cities
#  eta <- 1                                         #assume eta is known, eta=1
#  ## initialize alpha, PI, and sufficient statistics
#  a <- rexp(2,rate = eta)                 #initialize alpha
#  PI <- matrix(0,K,2L)                    #initialize pi
#  cityPrior <- CatDirichlet(gamma = list(alpha=a,uniqueLabels=c("death","no death")))
#  citySS <- lapply(cancerData,function(x){sufficientStatistics(obj = cityPrior,x=x)}) #sufficient statistics of each city
#  ## initialize functions used in Metropolis-Hastings, see ?MetropolisHastings for details
#  ## density of the target distribution
#  dp <- function(a){
#      if(any(a<0)) -Inf
#      else sum(dDir(x=PI,alpha = a,LOG = TRUE))+sum(dexp(x=a,rate = eta,log = TRUE))
#  }
#  ## density of the proposal distribution
#  dq <- function(anew,a){1}                #use a independent proposal
#  ## random sample generator of the proposal distribution
#  rq <- function(x){
#      c(runif(1,x[1]-1,x[1]+1),
#        runif(1,x[2]-1,x[2]+1))
#  }
#  ## Step2: main Gibbs sampling loop between alpha and pi --------------
#  maxit <- 1000
#  burnin <- 500                            #number of burn-in samples
#  meanPI <- numeric(K)                     #place-hoder for the sample mean
#  it <- 1
#  while(it<=maxit){
#      ## Step1: sample pi from p(pi|a,x)-------------
#      for(k in 1L:K){
#          posterior(obj = cityPrior,ss=citySS[[k]])
#          PI[k,] <- rDir(n=1,alpha = cityPrior$gamma$alpha)
#          posteriorDiscard(obj = cityPrior,ss=citySS[[k]])
#      }
#      ## calculate the sample mean
#      if(it>burnin) meanPI <- meanPI+PI[,1]/(maxit-burnin)
#      ## Step2: sample a from p(a|pi,g)--------------
#      ## use Metropolis-Hastings
#      a <- MetropolisHastings(nsamples = 1,xini = a,dp=dp,dq=dq,rq=rq)
#      ## increase iteration counter
#      it <- it+1
#  }
#  ## Step3: plot the result---------------------------------------------
#  ## black bars are the sample mean from the hierarchical Bayesian model
#  ## blue bars are the MLE of the mortality rates.
#  plot(1:K,meanPI,type = "h",xlab = "city",ylab = "mortality rate",lwd=3)
#  lines(1:K+0.2,sapply(cancerData,function(l){sum(l=="death")/length(l)}),type = "h",col = "blue",lwd = 3)
#  legend(1, 0.005, legend=c("Sample Mean", "MLE"),col=c("black", "blue"), lty=c(1,1), cex=1,lwd = 3)

## ----eval=FALSE---------------------------------------------------------------
#  ## Get the MAP estimate of pi and theta using EM algorithm.
#  
#  library(bbricks)
#  
#  ## load some mixture of Gaussian samples.
#  ## mmData is a numeric matrix with 2 columns, each row is a sample
#  ## see ?mmData for details
#  data(mmData)
#  K <- 4L                                 #number of clusters(mixtures components)
#  z <- matrix(runif(nrow(mmData)*K),nrow(mmData),K) #the expected cluster label of each observation
#  allK <- 1L:K    #temp variable, all component labels
#  allZ <- rep(allK,each=nrow(mmData))     #temp variable, all possible cluster labels for all observations
#  ## z, pi and alpha are distributed as a Categorical-Dirichlet sturcture:
#  mc <- CatDirichlet(gamma = list(alpha=0.5,uniqueLabels=allK)) # create a CatDirichlet object to track the posterior info, see ?CatDirichlet for details
#  ## each component distribution is a Gaussian-NIW structure:
#  ec <- replicate(K,GaussianNIW(gamma = list(m=c(0,0),k=0.00001,v=2,S=diag(2)))) # create a GaussianNIW object to track the posterior info of each mixture component, see ?GaussianNIW for details
#  mcMAP <- MAP(mc)                        #initialize the MAP estimate of pi
#  ecMAP <- replicate(K,list(muMAP=runif(2),sigmaMAP=diag(2)),simplify = FALSE) #initialize the MAP estimate of theta
#  ## The main EM loop
#  maxit <- 100                            #number of EM loops
#  it <- 1
#  while(it<=maxit){
#      ## E-step---------------------------------------------------------
#      ## calculate the expected cluster labels: p(z|pi,theta)
#      for(k in allK) z[,k] <- dGaussian(x=mmData,mu = ecMAP[[k]]$muMAP,Sigma=ecMAP[[k]]$sigmaMAP)+log(mcMAP[k])
#      z <- exp(z-logsumexp(z))            #use logsumexp() to avoid numerical underflow
#      ## calculate the expected sufficient statistics
#      ssComponents <- lapply(allK,function(k){
#          sufficientStatistics_Weighted(obj = ec[[k]],x=mmData,w=z[,k])
#      })                                  #the expected sufficient statistics of each Gaussian component
#      ssPi <- sufficientStatistics_Weighted(obj = mc,x=allZ,w=as.vector(z)) #the expected sufficient statistics of the cluster label distribution
#      ## M-step---------------------------------------------------------
#      ## use the sufficient statistics to update the prior distributions:
#      for(k in allK) posterior(obj = ec[[k]],ss=ssComponents[[k]]) #update component distributions
#      posterior(obj = mc,ss = ssPi)                                #update cluster label distribution
#      ## calculate the MAP estimates from posterior:
#      mcMAP <- MAP(mc)
#      ecMAP <- lapply(ec,MAP)
#      ## Reset the priors for next EM loop-----------------------------------------
#      ## to prepare for the next EM iteration, discard the sufficient statistics info from the posteriors:
#      for(k in allK) posteriorDiscard(obj = ec[[k]],ss=ssComponents[[k]])
#      posteriorDiscard(obj = mc,ss = ssPi)
#      ## increase the iteration counter
#      it <- it+1
#  }
#  
#  plot(mmData,col=apply(z,1,which.max)) #plot the best estimates
#  mcMAP                                 #the MAP estimate of pi
#  ecMAP                                 #the MAP estimate of theta_z

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn DP-MM posteriors using Gibbs sampling
#  
#  library(bbricks)
#  
#  ## load some mixture of Gaussian samples.
#  ## mmData is a numeric matrix with 2 columns, each row is a sample
#  ## see ?mmData for details
#  data(mmData)
#  maxit <- 100                            #number of total samples
#  burnin <- 50                            #number of burnin samples
#  ## Step1: Initialization -----------------------------------------
#  obj <- DP(gamma = list(alpha=10,H0aF="GaussianNIW",parH0=list(m=c(0,0),k=0.001,v=2,S=diag(2)))) #create a DP object to track all the changes, the DP object in this case is a combination of a CatDP object and a GaussianNIW object
#  z <- matrix(1L,nrow(mmData),maxit-burnin)    #place-holder for the sampled z
#  ss <- sufficientStatistics(obj,x=mmData,foreach = TRUE) #sufficient statistics of each observed sample
#  N <- nrow(mmData)
#  for(i in 1L:N){ # initialize labels before Gibbs sampling
#      z[i,1] <- rPosteriorPredictive(obj = obj,n=1,x=mmData[i,,drop=FALSE])
#      posterior(obj = obj,ss = ss[[i]], z = z[i,1])
#  }
#  ## Step2: Main Gibbs sampling loop--------------------------------
#  it <- 1                                 #iteration tracker
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      if(it>burnin) colIdx <- it-burnin
#      else colIdx <- 1
#      for(i in 1L:N){
#          ## remove the sample information from the posterior
#          posteriorDiscard(obj = obj,ss = ss[[i]],z=z[i,colIdx])
#          ## get a new sample
#          z[i,colIdx] <- rPosteriorPredictive(obj = obj,n=1,x=mmData[i,,drop=FALSE])
#          ## add the new sample information to the posterior
#          posterior(obj = obj,ss = ss[[i]],z=z[i,colIdx])
#      }
#      if(it>burnin & colIdx<ncol(z)) z[,colIdx+1] <- z[,colIdx] #copy result of previous iteration
#      it <- it+1
#      setTxtProgressBar(pb,it)
#      if(it>=maxit){cat("\n");break}
#      plot(x=mmData[,1],y=mmData[,2],col=z[,colIdx]) #to see how the labels change in each iteration
#  }
#  ## Step3: Estimate group labels of each observation---------------
#  ## pick the most frequent z as the best estimate
#  zBest <- apply(z,1,function(l){
#      tmp <- table(l)
#      names(tmp)[which.max(tmp)]
#  })
#  plot(x=mmData[,1],y=mmData[,2],col=zBest)

## ----eval=FALSE---------------------------------------------------------------
#  ## 1. add the information of the 4 observed samples to the DP object
#  ssObserved <- sufficientStatistics(obj=obj,x=mmData[c(50,100,150,200),,drop=FALSE],foreach = TRUE)
#  for(i in 1L:4L) posterior(obj = obj,ss = ssObserved[[i]], z = i) # the choice of cluster label 'z' for the 4 observed samples are arbitrary, as long as they are different from each other. In this example I simply use z=1L:4L.
#  ## 2. remove the 4 samples from the upcoming Gibbs sampling procedure
#  mmData <- mmData[-c(50,100,150,200),]

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn HDP-MM posteriors using Gibbs sampling
#  
#  library(bbricks)
#  
#  ## load some mixture of Gaussian samples.
#  ## mmhData is a list of two elements. mmhData$x is a matrix of Gaussian observations, each row is an observation; mmhData$groupLabel is the group label of each observation.
#  ## see ?mmhData for details
#  data(mmhData)
#  x <- mmhData$x
#  js <- mmhData$groupLabel
#  ## Step1: Initialization------------------------------------------
#  maxit <- 50                             #iterative for maxit times
#  burnin <- 30                            #number of burn in samples
#  ## create a HDP object to track all the changes, the HDP object in this case is a combination of a CatHDP object and a GaussianNIW object:
#  obj <- HDP(gamma = list(gamma=1,j=max(js),alpha=1,
#                          H0aF="GaussianNIW",
#                          parH0=list(m=c(0,0),k=0.001,v=2,S=diag(2)*0.01)))
#  ss <- sufficientStatistics(obj$H,x=x,foreach = TRUE) #sufficient statistics
#  set.seed(1)
#  z <- rep(1L,nrow(x))
#  k <- matrix(1L,nrow(x),maxit-burnin)    #place-holder for the sampled k
#  N <- length(ss)
#  for(i in 1L:N){# initialize k and z
#      tmp <- rPosteriorPredictive(obj = obj,n=1,x=x[i,,drop=FALSE],j=js[i])
#      z[i] <- tmp["z"]
#      k[i,1] <- tmp["k"]
#      posterior.HDP(obj = obj,ss = ss[[i]],ss1 = k[i],ss2 = z[i],j = js[i])
#  }
#  ## Step2: main Gibbs loop---------------------------------------------
#  it <- 1                                 #iteration tracker
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      if(it>burnin) colIdx <- it-burnin
#      else colIdx <- 1
#      for(i in 1L:N){
#          ## remove the sample from the posterior info
#          posteriorDiscard(obj = obj,ss = ss[[i]],ss1=k[i,colIdx],ss2=z[i],j=js[i])
#          ## resample a new partition
#          tmp <- rPosteriorPredictive(obj = obj,n=1,x=x[i,,drop=FALSE],j=js[i])
#          z[i] <- tmp["z"]
#          k[i,colIdx] <- tmp["k"]
#          ## add the information of the new sample
#          posterior(obj = obj,ss = ss[[i]], ss1=k[i,colIdx],ss2 = z[i],j=js[i])
#      }
#      if(it>burnin & colIdx<ncol(k)) k[,colIdx+1] <- k[,colIdx] #copy result of previous iteration
#      it <- it+1
#      plot(x=x[,1],y=x[,2],col=k[,colIdx])         #to visualize the group label dynamics
#      setTxtProgressBar(pb,it)
#  }
#  ## Step3: Estimate group labels of each observation---------------
#  ## pick the most frequent k as the best estimate
#  kBest <- apply(k,1,function(l){
#      tmp <- table(l)
#      names(tmp)[which.max(tmp)]
#  })
#  plot(x=x[,1],y=x[,2],col=kBest)

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn HDP-LDA posteriors of the farm-ads corpus
#  
#  ## load a subset of farm ads data from https://archive.ics.uci.edu/ml/datasets/Farm+Ads
#  ## see ?farmadsData for details
#  data(farmadsData)
#  word <- farmadsData$word
#  document <- farmadsData$document
#  ## Step1: Initialization------------------------------------------
#  set.seed(1)
#  maxit <- 30                            #iterative for maxit times
#  z <- rep(1L,length(word))
#  k <- rep(1L,length(word))
#  ## initialize
#  uniqueWords <- unique(word)
#  obj <- HDP(gamma = list(gamma=1,j=max(document),alpha=1,H0aF="CatDirichlet",parH0=list(alpha=rep(0.5,length(uniqueWords)),uniqueLabels=uniqueWords))) #create a HDP object to track all the changes, the HDP object in this case is a combination of a CatHDP object and a CatDrirchlet object
#  N <- length(word)
#  ## initialize k and z
#  for(i in 1L:N){
#      tmp <- rPosteriorPredictive(obj = obj,n=1,x=word[i],j=document[i])
#      z[i] <- tmp["z"]
#      k[i] <- tmp["k"]
#      posterior(obj = obj,ss = word[i], ss2 = z[i],j=document[i],ss1=k[i])
#  }
#  ## Step2: main Gibbs loop---------------------------------------------
#  it <- 1                                 #iteration tracker
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      for(i in 1L:N){
#          posteriorDiscard.HDP(obj = obj,ss = word[i],ss1=k[i],ss2=z[i],j=document[i]) #remove the sample information from the posterior
#          tmp <- rPosteriorPredictive(obj = obj,n=1,x=word[i],j=document[i])   #get a new sample
#          z[i] <- tmp["z"]
#          k[i] <- tmp["k"]
#          posterior(obj = obj,ss = word[i],ss1=k[i], ss2 = z[i],j=document[i]) #add the new sample information to the posterior
#      }
#      it <- it+1
#      setTxtProgressBar(pb,it)
#  }
#  ## Step3: plot the result --------------------------------------------
#  ## see which topics are most frequently appeared:
#  order(sapply(obj$X,function(l){sum(l$gamma$alpha)}),decreasing = TRUE)
#  ## seems topic 2 and 1 appear the most, let's plot them:
#  ## install.packages("wordcloud") # for word-cloud
#  ## install.packages("RColorBrewer") # color palettes
#  ## print topic 1
#  wordcloud:: wordcloud(words = obj$X[[1]]$gamma$uniqueLabels,
#                        freq = obj$X[[1]]$gamma$alpha,
#                        min.freq = 1,
#                        max.words=100,
#                        random.order=FALSE, rot.per=0.35,
#                        colors=RColorBrewer::brewer.pal(5, "Set1"))
#  ## print topic 2
#  wordcloud:: wordcloud(words = obj$X[[2]]$gamma$uniqueLabels,
#                        freq = obj$X[[2]]$gamma$alpha,
#                        min.freq = 1,
#                        max.words=100,
#                        random.order=FALSE, rot.per=0.35,
#                        colors=RColorBrewer::brewer.pal(5, "Set1"))

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn HDP2-MM posteriors using Gibbs sampling
#  
#  library(bbricks)
#  
#  ## load some mixture of Gaussian samples.
#  ## mmhData is a list of 3 elements. mmhhData$x is a matrix of Gaussian observations, each row is an observation; mmhhData$groupLabel is the group label of each observation. mmhhData$subGroupLabel is the subgroup label of each observation.
#  ## there are 2 groups, group1 has 10 subgroups, group has 20 subgroups.
#  ## see ?mmhhData for details
#  data(mmhhData)
#  x <- mmhhData$x
#  groupLabel <- mmhhData$groupLabel
#  subGroupLabel <- mmhhData$subGroupLabel
#  ## Step1: Initialization------------------------------------------
#  maxit <- 50                            #iterative for maxit times
#  burnin <- 20                           #number of burn in samples
#  ## create a HDP2 object to track all the changes, the HDP2 object in this case is a combination of a CatHDP2 object and a GaussianNIW object
#  obj <- HDP2(gamma = list(eta=1,gamma=1,alpha=1,m=2L,j=c(10L,20L),H0aF="GaussianNIW",parH0=list(m=c(0,0),k=0.001,v=1.1,S=diag(2)*0.001)))
#  ss <- sufficientStatistics(obj$H,x=x,foreach = TRUE) #sufficient statistics
#  z <- rep(1L,nrow(x))
#  k <- rep(1L,nrow(x))
#  u <- matrix(1L,nrow(x),maxit-burnin)
#  N <- length(ss)
#  ## initialization k, z and u
#  for(i in 1L:N){
#      tmp <- rPosteriorPredictive(obj = obj,n=1,x=x[i,,drop=FALSE],m=groupLabel[i],j=subGroupLabel[i])
#      z[i] <- tmp["z"]
#      k[i] <- tmp["k"]
#      u[i,1] <- tmp["u"]
#      posterior.HDP2(obj = obj,ss = ss[[i]],ss1 = u[i,1],ss2 = k[i],ss3 = z[i],m=groupLabel[i],j = subGroupLabel[i])
#  }
#  ## Step2: main Gibbs loop---------------------------------------------
#  it <- 1                                 #iteration counter
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      if(it>burnin) colIdx <- it-burnin
#      else colIdx <- 1
#      for(i in 1L:N){
#          ## remove the sample from the posterior info
#          posteriorDiscard(obj = obj,ss = ss[[i]],ss1=u[i,colIdx],ss2=k[i],ss3 = z[i],m=groupLabel[i],j=subGroupLabel[i])
#          ## resample a new partition
#          tmp <- rPosteriorPredictive(obj = obj,n=1L,x=x[i,,drop=FALSE],m=groupLabel[i],j=subGroupLabel[i])
#          z[i] <- tmp["z"]
#          k[i] <- tmp["k"]
#          u[i,colIdx] <- tmp["u"]
#          ## add the information of the new sample
#          posterior(obj = obj,ss = ss[[i]], ss1=u[i,colIdx],ss2 = k[i],ss3 = z[i],m=groupLabel[i],j=subGroupLabel[i])
#      }
#      if(it>burnin & colIdx<ncol(u)) u[,colIdx+1] <- u[,colIdx] #copy result of previous iteration
#      it <- it+1
#      plot(x=x[,1],y=x[,2],col=u[,colIdx])
#      setTxtProgressBar(pb,it)
#  }
#  ## Step3: Estimate group labels of each observation---------------
#  ## pick the most frequent u as the best estimate
#  uBest <- apply(u,1,function(l){
#      tmp <- table(l)
#      names(tmp)[which.max(tmp)]
#  })
#  plot(x=x[,1],y=x[,2],col=uBest)

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn posteriors of the Bayesian linear regression model
#  
#  library(bbricks)
#  
#  ## lrData is a list of two elements. lrData$x is the sample set of the dependent variable; lrData$X is the sample set of the independent variable
#  ## see ?lrData for details
#  data(lrData)
#  X <- lrData$X                           #a matrix of 1 column
#  x <- lrData$x                           #a numeric vector
#  ## task 1. update the prior into posterior using X and x
#  obj <- GaussianNIG(gamma=list(m=0,V=1,a=1,b=0)) #create a GaussianNIG object
#  ss <- sufficientStatistics(obj = obj,X=X,x=x)   #the sufficient statistics of X and x
#  posterior(obj = obj,ss = ss)                    #add the infomation to the posterior
#  ## task 2. get MAP estimate of beta and sigma^2 from the posterior
#  bsMAP <- MAP(obj)                               #get the MAP estimate of beta and sigma^2
#  bsMAP                                           #print the MAP estimate
#  ## plot the MAP estimate of the regression line
#  plot(X,X%*%bsMAP$betaMAP,type = "l")
#  points(X,x,pch=20)
#  ## task 3. calculate marginal likelihood
#  ## generate some new data
#  Xnew <- matrix(runif(3,min=0,max=),ncol=1)
#  xnew <- Xnew*0.2+rnorm(3,sd=10)
#  marginalLikelihood(obj = obj,X=x,x=x,LOG = TRUE)
#  ## task 4. calculate the posterior prediction
#  ## say we want to predict x at the location X=100
#  predictedSamples <- rPosteriorPredictive(obj = obj,X=matrix(101,ncol = 1),n=1000)
#  ## histogram of the prediction
#  hist(predictedSamples)
#  ## the mean and standard devition of the prediction
#  mean(predictedSamples)
#  sd(predictedSamples)
#  

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn HMM posteriors using single-move sampling
#  
#  library(bbricks)
#  
#  ## load some hidden markov data
#  ## see ?hmmData for details
#  data(hmmData)
#  x <- hmmData$x
#  Nsegs <- hmmData$Nsegs                  #number of segments
#  breaks <- hmmData$breaks                #break index of the segments
#  ## Step1: Initialization------------------------------------------
#  ## create Categorical-Dirichlet object for pi0 and transition matrix
#  K <- 3L
#  allk <- 1L:K
#  transitionObj <- replicate(K,CatDirichlet(gamma = list(alpha=1,uniqueLabels=allk)))
#  piObj <- CatDirichlet(gamma = list(alpha=1,uniqueLabels=allk))
#  ## create Gaussian-NIW object for observations
#  obsObj <- replicate(K,GaussianNIW(gamma=list(m=c(0,0),k=0.001,v=3,S=diag(2))))
#  ## place holder for the sampled hidden states
#  z <- integer(nrow(x))
#  ## observation sufficient statistics
#  ssx <- sufficientStatistics(obj = obsObj[[1]],x=x,foreach = TRUE)
#  ## initialize states with random assignment
#  z <- sample(allk,length(z),replace = TRUE)
#  for(i in 1L:Nsegs){
#      segStart <- breaks[i]+1L
#      segEnd <- breaks[i+1]
#      for(j in segStart:segEnd){
#          if(j==segStart) posterior(piObj,ss = z[j])
#          else posterior(transitionObj[[z[j-1L]]],ss = z[j])
#          posterior(obsObj[[z[j]]],ss = ssx[[j]])
#      }
#  }
#  ## Step2: main Gibbs loop---------------------------------------------
#  maxit <- 20
#  it <- 1
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      ## for all segments
#      for(i in 1L:Nsegs){
#          segStart <- breaks[i]+1L
#          segEnd <- breaks[i+1]
#          for(j in segStart:segEnd){
#              ## discard previous z
#              if(j==segStart){
#                  posteriorDiscard(piObj,ss = z[j])
#              }else if(j==segEnd){
#                  posteriorDiscard(transitionObj[[z[j-1L]]],ss = z[j])
#              }else{
#                  posteriorDiscard(transitionObj[[z[j-1L]]],ss = z[j])
#                  posteriorDiscard(transitionObj[[z[j]]],ss = z[j+1L])
#              }
#              posteriorDiscard(obsObj[[z[j]]],ss = ssx[[j]])
#              ## sample new z
#              if(j==segStart)
#                  pz <- dPosteriorPredictive(obj = piObj,x=allk,LOG = TRUE)
#              else
#                  pz <- dPosteriorPredictive(obj = transitionObj[[z[j-1L]]],x=allk,LOG = TRUE)
#              if(j!=segEnd)
#                  for(k in allk) pz[k] <- pz[k]+dPosteriorPredictive(obj = obsObj[[k]],x=x[j,,drop=FALSE],LOG = TRUE)+dPosteriorPredictive(obj = transitionObj[[k]],x=z[j+1L],LOG = TRUE)
#              else
#                  for(k in allk) pz[k] <- pz[k]+dPosteriorPredictive(obj = obsObj[[k]],x=x[j,,drop=FALSE],LOG = TRUE)
#              pz <- exp(pz-logsumexp(pz))
#              currentK <- sample.int(K,size=1,prob = pz)
#              z[j] <- currentK
#              ## update prior with newly sampled z
#              if(j==segStart){
#                  posterior(piObj,ss = currentK)
#              }else if(j==segEnd){
#                  posterior(transitionObj[[z[j-1L]]],ss = currentK)
#              }else{
#                  posterior(transitionObj[[z[j-1L]]],ss = currentK)
#                  posterior(transitionObj[[currentK]],ss = z[j+1])
#              }
#              posterior(obsObj[[currentK]],ss = ssx[[j]])
#          }
#      }
#      it <- it+1
#      setTxtProgressBar(pb,it)
#  }
#  ## print the MAP estimates of the transition matrix
#  for(k in allk) print(round(MAP(transitionObj[[k]]),2))
#  ## print the MAP estimates of the observation distributions
#  for(k in allk) print(MAP(obsObj[[k]]))
#  ## MAP estimates of the initial state distribution pi0
#  MAP(piObj)

## ----eval=FALSE---------------------------------------------------------------
#  ## Learn HDP-HMM posteriors using single-move sampling
#  
#  library(bbricks)
#  
#  source("Programs/Codes/Bayesian Nonparametrics/Dirichlet_Process.r")
#  ## load some hidden markov data
#  ## see ?hmmData for details
#  data(hmmData)
#  x <- hmmData$x
#  Nsegs <- hmmData$Nsegs
#  breaks <- hmmData$breaks
#  ## Step1: Initialization------------------------------------------
#  transitionObj <- HDP(gamma = list(gamma=1,alpha=1,j=1,H0aF="GaussianNIW",parH0=list(m=c(0,0),k=0.001,v=3,S=diag(2))))
#  ## place holder for the sampled hidden states
#  zk <- matrix(0L,ncol = 2,nrow = nrow(x))
#  ## observation sufficient statistics
#  ssx <- sufficientStatistics(obj = GaussianNIW(),x=x,foreach = TRUE)
#  ## sample k from p(k|gamma,x)
#  samplePi <- function(x,zkAfter){
#      ## allK <- which(transitionObj$Z$Z1$gamma$nk>0)
#      allK <- which(transitionObj$Z$Z1$gamma$nk>0) #all possible k
#      if(length(allK)==0) return(transitionObj$Z$Z1$gamma$newLabel)
#      ## p(k|pi) p(x|k) p(k_{2}|k)
#      logp <- dPosteriorPredictive(transitionObj$Z$Z1,x=allK,LOG = TRUE) +
#          vapply(allK,function(k){
#              dPosteriorPredictive(transitionObj$X[[k]],x=x,LOG = TRUE)+
#              dPosteriorPredictive(obj = transitionObj,z=zkAfter[1],k=zkAfter[2],j=k,LOG = FALSE)
#          },FUN.VALUE = numeric(1))
#  
#      logp <- exp(logp-max(logp))
#      allK[sample.int(length(allK),size = 1,prob = logp)]
#  }
#  ## sample z and k from p(z,k|gamma,alpha,x)
#  sampleZK <- function(kBefore,zkAfter,x){
#      ## p(z,k | k_{t-1}) p(x|k)
#      probs <- dAllIndicators(obj = transitionObj,j=kBefore,x=x)
#      ##
#      allK <- unique(probs$k)
#  
#      ## p(k_{t+1} | k)
#      p <- vapply(allK,function(k){
#          if(k==transitionObj$Z$Z1$gamma$newLabel)
#              dPosteriorPredictive(obj = transitionObj$Z$Z1,x=zkAfter[2],LOG = FALSE)
#          else
#              dPosteriorPredictive(obj = transitionObj,z=zkAfter[1],k=zkAfter[2],j=k,LOG = FALSE)
#      },FUN.VALUE = numeric(1))
#      idx <- sample.int(nrow(probs),size = 1,prob = p[match(probs$k,allK)]*probs$p)
#      c(z=probs$z[idx],k=probs$k[idx])
#  }
#  ## initialize states with forward assignment
#  for(i in 1L:Nsegs){
#      segStart <- breaks[i]+1L
#      segEnd <- breaks[i+1]
#      for(j in segStart:segEnd){
#          if(j==segStart){
#              k <- rPosteriorPredictive(obj = transitionObj$Z$Z1,n=1)
#              ## update initial and observation
#              posterior(obj = transitionObj,ss=ssx[[j]],ss1=k,ss2=1L,j=k)
#              zk[j,2] <- k
#          }else{
#              ## sample from the j(or k) th DP
#              zk1 <- rPosteriorPredictive(obj = transitionObj,n=1,j=zk[j-1,2],x=x[j,,drop=FALSE])
#              ## update transition and observation
#              posterior(obj = transitionObj,ss = ssx[[j]],ss1=zk1[2],ss2=zk1[1],j=zk[j-1,2])
#              ## write to book
#              zk[j,] <- zk1
#          }
#      }
#  }
#  ## Step2: main Gibbs loop---------------------------------------------
#  maxit <- 20
#  it <- 1
#  pb <- txtProgressBar(min = 0,max = maxit,style = 3)
#  while(it<=maxit){
#      ## for all segments
#      for(i in 1L:Nsegs){
#          segStart <- breaks[i]+1L
#          segEnd <- breaks[i+1]
#          for(j in segStart:segEnd){
#              ## discard previous z
#              if(j==segStart){
#                  ## discard 1 obs 1 initial and 1 transition
#                  posteriorDiscard(obj = transitionObj,ss=ssx[[j]],ss1=zk[j,2],ss2=1L,j=zk[j,2])
#                  posteriorDiscard(obj = transitionObj,ss=NULL, ss1=zk[j+1,2],ss2=zk[j+1,1],j=zk[j,2])
#                  ## sample new initial k
#                  k <- samplePi(x=x[j,,drop=FALSE],zkAfter = zk[j+1,,drop=FALSE])
#                  ## update prior with newly sampled k
#                  posterior(obj = transitionObj,ss=ssx[[j]],ss1=k,ss2=1L,j=k)
#                  posterior(obj = transitionObj,ss=NULL, ss1=zk[j+1,2],ss2=zk[j+1,1],j=k)
#                  ## write to book
#                  zk[j,2] <- k
#              }else if(j==segEnd){
#                  ## discard 1 obs and 1 transition
#                  posteriorDiscard(obj = transitionObj,ss=ssx[[j]],ss1=zk[j,2],ss2=zk[j,1],j=zk[j-1,2])
#                  ## sample new z k
#                  zk1 <- rPosteriorPredictive(obj = transitionObj,n=1,x=x[j,,drop=FALSE],j=zk[j-1,2])
#                  ## update prior with newly sampled z
#                  posterior(obj = transitionObj,ss=ssx[[j]],ss1=zk1[2],ss2=zk1[1],j=zk[j-1,2])
#                  ## write to book
#                  zk[j,] <- zk1
#              }else{
#                  ## discard 2 transitions and 1 obs
#                  posteriorDiscard(obj = transitionObj,ss=ssx[[j]],ss1=zk[j,2],ss2=zk[j,1],j=zk[j-1,2])
#                  posteriorDiscard(obj = transitionObj,ss=NULL, ss1=zk[j+1,2],ss2=zk[j+1,1],j=zk[j,2])
#                  ## sample new z k
#                  zk1 <- sampleZK(kBefore = zk[j-1,2],zkAfter = zk[j+1,],x=x[j,,drop=FALSE])
#                  ## update prior with newly sampled z k
#                  posterior(obj = transitionObj,ss=ssx[[j]],ss1=zk1[2],ss2=zk1[1],j=zk[j-1,2])
#                  posterior(obj = transitionObj,ss=NULL, ss1=zk[j+1,2],ss2=zk[j+1,1],j=zk1[2])
#                  ## write to book
#                  zk[j,] <- zk1
#              }
#          }
#      }
#      it <- it+1
#      setTxtProgressBar(pb,it)
#  }
#  allK <- which(transitionObj$Z$Z1$gamma$nk>0)
#  ## print the MAP estimates of the state transitions
#  for(k in allK) print(vapply(split(MAP(transitionObj$Z$Z2[[k]]),f=transitionObj$Z$Z12map[[k]]),sum,numeric(1)))
#  ## print the MAP estimates of the observation parameters
#  for(k in allK) print(MAP(transitionObj$X[[k]]))
#  ## print the MAP esitmate of the initial state distribution
#  round(MAP(transitionObj$Z$Z1)[allK],2)
#  

