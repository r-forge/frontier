## Code for conducting semiparametric stochastic frontier analysis
## 1. Fan, Li and Weersink (1996) (FLW)
flw.ll <- function(lmd=1,e){

	## Caculate sigma(lambda) as in equation (14), page 462
 	sc <- sqrt(2*lmd^2/pi/(1+lmd^2))
 	si <- sqrt(mean(e^2)/(1-sc^2)) ## This is (14)
 	
 	## Now calculate mean correction as in discussion 
 	## on page 462, left column prior to (10) 
 	mu <- si*sc
 	
 	## Now calculate bias adjusted residuals
 	ep <- e-mu
    
 	# log-likelihood
 	flw.ll <- -length(ep)*log(si)+sum(pnorm(-ep*lmd/si,log=T))-0.5*sum(ep^2)/si^2    
 	return(-flw.ll)

}

FLW <- function(y,x,regtype="lc",bw.sel="cv.ls"){
	
	## Step 1: Estimate conditional mean and obtain residuals
	
	if(bw.sel=="cv.ls" | bw.sel=="cv.aic"){
		bw <- npregbw(ydat=y,xdat=x,regtype=regtype,bwmethod=bw.sel)
	}else{
		bw <- npregbw(ydat=y,xdat=x,regtype=regtype,bandwidth.compute=F)
		bw$bw <- 1.06*apply(as.matrix(x),2,sd)*length(y)^(-1/(4+ncol(as.matrix(x))))
		
	}

	## Need to allow for users to pass own bandwidths directly
	## so tht we can also have constrained FLW.

	model <- npreg(bws=bw,tydat=y,txdat=x,residuals=T,gradients=T)
	
	## Extract residuals from model
	resid <- residuals(model)
	
	## Step 2: Pass resid to flw.ll to obtain estimates of lambda and sigma
	## if(skewness(resid)>0){resid<- -resid}
	
	sol <- mle2(flw.ll,start=list(lmd=1),data=list(e=resid))
	
	lmd <- as.numeric(coef(sol)[1])
	
	## Use estimate of lambda to calculate sigma^2
	sc <- sqrt(2*lmd^2/pi/(1+lmd^2))
 	sig.sq <- mean(resid^2)/(1-sc^2)
 	
 	## Now calculate sigma.u and sigma.v
 	sigma.v <- sqrt(sig.sq/(1+lmd^2))
	sigma.u <- sigma.v*lmd
	
	## Caluclate Bias Correction, see beneath (15) on page 463.
	mu <- sqrt(sig.sq)*sc

	return(list(mhat=(fitted(model)+mu),mprime=gradients(model),e=resid,
				sigma.sq=sig.sq,lambda=lmd,sigma.u=sigma.u,sigma.v=sigma.v,
				bw=bw$bw))

}
