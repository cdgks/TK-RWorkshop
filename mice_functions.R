mice_qqnorm <- function(models){
	par_old <- par(no.readonly=T) #save pre-change par settings
	par(mfrow=c(2,2))
	for(i in sample(1:length(models$analyses),size=4)){
		qqnorm(residuals(models$analyses[[i]]))
		qqline(residuals(models$analyses[[i]]), col = "steelblue", lwd = 2)
	}
	par(par_old) #return to old par settings
}
mice_res_yhat <- function(models){
	par_old <- par(no.readonly=T) #save pre-change par settings
	par(mfrow=c(2,2))
	for(i in sample(1:length(models$analyses),size=4)){
		plot(predict(models$analyses[[i]]),residuals(models$analyses[[i]]), ylab="residuals", xlab="yhat")
		abline(h=0,lty=2)
	}
	par(par_old) #return to old par settings
}
mice_res_x <- function(models,data,x="exposure"){
	par_old <- par(no.readonly=T) #save pre-change par settings
	par(mfrow=c(2,2))
	for(i in sample(1:length(models$analyses),size=4)){
		plot(complete(data,action=i)[,x],residuals(models$analyses[[i]]), xlab=paste0(x), ylab="residuals")
		abline(h=0,lty=2)
		lines(loess.smooth(y=residuals(models$analyses[[i]]),x=complete(data,action=i)[,x]), lwd=2,col="red")
	}
	par(par_old) #return to old par settings
}