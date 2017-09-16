### This script reproduces the wage~age chart, which illustates how 
### the mean of wage varies with age against the backdrop of the 
### sample data used.
attach(Wage)
start<-min(age)
end<-77             #The data goes to age 80, but there are gaps after 77
myvec<-rep(0,end-start+1)
for(i in start:end) {
	myvec[i-start+1]<-mean(wage[age==i])
}
mylistpoints<-list(x=age[age<=end],y=wage[age<=end])
mylistmeans<-list(x=start:end,y=myvec)
plot(mylistpoints,cex=.4,ylim=c(0,400))
par(new=T)
lines(mylistmeans,type="l",lwd=3)
par(new=F)
##For a smoother line
windows()
plot(mylistpoints,cex=.4,ylim=c(0,400))
par(new=T)
smoothingSpline = smooth.spline(start:end, myvec, spar=0.7)
lines(smoothingSpline,type="l",lwd=3)
par(new=F)
detach(Wage)
