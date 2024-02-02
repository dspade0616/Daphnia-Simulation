install.packages("TSA")
install.packages("SMFI5")
install.packages("tseries")
install.packages("MARSS")
install.packages("forecast")
install.packages("astsa")
install.packages("animation")
install.packages("ggplot2")
install.packages("plot3D")
install.packages("rgl")
install.packages("gganimate")
install.packages("MKpower")
install.packages("spgs")
install.packages("snpar")
install.packages("EnvStats")
install.packages("gapminder")
install.packages("plotly")
install.packages("dplyr")
library(TSA)
library(MARSS)
library(generics)
library(forecast)
library(tseries)
library(astsa)
library(SMFI5)
library(animation)
library(ggplot2)
library(plot3D)
library(rgl)
library(gganimate)
library(MKpower)
library(spgs)
library(snpar)
library(EnvStats)
library(gapminder)
library(plotly)
library(dplyr)

sim.3d.sph <- function(n, model.r, mean.r, sd.r,  model.theta, mean.theta, sd.theta, model.phi, mean.phi, sd.phi){
	log.r <- mean.r + arima.sim(n=n, model=model.r, sd=sd.r)
	r.sim <- exp(log.r)
	for(i in 1:length(r.sim)){
		if(r.sim[i] >= 100){
			r.sim[i] <- runif(1, 0, 100)
		}
	}
	theta <- mean.theta + arima.sim(n=n, model=model.theta, sd=sd.theta)
	theta <- pi/180*theta
	phi <- mean.phi + arima.sim(n=n, model= model.phi, sd=sd.phi)
	phi <- pi/180*phi
	############Converting Back to Cartesian#########
	x.diff <-  r.sim*sin(phi)*cos(theta)
	y.diff <- r.sim*sin(phi)*sin(theta)
	z.diff <- r.sim*cos(phi)
	x.diff <- x.diff-mean(x.diff)
	y.diff <- y.diff-mean(y.diff)
	z.diff <- z.diff-mean(z.diff)
	x.start <- sample(-1000:1000, size=1)
	y.start <- sample(-1000:1000, size=1)
	z.start <- sample(-1000:1000, size=1)
	#x.start <- t[1,1]
	#y.start <- t[1,2]
	#z.start <- t[1,3]
	x <- c(x.start, rep(0, n))
	y <- c(y.start, rep(0, n))
	z <- c(z.start, rep(0, n))
	for(i in 1:n){
		x[i+1] <- x[i] + x.diff[i]
		y[i+1] <- y[i] + y.diff[i]
		z[i+1] <- z[i] + z.diff[i]
	}
	XYZ <- cbind(x,y,z)
	return(XYZ)
}

n <-  123
sd.r <- 2
mean.r <- 5
model.r <- list(order=c(1,0,0), ar=0.3)
mean.theta <- 5
sd.theta <- 50
model.theta <- list(order=c(0,0,2), ma=c(-0.4, 0.8))
mean.phi <-  12
sd.phi <- 25
model.phi <- list(order=c(0,0,2), ma=c(-0.1,  -0.4))
xyz <- sim.3d.sph(n=n, model.r=model.r, mean.r=mean.r, sd.r=sd.r, model.theta=model.theta, mean.theta=mean.theta, sd.theta=sd.theta, model.phi=model.phi, mean.phi=mean.phi, sd.phi=sd.phi)
