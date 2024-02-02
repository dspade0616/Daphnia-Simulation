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

sim.2d <- function(n, model.angle, sd.angle, scale.length, model.length, sd.length){
	angles <- arima.sim(n=n, model=model.angle, sd=sd.angle)*pi/180
	#######Transform lengths using PIT#######
	unis <- runif(n, 0, 1)
	norms <- qnorm(unis)
	sim.norm <- zscore(arima.sim(n=n, model=model.length, sd=sd.length))
	norm.cdf <-  pnorm(sim.norm)
	lengths <- -scale.length*log(1-norm.cdf)
	diff.x <- lengths*cos(angles)
	diff.x <- diff.x-mean(diff.x)
	diff.y <- lengths*sin(angles)
	diff.y <- diff.y-mean(diff.y)
	x.start <- points[1,1]
	y.start <- points[1,2]
	x <- c(x.start, rep(0, n))
	y <- c(y.start, rep(0, n))
	for(i in 1:n){
		x[i+1] <- x[i] + diff.x[i]
		y[i+1] <- y[i] + diff.y[i]
	}
	XY <- cbind(x,y)
	return(XY)
}

##########Arguments to function################
n <-  123
model.angle <- list(order=c(0,0,2), ma=c(-1, 0.5))
sd.angle <- 75
scale.length <- 40
model.length <- list(order=c(1,0,0), ar=c(-0.9))
sd.length <-  sqrt(0.5)

daph <- sim.2d(n,  model.angle, sd.angle, scale.length, model.length, sd.length)
