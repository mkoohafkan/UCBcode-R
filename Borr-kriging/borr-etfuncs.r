#######################################################################
# name: borr-etfuncs.r
# author: Michael Koohafkan
# purpose: function definitions for a variety of ET models
#######################################################################
#
# TODO: check equations for correct units, 
# RED FLAGS: Copais
# Hargreaves gives monthly
# Turc gives daily
# Copais gives what?

hargreaves <- function(tmin, tmax, doy){
	# valid for monthly data only
	# tmin = vector of minimum temps (degrees C)
	# tmax = vector of maximum temps (degrees C)
	# doy = day of year
	# outputs EP in mm/day
	
	# calibration coefficients
	a <- 0
	b <- 1
	c <- 2*pi*doy/365
	# extraterrestrial solar radiation in W/m^2
	Ra <- 0.408*0.0864*1367*((1.00011 + 0.034221*cos(c) + 0.001280*sin(c) + 
				              0.000719*cos(2*c) + 0.000077*sin(2*c) )^2)	
	return(a + b*0.0023*(0.5*(tmax + tmin) + 17.8)*Ra*sqrt(tmax - tmin) )
}

copais <- function(Tavg, RHavg, Rs){
	# Tavg = average temperature (degrees C)
	# RHavg = average relative humidity (percent, e.g. 63)
	# Rs is shortwave (aka global?) radiation (W/m^2)
	
	# outputs EP in mm/day
	
	# coefficients
	m1 <- 0.057
	m2 <- 0.277
	m3 <- 0.643
	m4 <- 0.0124
	c1 <-  0.6416 - 0.00784*RHavg + 0.372*Rs - 0.00264*Rs*RHavg
	c2 <- -0.0033 + 0.00812*Tavg + 0.101*Rs + 0.00584*Rs*Tavg
	return(m1 + m2*c2 + m3*c1 + m4*c1*c2)
}

ivanov <- function(Tavg, RHavg){
	# Tavg is in Celsius
	# RH is in percent (e.g. 63)
	
	# outputs EP in mm/day

	return(0.000036*((25 + Tavg)^2)*(100 - RHavg) )
}

turc <- function(Tavg, RHavg, Rg){
	# applicable only when EP exceeds 0.1 mm/day
	# Rg = global (aka shortwave?) radiation
	# Rg in MJ m-2/day
	
	# outputs EP in mm/day
	
	# coefficients
	a <- 0.31 
	b <- 2.094
	C <- rep(NA, length(Tavg))
	C[RHavg >= 50] <- 1
	C[RHavg < 50] <- 1 + (50 - RHavg[RHavg < 50])/70
	
	return(a*C*(Rg + b)*(Tavg/(Tavg + 15) ) )
}