library(matrixStats)


# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(calibrations)

# define temporal baseline
first_year <- 2020 # 1970 ?
last_year <- 2100
yrstot <- length(first_year:last_year)+(first_year-1900)-1
sim_fitrange <- (first_year-1900):yrstot



NC <- Ngcm*Nssp #climate component, i.e. GCM*SSP


yfit <- array(NA,dim=c(Ngcm,Nssp,Nsdm,yrstot))
climyield <- array(NA,dim=c(Ngcm,Nssp,Nsdm))
yieldanom <- array(NA,dim=c(Ngcm,Nssp,Nsdm,yrstot))

for(c in 1:Nsdm){
  
  ci <- calibrations[c]
  
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      y <- simulations %>%
        dplyr::filter(calibration == ci, gcm == mi, scenario == si)
      y <- as.numeric(y$ncl)
      x <- sim_fitrange
      
      
      #fit the model
      if (this_model <= 3) {
        model_fit <- loess(y~x,
                           span=model_list$span[model_list$model == this_model],
                           degree=model_list$degree[model_list$model == this_model])
      } else if (this_model == 4) {
        model_fit <- lm(y~poly(x,2,raw=TRUE))
      } else if (this_model == 5) {
        model_fit <- lm(y~poly(x,3,raw=TRUE))
      } else if (this_model == 6) {
        model_fit <- lm(y~poly(x,4,raw=TRUE))
      } else {
        stop("wrong model choice")
      }
      
      #predict values within range
      yfit_dummy <- predict(model_fit,data.frame(x=sim_fitrange))
      plot(x,y); lines(x,yfit_dummy)
      
      yfit[m,s,c,sim_fitrange] <- yfit_dummy
      climyield[m,s,c] <- mean(yfit[m,s,c,(first_year-1900):((first_year-1900)+20-1)])
      
      # 
      yieldanom[m,s,c,sim_fitrange] <- y
      
      # # #percentage deviation from predicted
      # # yieldanom_dummy <- 100 * ((y / climyield[m,s,c]) - 1)
      # yieldanom_dummy <- y - climyield[m,s,c]
      # yieldanom[m,s,c,sim_fitrange] <- yieldanom_dummy
      # # 
      # # # #percentage
      # # yfit_dummy <- 100 * ((yfit[m,s,c,] / climyield[m,s,c]) - 1)
      # yfit_dummy <- yfit[m,s,c,] - climyield[m,s,c]
      # yfit[m,s,c,] <- yfit_dummy
      
    }
    
  }
  
}

#pooled uncertainty components
clim_comp <- array(NA,dim=c(yrstot))
crop_comp <- array(NA,dim=c(yrstot))


#average predicted yield per dimension, used to calculate uncertainty components
ysdm <- array(NA,dim=c(Nsdm,yrstot)) #crop_model
ygcm <- array(NA,dim=c(Ngcm,yrstot)) #gcm
yssp <- array(NA,dim=c(Nssp,yrstot)) #rcp

#uncertainty components
sdm_comp <- array(NA,dim=c(yrstot))
gcm_comp <- array(NA,dim=c(yrstot))
ssp_comp <- array(NA,dim=c(yrstot))

yfitcomb <- array(NA,dim=c(NC,Nsdm,yrstot)) #signal

yfitcomb[,,] <- array(yfit[,,,],dim=c(NC,Nsdm,yrstot))

yfitcomb[,,] <- array(yfit[,,,],dim=c(NC,Nsdm,yrstot))

clim_comp[] <- matrixStats::colSds(apply(yfitcomb,c(1,3),mean))

crop_comp[] <- matrixStats::colSds(apply(yfitcomb,c(2,3),mean))


#uncertainty: first take average of all other dimensions but the one analysed and time
for (i in 1:Ngcm) {ygcm[i,] <- colMeans(array(yfit[i,,,],c(Nsdm*Nssp,yrstot)))} #GCM
for (i in 1:Nssp) {yssp[i,] <- colMeans(array(yfit[,i,,],c(Nsdm*Ngcm,yrstot)),na.rm=TRUE)} #RCP
for (i in 1:Nsdm) {ysdm[i,] <- colMeans(array(yfit[,,i,],c(NC,yrstot)))} #crop_model


gcm_comp[] <- colSds(ygcm[,])
ssp_comp[] <- colSds(yssp[,]) 
sdm_comp[] <- colSds(ysdm[,])






varifit <- array(NA,dim=c(yrstot))
varifitconst <- array(NA,dim=c(yrstot))

### calculate variability
variability <- array(NA,dim=c(Ngcm,Nssp,Nsdm,yrstot))
vres <- array(dim=c(Nsdm*NC,length(sim_fitrange)))
vresdec <- array(NA,dim=c(NC*Nsdm,(length(sim_fitrange)-9)))

#fill variability array
variability[,,,] <- yieldanom[,,,] - yfit[,,,]
vres[,] <- array(variability[,,,sim_fitrange], dim=c(Nsdm*NC, length(sim_fitrange)))


#decadal - mean over 10 year periods (moving average)
for (tt in 1:62) {vresdec[,tt] <- rowMeans(vres[,tt:(tt+9)])}


#fit decadal variability to time
x <- 10:(length(sim_fitrange)-9)
y <- colSds(vresdec[,10:(length(sim_fitrange)-9)])
fit1 <- lm(y~poly(x,1,raw=TRUE)) 

#predict trend of decadal variance in time
#the whole vector needs to be length(x)==yrstot
varifit[] <- predict(fit1,data.frame(x=(length(sim_fitrange) - yrstot + 1):(length(sim_fitrange))))
varifitconst[] <- rep(sqrt(mean(rowVars(vresdec[,]))), yrstot)




total_err <- array(NA,dim=c(yrstot)) #total uncertainty
total_err[] <- gcm_comp[]^2 + ssp_comp[]^2 + sdm_comp[]^2 + varifit[]^2

total_err_t <- total_err[sim_fitrange]

plot(1900+sim_fitrange,gcm_comp[sim_fitrange]^2 / total_err[sim_fitrange],
     type="n",col="blue",xlim=c(2025,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2025,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

plotline1 <- gcm_comp[sim_fitrange]^2 / total_err_t
lines(1900+sim_fitrange, plotline1, col="blue")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline1,rep(0,length(sim_fitrange))),
        col="blue",border=NA)

#ssp (but needs to be gcm+ssp since it's cumulative)
plotline2 <- (gcm_comp[sim_fitrange]^2 + ssp_comp[sim_fitrange]^2) / total_err_t
lines((1900+sim_fitrange), plotline2, col="seagreen")
polygon(c((1900+sim_fitrange),rev(1900+sim_fitrange)),c(plotline1,rev(plotline2)),col="seagreen",border=NA)

#variability (but needs to be gcm+ssp+variability since it's cumulative)
plotline3 <- (gcm_comp[sim_fitrange]^2 + ssp_comp[sim_fitrange]^2 + varifit[sim_fitrange]^2) / total_err_t
lines((1900+sim_fitrange), plotline3, col="orange")
polygon(c((1900+sim_fitrange),rev(1900+sim_fitrange)),c(plotline3,rev(plotline2)),col="orange",border=NA)

#total of all others, but could have done ok with just a square in the background
plotline4 <- (gcm_comp[sim_fitrange]^2 + ssp_comp[sim_fitrange]^2 + varifit[sim_fitrange]^2 + sdm_comp[sim_fitrange]^2) / total_err_t
lines(1900+sim_fitrange, plotline4, col="purple")
polygon(c(1900+sim_fitrange,rev(1900+sim_fitrange)),c(plotline4,rev(plotline3)),col="purple",border=NA)




plot(sim_fitrange+1900, sigfac * sqrt(total_err[sim_fitrange]), col="black", type="l", 
     lty=1,lwd=2,xlab="Year",ylab="Uncertainty [% yield]",xlim=c(2005,2098),ylim=c(0,2000),
     axes=F) #total
axis(side=1,at=seq(2005,2100,by=10))
axis(side=2,at=seq(0,2000,by=100),labels=seq(0,2000,by=100))
box()

lines(sim_fitrange+1900, sigfac * gcm_comp[sim_fitrange], col="blue", lwd=2) #gcm component
lines(sim_fitrange+1900, sigfac * ssp_comp[sim_fitrange], col="seagreen", lwd=2) #ssp component
lines(sim_fitrange+1900, sigfac * sdm_comp[sim_fitrange], col="purple", lwd=2) #ssp component
lines(sim_fitrange+1900, sigfac * varifit[sim_fitrange], col="orange", lwd=2) #variability
lines(sim_fitrange+1900, tot_mean[sim_fitrange], col="black", lty=2, lwd=2) #signal
grid(lwd=1)


