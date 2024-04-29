library(matrixStats)
library(zoo)

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(calibrations)

# define temporal baseline
first_year <- 1970 # 1970 ?
last_year <- 2100
baseline <- 1970:2000

yrstot <- length(first_year:last_year)+(first_year-1900)-1
sim_fitrange <- (first_year-1900):yrstot

ndeg_poly <- 4

simulations <- data.frame()
for(c in 1:Nsdm){
  
  ci <- calibrations[c]
  
  suit <- rast(file.path(dir, "data", "processed", species, "suit", paste0(ci, ".tif"))) %>% 
    crop(country_map, mask = T)
  
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    indices <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% baseline)
    ref <- as.numeric(global(mean(subset(suit,indices)), mean, na.rm = TRUE))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      indices_hist <- which(names(suit) == paste0(mi, "_hist") & time(suit, format ="years") %in% c(1970:2010))
      indices <- c(indices_hist, which(names(suit) == paste0(mi, "_", si)))
      y <- unlist(global(subset(suit,indices), mean, na.rm = TRUE))
      x <- year(time(subset(suit,indices)))
      x <- x - (min(x)) + 1
      
      # change
      y <- (y-ref)/ref*100
      
      # fit 4th order polynomial to smooth "natural" variability
      fit <- lm(y~poly(x,ndeg_poly,raw=TRUE))
      smy <- predict(fit,data.frame(x=x))
      
      fit <- smooth.spline(x = x, y = y, df = 6)
      smy2 <- unlist(predict(fit,data.frame(x=x))$y)
      
      plot(y~x) ; lines(y~x) ; lines(smy~x, col = "red", lty = 2, lwd = 2) ; lines(smy2~x, col = "blue", lty = 2, lwd = 2)
      
      simulations <- rbind(
        simulations,
        data.frame(year = year(time(subset(suit,indices))),
                   cal = ci, gcm = mi, ssp = si, smy = smy, res = y-smy, ref = ref)
      )
      
    }
  }
}


data_to_plot <- unc_comp %>% dplyr::filter(year %in% c(2000:2100))

# Plot test
plot(1900+c(100:190),data_to_plot$gcm / data_to_plot$tot,
     type="n",col="blue",xlim=c(2000,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2000,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

plotline1 <- data_to_plot$gcm / data_to_plot$tot
lines(1900+c(100:190), plotline1, col="blue")
polygon(c(1900+c(100:190),rev(1900+c(100:190))),c(plotline1,rep(0,length(c(100:190)))),
        col="blue",border=NA)

plotline2 <- (data_to_plot$gcm + data_to_plot$ssp) / data_to_plot$tot
lines((1900+c(100:190)), plotline2, col="seagreen")
polygon(c((1900+c(100:190)),rev(1900+c(100:190))),c(plotline1,rev(plotline2)),col="seagreen",border=NA)

plotline3 <- (data_to_plot$gcm + data_to_plot$ssp + data_to_plot$cal) / data_to_plot$tot
lines((1900+c(100:190)), plotline3, col="purple")
polygon(c((1900+c(100:190)),rev(1900+c(100:190))),c(plotline3,rev(plotline2)),col="purple",border=NA)

plotline4 <- (data_to_plot$gcm + data_to_plot$ssp + data_to_plot$cal + data_to_plot$intvar) / data_to_plot$tot
lines((1900+c(100:190)), plotline4, col="orange")
polygon(c((1900+c(100:190)),rev(1900+c(100:190))),c(plotline4,rev(plotline3)),col="orange",border=NA)


ggplot(data = simulations, aes(x = year, y = smy, 
                               col = ssp, group = paste0(gcm, ssp, cal))) +
  geom_line(alpha = 0.3, linewidth = 0.2) +
  stat_summary(aes(group = ssp), color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(aes(group = ssp), fun.y=mean, geom="line", linewidth = 0.7, linetype = "longdash") + 
  theme_minimal() + xlim(c(2000,2090)) +
  ylab("Fagus cells occupied") +
  scale_color_manual(values = cols)


ggplot(data = simulations, aes(x = year, y = smy, 
                               col = ssp, group = paste0(gcm, ssp, cal))) +
  geom_point(aes(y = smy + res), alpha = 0.1, size = 0.1) +
  geom_line(alpha = 0.3, linewidth = 0.2) +
  stat_summary(aes(group = ssp), color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(aes(group = ssp), fun.y=mean, geom="line", linewidth = 0.7, linetype = "longdash") + 
  theme_minimal() + xlim(c(2000,2100)) +
  ylab("Fagus cells occupied") +
  scale_color_manual(values = cols)
