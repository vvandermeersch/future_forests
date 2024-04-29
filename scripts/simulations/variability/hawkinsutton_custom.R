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

simulations$ncl <- as.numeric(simulations$ncl)

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
      
      # fit 4th order polynomial to smooth "natural" variability
      fit <- lm(y~poly(x,4,raw=TRUE))
      smy <- predict(fit,data.frame(x=sim_fitrange))
      # plot(y~x) ; lines(smy~x)
      simulations[simulations$calibration == ci &
                    simulations$gcm == mi &
                    simulations$scenario == si, "smy"] <- smy
     
      # residuals
      simulations[simulations$calibration == ci &
                    simulations$gcm == mi &
                    simulations$scenario == si, "res"] <- y - smy

      # compute baseline average
      ref <- simulations %>%
        dplyr::filter(calibration == ci, gcm == mi, scenario == si, year %in% baseline)
      ref <- mean(as.numeric(ref$ncl))
      simulations[simulations$calibration == ci &
                    simulations$gcm == mi &
                    simulations$scenario == si, "ref"] <- ref

    }
    
  }
  
}

# intvar <- simulations %>%
#   dplyr::filter(year > max(baseline)) %>%
#   group_by(calibration, gcm, scenario) %>%
#   reframe(intvar = var(res)) %>%
#   summarize(intvar = mean(intvar)) # average over all simulations (gcm + ssp + sdm)

intvar <- simulations %>%
  dplyr::filter(year > max(baseline)) %>%
  group_by(calibration, gcm, scenario) %>% # processing each simulation (gcm + ssp + sdm)
  reframe(res = rollapply(res,10,mean,align='center',fill=NA)) %>% # decadal mean residuals (HS09 approach)
  na.omit() %>% 
  group_by(calibration, gcm, scenario) %>%
  reframe(intvar = var(res)) %>%
  summarize(intvar = mean(intvar)) # average over all simulations (gcm + ssp + sdm)

intvar <- simulations %>%
  group_by(calibration, gcm, scenario) %>% # processing each simulation (gcm + ssp + sdm)
  reframe(
    year = year,
    intvar = rollapply(res,11,var,align='center',fill=NA)) %>% # centered rolling 11-year variance
  na.omit() %>%
  group_by(year) %>%
  summarize(intvar = mean(intvar)) # average over all simulations (gcm + ssp + sdm), by year


unc_comp <- cbind(
  simulations %>%
    group_by(gcm, year) %>%
    summarize(av = mean(smy-ref)) %>%
    group_by(year) %>% 
    summarize(gcm = sd(av)^2),
  simulations %>%
    group_by(scenario, year) %>%
    summarize(av = mean(smy-ref)) %>%
    group_by(year) %>% 
    summarize(ssp = sd(av)^2) %>% select(-year),
  simulations %>%
    group_by(calibration, year) %>%
    summarize(av = mean(smy-ref)) %>%
    group_by(year) %>% 
    summarize(sdm = sd(av)^2) %>% select(-year)
)

# unc_comp$intvar <- intvar$intvar
unc_comp <- inner_join(unc_comp, intvar)

unc_comp$tot <- unc_comp$gcm + unc_comp$ssp + unc_comp$sdm + unc_comp$intvar

library(zoo)
unc_comp_dec <- unc_comp %>%
  arrange(year) %>%
  reframe(
    year = year,
    gcm = rollapply(gcm,10,mean,align='center',fill=NA),
    ssp = rollapply(ssp,10,mean,align='center',fill=NA),
    sdm = rollapply(sdm,10,mean,align='center',fill=NA),
    tot = gcm + ssp + sdm) %>%
  na.omit()


data_to_plot <- unc_comp %>% dplyr::filter(year %in% c(2000:2100))

plot(1900+c(100:193),data_to_plot$gcm / data_to_plot$tot,
     type="n",col="blue",xlim=c(2000,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2000,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

plotline1 <- data_to_plot$gcm / data_to_plot$tot
lines(1900+c(100:193), plotline1, col="blue")
polygon(c(1900+c(100:193),rev(1900+c(100:193))),c(plotline1,rep(0,length(c(100:193)))),
        col="blue",border=NA)

plotline2 <- (data_to_plot$gcm + data_to_plot$ssp) / data_to_plot$tot
lines((1900+c(100:193)), plotline2, col="seagreen")
polygon(c((1900+c(100:193)),rev(1900+c(100:193))),c(plotline1,rev(plotline2)),col="seagreen",border=NA)

plotline3 <- (data_to_plot$gcm + data_to_plot$ssp + data_to_plot$cal) / data_to_plot$tot
lines((1900+c(100:193)), plotline3, col="purple")
polygon(c((1900+c(100:193)),rev(1900+c(100:193))),c(plotline3,rev(plotline2)),col="purple",border=NA)

plotline4 <- (data_to_plot$gcm + data_to_plot$ssp + data_to_plot$cal + data_to_plot$intvar) / data_to_plot$tot
lines((1900+c(100:193)), plotline4, col="orange")
polygon(c((1900+c(100:193)),rev(1900+c(100:193))),c(plotline4,rev(plotline3)),col="orange",border=NA)


ggplot(data = simulations, aes(x = year, y = smy-ref, 
                               col = ssp, group = paste0(gcm, ssp, cal))) +
  geom_line(alpha = 0.3, linewidth = 0.2) +
  stat_summary(aes(group = ssp), color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(aes(group = ssp), fun.y=mean, geom="line", linewidth = 0.7, linetype = "longdash") + 
  theme_minimal() + xlim(c(1970,2100)) +
  ylab("Fagus cells occupied") +
  scale_color_manual(values = cols)

ggplot() +
  geom_line(
    data = simulations %>% dplyr::filter(calibration != "expert"), 
    aes(x = year, y = ncl-ref, col = scenario, group = paste0(gcm, scenario, calibration)),
    alpha = 0.3, linewidth = 0.2) +
  stat_summary(
    data = simulations %>% dplyr::filter(calibration != "expert"),
    aes(x = year, y = ncl-ref, col = scenario, group = scenario), 
    color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(
    data = simulations %>% dplyr::filter(calibration != "expert"),
    aes(x = year, y = ncl-ref, col = scenario, group = scenario), 
    fun.y=mean, geom="line", linewidth = 0.7)  + 
  geom_line(
    data = simulations %>% dplyr::filter(calibration == "expert"), 
    aes(x = year, y = ncl-ref, col = scenario, group = paste0(gcm, scenario, calibration)),
    alpha = 0.3, linewidth = 0.2) +
  stat_summary(
    data = simulations %>% dplyr::filter(calibration == "expert"),
    aes(x = year, y = ncl-ref, col = scenario, group = scenario), 
    color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(
    data = simulations %>% dplyr::filter(calibration == "expert"),
    aes(x = year, y = ncl-ref, col = scenario, group = scenario), 
    fun.y=mean, geom="line", linewidth = 0.7, linetype = "longdash") +
  theme_minimal() + xlim(c(2030,2100)) +
  ylab("Fagus cells occupied")

