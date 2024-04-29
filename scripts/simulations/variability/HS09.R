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
  
  suit <- rast(file.path(dir, "data", "processed", species, "bin", paste0(ci, ".tif"))) %>% 
    crop(country_map, mask = T)
  
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    indices <- which(names(suit) == paste0(mi, "_ref"))
    ref <- sapply(subset(suit,indices), function(i) ncell(i[i == 1]))
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      indices <- which(names(suit) == paste0(mi, "_", si))
      y <- (sapply(subset(suit,indices), function(i) ncell(i[i == 1]))-ref)/ref*100
      x <- year(time(subset(suit,indices)))
      x <- x - (min(x)) + 1
      
      # fit 4th order polynomial to smooth "natural" variability
      fit <- lm(y~poly(x,ndeg_poly,raw=TRUE))
      smy <- predict(fit,data.frame(x=x))
      
      # plot(y~x) ; lines(smy~x)
      
      simulations <- rbind(
        simulations,
        data.frame(year = year(time(subset(suit,indices))),
                   cal = ci, gcm = mi, ssp = si, smy = smy, res = y-smy, ref = ref)
      )
      
    }
  }
}

# INTERANNUAL VARIABILITY
intvar <- simulations %>%
  dplyr::filter(year > max(baseline)) %>%
  group_by(cal, gcm, ssp) %>% # processing each simulation (gcm + ssp + sdm)
  reframe(res = rollapply(res,10,mean,align='center',fill=NA)) %>% # decadal mean residuals (HS09 approach)
  na.omit() %>% 
  group_by(cal, gcm, ssp) %>%
  reframe(intvar = var(res)) %>%
  summarize(intvar = mean(intvar)) # average over all simulations (gcm + ssp + sdm)



# another possibility: the centered rolling 11-year variance of the residuals, averaged over all outputs (Lafferty et al.)
intvar <- simulations %>%
  # dplyr::filter(year > max(baseline)) %>%
  group_by(cal, gcm, ssp) %>% 
  reframe(year = year, intvar = rollapply(res,11,var,align='center',fill=NA)) %>%
  na.omit() %>% 
  group_by(year) %>% # IAvar evolve over time
  summarize(intvar = mean(intvar)) # averaged over all outputs

# another possibility ? 11-year mean residuals ?
intvar <- simulations %>%
  # dplyr::filter(year > max(baseline)) %>%
  group_by(cal, gcm, ssp) %>% 
  mutate(res = rollapply(res,11,mean,align='center',fill=NA)) %>% 
  reframe(year = year, intvar = rollapply(res,11,var,align='center',fill=NA)) %>%
  na.omit() %>% 
  group_by(year) %>% # IAvar evolve over time
  summarize(intvar = mean(intvar)) # averaged over all outputs


unc_comp <- cbind(
  simulations %>%
    group_by(gcm, year) %>%
    summarize(av = mean(smy)) %>%
    group_by(year) %>% 
    summarize(gcm = sd(av)^2),
  simulations %>%
    group_by(ssp, year) %>%
    summarize(av = mean(smy)) %>%
    group_by(year) %>% 
    summarize(ssp = sd(av)^2) %>% select(-year),
  simulations %>%
    group_by(cal, year) %>%
    summarize(av = mean(smy)) %>%
    group_by(year) %>% 
    summarize(cal = sd(av)^2) %>% select(-year)) %>%
  inner_join(intvar)
    


unc_comp$tot <- unc_comp$gcm + unc_comp$ssp + unc_comp$cal + unc_comp$intvar

data_to_plot <- unc_comp %>% dplyr::filter(year %in% c(2000:2100))


# Plot test
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


ggplot(data = simulations, aes(x = year, y = smy, 
                               col = ssp, group = paste0(gcm, ssp, cal))) +
  geom_line(alpha = 0.3, linewidth = 0.2) +
  stat_summary(aes(group = ssp), color = "white", fun.y=mean, geom="line", linewidth = 1.8) + 
  stat_summary(aes(group = ssp), fun.y=mean, geom="line", linewidth = 0.7, linetype = "longdash") + 
  theme_minimal() + xlim(c(1970,2100)) +
  ylab("Fagus cells occupied") +
  scale_color_manual(values = cols)


simulations %>%
  dplyr::filter(year %in% c(2000:2100)) %>%
  group_by(year) %>%
  reframe(y = mean(smy)) %>%
  left_join(data_to_plot, by = join_by(year)) %>%
  mutate(gcm = sqrt(gcm), ssp = sqrt(ssp), cal = sqrt(cal), intvar = sqrt(intvar),
         f = (gcm+ssp+cal+intvar)/sqrt(tot))%>%
  ggplot(aes(x = year, y = y)) +
  geom_ribbon(aes(ymin = y - (1.64*(intvar+gcm+ssp+cal)/f),
                  ymax = y + (1.64*(intvar+gcm+ssp+cal)/f)),
              fill = "purple") +
  geom_ribbon(aes(ymin = y - (1.64*(intvar+gcm+ssp)/f),
                  ymax = y + (1.64*(intvar+gcm+ssp)/f)),
              fill = "seagreen") +
  geom_ribbon(aes(ymin = y - (1.64*(intvar+gcm)/f),
                  ymax = y + (1.64*(intvar+gcm)/f)),
              fill = "blue") +
  geom_ribbon(aes(ymin = y - (1.64*(intvar)/f),
                  ymax = y + (1.64*(intvar)/f)),
              fill = "orange") +
  geom_line(alpha = 1, linewidth = 0.5, linetype = "dashed") +
  theme_minimal() + xlim(c(2000,2100)) +
  ylab("Change in fitness (%)") +
  coord_cartesian(ylim = c(-100, 50), expand = FALSE)




