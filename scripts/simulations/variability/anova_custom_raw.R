anova_ss <- data.frame()
for(y in 2025:2095){
  linfit <- lm((ncl-ref) ~ gcm + scenario + calibration + 
                 gcm:scenario + gcm:calibration + scenario:calibration, 
               data = simulations %>% dplyr::filter(year == y))
  anova <- car::Anova(linfit, type = 2)
  
  anova_ss <- rbind(anova_ss,
                    t(data.frame(append(y, anova$`Sum Sq`))))
  
  
}
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("year", 
                         "ss_gcm", "ss_ssp", "ss_sdm", 
                         "ss_gcm.ssp", "ss_gcm.sdm", "ss_ssp.sdm",
                         "ss_var")

# anova_ss <- anova_ss %>%
#   arrange(year) %>%
#   reframe(
#     year = year,
#     ss_gcm = rollapply(ss_gcm,10,mean,align='center',fill=NA),
#     ss_ssp = rollapply(ss_ssp,10,mean,align='center',fill=NA),
#     ss_sdm = rollapply(ss_sdm,10,mean,align='center',fill=NA),
#     ss_tot = ss_gcm + ss_ssp + ss_sdm) %>%
#   na.omit() 



# intvar <- simulations %>%
#   reframe(year = year,
#           ss = (ncl-smy)^2) %>%
#   group_by(year) %>%
#   summarise(ss_var = sum(ss))

# intvar <- simulations %>%
#   group_by(calibration, gcm, scenario) %>%
#   reframe(year = year, res = rollapply(res,11,mean,align='center',fill=NA)) %>%
#   na.omit() %>%
#   reframe(year = year,
#           ss = (res)^2) %>%
#   group_by(year) %>%
#   summarise(ss_var = sum(ss))

# totvar <- simulations %>%
#   group_by(year) %>%
#   summarise(ss_var = var((ncl-ref))*39) %>%
#   dplyr::filter(year %in% c(2030:2090))
# 
# totvar <- simulations %>%
#   group_by(year) %>%
#   summarise(ss_var = sum((ncl-ref)^2)) %>%
#   dplyr::filter(year %in% c(2030:2090))

# anova_ss <- inner_join(anova_ss, intvar)


# anova_ss$ss_tot/totvar$ss_var

anova_ss <- anova_ss %>%
  reframe(
    year = year,
    ss_gcm = rollapply(ss_gcm,11,mean,align='center',fill=NA),
    ss_ssp = rollapply(ss_ssp,11,mean,align='center',fill=NA),
    ss_sdm = rollapply(ss_sdm,11,mean,align='center',fill=NA),
    ss_gcm.ssp = rollapply(ss_gcm.ssp,11,mean,align='center',fill=NA),
    ss_gcm.sdm = rollapply(ss_gcm.sdm,11,mean,align='center',fill=NA),
    ss_ssp.sdm = rollapply(ss_ssp.sdm,11,mean,align='center',fill=NA),
    ss_var = rollapply(ss_var,11,mean,align='center',fill=NA)
  )

anova_ss <- anova_ss %>%
  dplyr::filter(year %in% c(2030:2090))

anova_ss$ss_tot <- anova_ss$ss_gcm + anova_ss$ss_ssp + anova_ss$ss_sdm + 
  anova_ss$ss_gcm.ssp + anova_ss$ss_gcm.sdm + anova_ss$ss_ssp.sdm + anova_ss$ss_var

plot(1900+c(130:190),anova_ss$ss_gcm / anova_ss$ss_tot,
     type="n",col="blue",xlim=c(2030,2090),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2030,2090,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

plotline1 <- anova_ss$ss_gcm / anova_ss$ss_tot  
lines(1900+c(130:190), plotline1, col="#285c91")
polygon(c(1900+c(130:190),rev(1900+c(130:190))),c(plotline1,rep(0,length(c(130:190)))),
        col="#285c91",border=NA)

plotline2 <- (anova_ss$ss_gcm + anova_ss$ss_ssp) / anova_ss$ss_tot 
lines((1900+c(130:190)), plotline2, col="seagreen")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline2,rev(plotline1)),col="seagreen",border=NA)

plotline3 <- (anova_ss$ss_gcm + anova_ss$ss_ssp + anova_ss$ss_gcm.ssp) / anova_ss$ss_tot 
lines((1900+c(130:190)), plotline3, col="#00b9a4")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline3,rev(plotline2)),col="#00b9a4",border=NA)

plotline4 <- (anova_ss$ss_gcm + anova_ss$ss_ssp + anova_ss$ss_gcm.ssp + anova_ss$ss_sdm) / anova_ss$ss_tot 
lines((1900+c(130:190)), plotline4, col="purple")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline4,rev(plotline3)),col="purple",border=NA)

plotline5 <- (anova_ss$ss_gcm + anova_ss$ss_ssp + anova_ss$ss_gcm.ssp + anova_ss$ss_sdm + anova_ss$ss_gcm.sdm + anova_ss$ss_ssp.sdm) / anova_ss$ss_tot 
lines((1900+c(130:190)), plotline5, col="violet")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline5,rev(plotline4)),col="violet",border=NA)

plotline6 <- (anova_ss$ss_gcm + anova_ss$ss_ssp + anova_ss$ss_gcm.ssp + anova_ss$ss_sdm + anova_ss$ss_gcm.sdm + anova_ss$ss_ssp.sd + anova_ss$ss_var) / anova_ss$ss_tot 
lines((1900+c(130:190)), plotline6, col="orange")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline6,rev(plotline5)),col="orange",border=NA)