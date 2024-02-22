plot(1900+c(130:190),sqrt(anova_ss$ss_tot),
     type="n",col="blue",xlim=c(2030,2110),ylim=c(0,550),xlab="Year",ylab="Uncertainty",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2030,2090,by=10))
axis(side=2,at=seq(0,550,by=50))

plotline1 <- sqrt(anova_ss$ss_gcm)/40
lines(1900+c(130:190), plotline1, col="#285c91")
polygon(c(1900+c(130:190),rev(1900+c(130:190))),c(plotline1,rep(0,length(c(130:190)))),
        col="#285c91",border=NA)

plotline2 <- (sqrt(anova_ss$ss_gcm) + sqrt(anova_ss$ss_ssp))/40
lines((1900+c(130:190)), plotline2, col="seagreen")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline2,rev(plotline1)),col="seagreen",border=NA)

plotline3 <- (sqrt(anova_ss$ss_gcm) + sqrt(anova_ss$ss_ssp) + sqrt(anova_ss$ss_gcm.ssp))/40
lines((1900+c(130:190)), plotline3, col="#00b9a4")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline3,rev(plotline2)),col="#00b9a4",border=NA)

plotline4 <- (sqrt(anova_ss$ss_gcm) + sqrt(anova_ss$ss_ssp) + sqrt(anova_ss$ss_gcm.ssp) + sqrt(anova_ss$ss_sdm))/40
lines((1900+c(130:190)), plotline4, col="purple")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline4,rev(plotline3)),col="purple",border=NA)

plotline5 <- (sqrt(anova_ss$ss_gcm) + sqrt(anova_ss$ss_ssp) + 
                sqrt(anova_ss$ss_gcm.ssp) + sqrt(anova_ss$ss_sdm) + 
                sqrt(anova_ss$ss_gcm.sdm) + sqrt(anova_ss$ss_ssp.sdm))/40
lines((1900+c(130:190)), plotline5, col="violet")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline5,rev(plotline4)),col="violet",border=NA)

plotline6 <- (sqrt(anova_ss$ss_gcm) + sqrt(anova_ss$ss_ssp) + 
                sqrt(anova_ss$ss_gcm.ssp) + sqrt(anova_ss$ss_sdm) + 
                sqrt(anova_ss$ss_gcm.sdm) + sqrt(anova_ss$ss_ssp.sdm) + sqrt(anova_ss$ss_var))/40
lines((1900+c(130:190)), plotline6, col="orange")
polygon(c((1900+c(130:190)),rev(1900+c(130:190))),c(plotline6,rev(plotline5)),col="orange",border=NA)


# grid(lwd=1)
legend("right",c("GCM","SSP","GCM:SSP","SDM","SDM:climate", "Variability"),
       lty=c(1,1,1,1,1), lwd = 3, col=c("#285c91","seagreen","#00b9a4","purple","violet", "orange"), 
       bty="n", cex=1)
