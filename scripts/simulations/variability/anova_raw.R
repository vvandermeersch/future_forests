library(matrixStats)
library(zoo)

# number of SSPs/GCMs/SDMs
Ngcm <- length(gcms)
Nssp <- length(scenarios)
Nsdm <- length(calibrations)

# define temporal baseline
first_year <- 1970 # 1970 ?
last_year <- 2100
baseline <- 1970:1950

yrstot <- length(first_year:last_year)+(first_year-1900)-1
sim_fitrange <- (first_year-1900):yrstot

simulations$y <- simulations$smy + simulations$res

simulations2 <- simulations %>%
  group_by(cal, gcm, ssp) %>% 
  mutate(y = rollapply(y,21,mean,align='center',fill=NA)) %>%
  ungroup %>% na.omit()

anova_ss <- data.frame()
for(yr in min(simulations2$year):max(simulations2$year)){
  print(yr)
  
  linfit <- lm(I(y*1e10) ~ gcm + ssp + cal + 
                 gcm:ssp + gcm:cal + ssp:cal, 
               data = simulations2 %>% dplyr::filter(year == yr))
  anova <- car::Anova(linfit, type = 2)
  
  anova_ss <- rbind(anova_ss,
                    t(data.frame(append(yr, anova$`Sum Sq`))))
}
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("year", 
                         "gcm", "ssp", "sdm", 
                         "gcm.ssp", "gcm.sdm", "ssp.sdm",
                         "residuals")

anova_ss$tot <- anova_ss$gcm + anova_ss$ssp + anova_ss$sdm + anova_ss$gcm.ssp + anova_ss$gcm.sdm + anova_ss$ssp.sdm + anova_ss$residuals


ss_intvar <- simulations %>%
  mutate(intvar = res^2) %>% 
  group_by(year) %>%
  reframe(intvar = sum(intvar))


anova_ss <- anova_ss %>%
  inner_join(ss_intvar)


anova_ss$tot <- anova_ss$tot + anova_ss$intvar

plot(1900+c(100:195),anova_ss$gcm / anova_ss$tot,
     type="n",col="blue",xlim=c(2000,2100),ylim=c(0,1),xlab="Year",ylab="Variance Fraction [%]",
     frame.plot=FALSE,axes=FALSE)
axis(side=1,at=seq(2000,2100,by=10))
axis(side=2,at=seq(0,1,by=0.1),labels=seq(0,100,by=10))

plotline1 <- anova_ss$gcm / anova_ss$tot  
lines(1900+c(100:195), plotline1, col="#285c91")
polygon(c(1900+c(100:195),rev(1900+c(100:195))),c(plotline1,rep(0,length(c(100:195)))),
        col="#285c91",border=NA)

plotline2 <- (anova_ss$gcm + anova_ss$ssp) / anova_ss$tot 
lines((1900+c(100:195)), plotline2, col="seagreen")
polygon(c((1900+c(100:195)),rev(1900+c(100:195))),c(plotline2,rev(plotline1)),col="seagreen",border=NA)

plotline3 <- (anova_ss$gcm + anova_ss$ssp + anova_ss$gcm.ssp) / anova_ss$tot 
lines((1900+c(100:195)), plotline3, col="#00b9a4")
polygon(c((1900+c(100:195)),rev(1900+c(100:195))),c(plotline3,rev(plotline2)),col="#00b9a4",border=NA)

plotline4 <- (anova_ss$gcm + anova_ss$ssp + anova_ss$gcm.ssp + anova_ss$sdm) / anova_ss$tot 
lines((1900+c(100:195)), plotline4, col="purple")
polygon(c((1900+c(100:195)),rev(1900+c(100:195))),c(plotline4,rev(plotline3)),col="purple",border=NA)

plotline5 <- (anova_ss$gcm + anova_ss$ssp + anova_ss$gcm.ssp + anova_ss$sdm + anova_ss$gcm.sdm + anova_ss$ssp.sdm) / anova_ss$tot 
lines((1900+c(100:195)), plotline5, col="violet")
polygon(c((1900+c(100:195)),rev(1900+c(100:195))),c(plotline5,rev(plotline4)),col="violet",border=NA)

plotline6 <- (anova_ss$gcm + anova_ss$ssp + anova_ss$gcm.ssp + anova_ss$sdm + anova_ss$gcm.sdm + anova_ss$ssp.sd + anova_ss$residuals) / anova_ss$tot 
lines((1900+c(100:195)), plotline6, col="orange")
polygon(c((1900+c(100:195)),rev(1900+c(100:195))),c(plotline6,rev(plotline5)),col="orange",border=NA)


simulations %>%
  dplyr::filter(year %in% c(2000:2100)) %>%
  dplyr::select("year", "smy") %>%
  group_by(year) %>%
  reframe(y = mean(smy)) %>%
  left_join(anova_ss) %>% 
  mutate(gcm = sqrt(gcm), ssp = sqrt(ssp), sdm = sqrt(sdm), residuals = sqrt(residuals),
         f = (gcm+ssp+sdm+residuals)/sqrt(tot)) %>%
  ggplot(aes(x = year, y = y)) +
  geom_ribbon(aes(ymin = y - (1.64*(residuals+gcm+ssp+sdm)/f),
                  ymax = y + (1.64*(residuals+gcm+ssp+sdm)/f)),
              fill = "purple") +
  geom_ribbon(aes(ymin = y - (1.64*(residuals+gcm+ssp)/f),
                  ymax = y + (1.64*(residuals+gcm+ssp)/f)),
              fill = "seagreen") +
  geom_ribbon(aes(ymin = y - (1.64*(residuals+gcm)/f),
                  ymax = y + (1.64*(residuals+gcm)/f)),
              fill = "blue") +
  geom_ribbon(aes(ymin = y - (1.64*(residuals)/f),
                  ymax = y + (1.64*(residuals)/f)),
              fill = "orange") +
  geom_line(alpha = 1, linewidth = 0.5, linetype = "dashed") +
  theme_minimal() + xlim(c(2000,2100)) +
  ylab("Change in fitness (%)") +
  coord_cartesian(ylim = c(-100, 50), expand = FALSE)



simulations2 %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(y), int = sd(y)) %>%
  left_join(anova_ss, by = join_by(year)) %>%
  ggplot(aes(x = year, y = meany)) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals)/tot),
              fill = "orange") +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "purple") +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "violet") +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp)/tot),
              fill = "seagreen") +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+gcm.ssp)/tot),
              fill = "#00b9a4") +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm)/tot,
                  ymax = meany + (1.645*int)*(gcm)/tot),
              fill = "blue")+
  geom_line(alpha = 1, linewidth = 0.5, linetype = "dashed")+
  coord_cartesian(xlim = c(1990, 2090), expand = FALSE) +
  theme_bw()


simulations2 %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(y), int = sd(y)) %>%
  left_join(anova_ss) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals)/tot),
              fill = "orange") +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "purple") +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "violet") +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+gcm.ssp)/tot),
              fill = "seagreen") +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+gcm.ssp)/tot),
              fill = "#00b9a4") +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm)/tot),
              fill = "blue") +
  coord_cartesian(ylim = c(0,1), xlim = c(1990, 2090), expand = FALSE) +
  theme_bw()
  
  
  

