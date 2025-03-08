

anova_ss <- foreach(yr = 2090, .combine=rbind) %dofuture% {
  
  linfit <- lm(y ~ gcm + ssp + method + 
                 gcm:ssp + gcm:method + ssp:method, 
               data = simulations %>% dplyr::filter(year == yr))
  
  anova <- car::Anova(linfit, type = 2)
  
  t(data.frame(append(yr, anova$`Sum Sq`)))
  
} %>% as.data.frame()
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("year", 
                         "gcm", "ssp", "sdm", 
                         "gcm.ssp", "gcm.sdm", "ssp.sdm", 
                         "residuals")

# Total uncertainty
anova_ss$tot <- anova_ss$gcm + anova_ss$ssp + anova_ss$sdm +
  anova_ss$gcm.ssp + anova_ss$gcm.sdm + anova_ss$ssp.sdm + 
  anova_ss$residuals

anova_ss$species <- species
