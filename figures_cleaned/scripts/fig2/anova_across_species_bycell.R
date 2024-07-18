
#---------------------------#
# Run ANOVA on cell outputs #
#---------------------------#

options(future.globals.maxSize= 1050*1024^2)

plan(multisession, workers = ncores)
anova_ss <- foreach(i = unique(simulations$id_cell), .combine=rbind) %dofuture% {
  
  data <- simulations %>% dplyr::filter(id_cell == i)
  
  linfit <- lm(mean ~ gcm + ssp + method + species +
                 gcm:ssp + gcm:method + ssp:method + species:method + species:gcm + species:ssp,
               data = data)
  
  # linfit <- lm(mean ~ gcm + ssp + method +
  #                gcm:ssp + gcm:method + ssp:method, 
  #              data = data)
  
  anova <- car::Anova(linfit, type = 2)
  
  t(data.frame(append(c(i, unique(data$x), unique(data$y)), anova$`Sum Sq`)))
  
} %>% as.data.frame()
plan(sequential);gc()
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("id","x", "y",
                         "gcm", "ssp", "sdm", "species",
                         "gcm.ssp", "gcm.sdm", "ssp.sdm", 
                         "species.sdm", "species.gcm", "species.ssp",
                         "residuals")

# Total uncertainty
anova_ss$tot <- anova_ss$gcm + anova_ss$ssp + anova_ss$sdm + anova_ss$species + 
  anova_ss$gcm.ssp + anova_ss$gcm.sdm + anova_ss$ssp.sdm + 
  anova_ss$species.sdm + anova_ss$species.gcm + anova_ss$species.ssp +
  anova_ss$residuals

anova_ss$sdm.climate <- anova_ss$ssp.sdm + anova_ss$gcm.sdm
anova_ss$climate <- anova_ss$ssp + anova_ss$gcm + anova_ss$gcm.ssp
anova_ss$species.all <- anova_ss$species.gcm + anova_ss$species.sdm + anova_ss$species.ssp

cols <- c("climate", "species", "sdm",
          "sdm.climate", "species.all",
          "residuals")

anova_ss$typemax1 <- apply(anova_ss[,cols], 1, FUN = function(x) 
  names(which(x == sort(x, decreasing = TRUE)[1])))
anova_ss$typemax2 <- apply(anova_ss[,cols], 1, FUN = function(x) 
  names(which(x == sort(x, decreasing = TRUE)[2])))









