
#-------------------------------#
# Run ANOVA on smoothed outputs #
#-------------------------------#

plan(multisession, workers = 10)
anova_ss <- foreach(yr = min(simulations$year):max(simulations$year), .combine=rbind) %dofuture% {
  
  linfit <- lm(y ~ gcm + ssp + method + species +
                 gcm:ssp + gcm:method + ssp:method + species:method + species:gcm + species:ssp, 
               data = simulations %>% dplyr::filter(year == yr))
  anova <- car::Anova(linfit, type = 2)
  
  t(data.frame(append(yr, anova$`Sum Sq`)))
  
} %>% as.data.frame()
plan(sequential);gc()
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("year", 
                         "gcm", "ssp", "sdm", "species",
                         "gcm.ssp", "gcm.sdm", "ssp.sdm", 
                         "species.sdm", "species.gcm", "species.ssp",
                         "residuals")

#compute some marginal effects for figure in Appendix
saved_fit <- foreach(yr = c(2050,2090), .combine=rbind) %dofuture% {
  
  linfit <- lm(y ~ gcm + ssp + method + species +
                 gcm:ssp + gcm:method + ssp:method + species:method + species:gcm + species:ssp, 
               data = simulations %>% dplyr::filter(year == yr))
  anova <- car::Anova(linfit, type = 2)
  
  pred <- ggeffects::predict_response(linfit, terms = c("species", "method"), margin = "marginalmeans")
  pred$ecoregion <- ecoregion
  pred$year <- yr
  
  pred
  
} %>% as.data.frame()
saved_fit_speciesSDM <- rbind(saved_fit_speciesSDM, saved_fit)

# Interannual variability (can evolve between years)
# sum of squares of the difference between raw outputs and the smoothed ones
# ss_intvar <- simulations %>%
#   mutate(intvar = (y-smy)^2) %>% 
#   group_by(year) %>%
#   reframe(intvar = sum(intvar))

# Join with ANOVA results
# anova_ss <- anova_ss %>% 
#   left_join(ss_intvar, by = join_by(year))

# Total uncertainty
anova_ss$tot <- anova_ss$gcm + anova_ss$ssp + anova_ss$sdm + anova_ss$species + 
  anova_ss$gcm.ssp + anova_ss$gcm.sdm + anova_ss$ssp.sdm + 
  anova_ss$species.sdm + anova_ss$species.gcm + anova_ss$species.ssp +
  anova_ss$residuals


# Uncertainty fractional decomposition
cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm:gcm + sdm:ssp" = "#ddb166",
          "ssp" = "#D98B65", "gcm:ssp" = "#D98B65", "gcm" = "#D98B65", "species" = "#6B95B2", "species:all" = "#8E85C4")
unc_decomp <- simulations %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(y)) %>%
  left_join(anova_ss) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species+species.ssp+species.gcm+species.sdm+residuals)/tot*100, fill = "residuals")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species+species.ssp+species.gcm+species.sdm)/tot*100, fill = "species:all")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species)/tot*100, fill = "species")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot*100, fill = "sdm")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot*100, fill = "sdm:gcm + sdm:ssp")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+gcm.ssp)/tot*100, fill = "ssp")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+gcm.ssp)/tot*100, fill = "gcm:ssp")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm)/tot*100, fill = "gcm")) +
  coord_cartesian(ylim = c(0,100), xlim = c(2000, 2090), expand = FALSE) +
  theme_bw() + scale_y_continuous(position = "right") +
  labs(y = "Fraction of total variance (%)") +
  scale_x_continuous(breaks = seq(2000, 2090, 15)) +
  theme(
    plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 11),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    axis.text = element_text(size = 7), axis.title.y = element_text(size = 8),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt')) +
  scale_fill_manual(values=cols, breaks = c("gcm", "gcm:ssp", "ssp",  "sdm", "species", "sdm:gcm + sdm:ssp", "residuals")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Decomposition of species interaction (species:all)
cols <- c("species.ssp" = "#474262", "species.gcm" = "#8E85C4", "species.sdm" = "#d1cee7")
unc_decomp_species <- simulations %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(y)) %>%
  left_join(anova_ss) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp+species.gcm+species.sdm)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.sdm")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp+species.gcm)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.gcm")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.ssp")) +
  coord_cartesian(ylim = c(0,100), xlim = c(2000, 2090), expand = FALSE) +
  theme_bw() + scale_y_continuous(position = "left") +
  labs(y = "Fraction of species interaction variance (%)") +
  scale_x_continuous(breaks = seq(2000, 2090, 15)) +
  theme(
    plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 11),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    axis.text = element_text(size = 7), axis.title.y = element_text(size = 8),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt')) +
  scale_fill_manual(values=cols, 
                    breaks = c("species.ssp", "species.gcm", "species.sdm"),
                    labels = c("Species - SSP", "Species - GCM", "Species - SDM")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


# Trends and uncertainty sources
cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm:gcm + sdm:ssp" = "#ddb166",
          "climate" = "#D98B65", "species" = "#6B95B2", "species:all" = "#8E85C4") 
trend_unc <- simulations %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(ssp, gcm, method, species, year) %>%
  summarise(y = mean(y)) %>%
  ungroup() %>%
  group_by(year) %>%
  reframe(meany = mean(y), int = sd(y)) %>%
  left_join(anova_ss, by = join_by(year)) %>%
  ggplot(aes(x = year, y = meany)) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals+species+species.sdm+species.gcm+species.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals+species+species.sdm+species.gcm+species.ssp)/tot,
                  fill = "residuals"), alpha = 1) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species+species.sdm+species.gcm+species.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species+species.sdm+species.gcm+species.ssp)/tot,
                  fill = "species:all"), alpha = 1) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+species)/tot,
                  fill = "species"), alpha = 1) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  fill = "sdm"), alpha = 1) + 
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  fill = "sdm:gcm + sdm:ssp"), alpha = 1) +  
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp)/tot,
                  fill = "climate"), alpha = 1) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+gcm.ssp)/tot,
                  fill = "climate"), alpha = 1) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm)/tot,
                  ymax = meany + (1.645*int)*(gcm)/tot,
                  fill = "climate"), alpha = 1)+
  geom_line(linewidth = 0.7, linetype = "solid", color = "white", alpha = 0.6)+
  geom_line(alpha = 1, linewidth = 0.3, linetype = "dashed")+
  coord_cartesian(xlim = c(2000, 2090), ylim = c(-0.5,0.75), expand = FALSE) +
  labs(y = "Projected change in suitability\nrelative to 1970-2000") +
  scale_x_continuous(breaks = seq(2000, 2090, 15), labels = c("2000", "", "2030", "", "2060", "", "2090")) +
  theme_bw() +
  theme(
    legend.position = "none", legend.title = element_text(size = 6.5), legend.text = element_text(size = 6.5, margin = margin(l=1)),
    axis.ticks = element_line(colour = "grey30", linewidth=0.3),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    plot.margin = margin(t = 5.5, b = 5.5, r = 0, l = 5.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 6.5), axis.text.y = element_text(size = 6.5),
    axis.title.y = element_text(size = 7.5),
    legend.key.size = unit(7, 'pt'),
    legend.key.spacing.x = unit(3.5, 'pt'),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_fill_manual(name = "Source of uncertainty:", values=cols, 
                    breaks = c("climate",  "sdm:gcm + sdm:ssp", "sdm", "species", "species:all", "residuals + int. ann. var."),
                    labels = c("Climate (SSP, GCM)", "Climate - SDM", "SDM", "Species", "Species - climate/SDM", "Residuals")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


saved_data_all <-  rbind(saved_data_all, 
                         simulations %>%
                           dplyr::filter(year %in% c(1990:2100)) %>%
                           group_by(ssp, gcm, method, species, year) %>%
                           summarise(y = mean(y)) %>%
                           ungroup() %>%
                           group_by(year) %>%
                           reframe(meany = mean(y), int = sd(y)) %>%
                           left_join(anova_ss, by = join_by(year)) %>% 
                           mutate(ecoregion = ecoregion))

saved_data_species <-  rbind(saved_data_species, 
                             simulations %>%
                               dplyr::filter(year %in% c(1990:2100)) %>%
                               group_by(ssp, gcm, method, species, year) %>%
                               summarise(y = mean(y)) %>%
                               ungroup() %>%
                               group_by(year,species) %>%
                               reframe(meany = mean(y), int = sd(y)) %>%
                               mutate(ecoregion = ecoregion))

saved_data_speciesSDM <-  rbind(saved_data_speciesSDM, 
                                simulations %>%
                               dplyr::filter(year %in% c(1990:2100)) %>%
                               group_by(ssp, gcm, method, species, year) %>%
                               summarise(y = mean(y)) %>%
                               ungroup() %>%
                               group_by(year,species, method) %>%
                               reframe(meany = mean(y), int = sd(y)) %>%
                               mutate(ecoregion = ecoregion))



