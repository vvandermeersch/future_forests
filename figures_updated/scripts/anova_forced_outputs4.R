
#-------------------------------#
# Run ANOVA on smoothed outputs #
#-------------------------------#

plan(multisession, workers = 10)
anova_ss <- foreach(yr = min(simulations$year):max(simulations$year), .combine=rbind) %dofuture% {
  
  linfit <- lm(smy ~ gcm + ssp + method + 
                 gcm:ssp + gcm:method + ssp:method, 
               data = simulations %>% dplyr::filter(year == yr))
  anova <- car::Anova(linfit, type = 2)
  
  t(data.frame(append(yr, anova$`Sum Sq`)))
  
} %>% as.data.frame()
plan(sequential);gc()
rownames(anova_ss) <- NULL
colnames(anova_ss) <-  c("year", 
                         "gcm", "ssp", "sdm", 
                         "gcm.ssp", "gcm.sdm", "ssp.sdm",
                         "residuals")

# Interannual variability (can evolve between years)
# sum of squares of the difference between raw outputs and the smoothed ones
ss_intvar <- simulations %>%
  mutate(intvar = (y-smy)^2) %>% 
  group_by(year) %>%
  reframe(intvar = sum(intvar))

# Join with ANOVA results
anova_ss <- anova_ss %>% 
  left_join(ss_intvar, by = join_by(year))

# Total uncertainty
anova_ss$tot <- anova_ss$gcm + anova_ss$ssp + anova_ss$sdm + 
  anova_ss$gcm.ssp + anova_ss$gcm.sdm + anova_ss$ssp.sdm + 
  anova_ss$residuals + anova_ss$intvar

# Uncertainty decomposition
cols <- c("residuals + int. ann. var." = "#FFBB70", "sdm" = "#7469B6", "sdm:gcm + sdm:ssp" = "#E1AFD1",
          "ssp" = "#139474", "gcm:ssp" = "#00b9a4", "gcm" = "#135f94")
unc_decomp <- simulations %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(y)) %>%
  left_join(anova_ss) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals+intvar)/tot*100, fill = "residuals + int. ann. var.")) +
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
  scale_fill_manual(values=cols, breaks = c("gcm", "gcm:ssp", "ssp",  "sdm", "sdm:gcm + sdm:ssp", "residuals + int. ann. var.")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


# Trends and uncertainty sources
trend_unc <- simulations %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  group_by(year) %>%
  reframe(meany = mean(smy), int = sd(smy)) %>%
  left_join(anova_ss, by = join_by(year)) %>%
  ggplot(aes(x = year, y = meany)) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals+intvar)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm+residuals+intvar)/tot),
              fill = "#8E85C4", alpha = 0.85) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+sdm+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "#6B95B2", alpha = 0.8) + 
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp+gcm.sdm+ssp.sdm)/tot),
              fill = "#54896A", alpha = 0.8) + 
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+ssp+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+ssp+gcm.ssp)/tot),
              fill = "#ddb166", alpha = 0.8) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm+gcm.ssp)/tot,
                  ymax = meany + (1.645*int)*(gcm+gcm.ssp)/tot),
              fill = "#a25752", alpha = 0.85) +
  geom_ribbon(aes(ymin = meany - (1.645*int)*(gcm)/tot,
                  ymax = meany + (1.645*int)*(gcm)/tot),
              fill = "#D98B65", alpha = 0.8)+
  geom_line(linewidth = 0.7, linetype = "solid", color = "white", alpha = 0.6)+
  geom_line(alpha = 1, linewidth = 0.3, linetype = "dashed")+
  coord_cartesian(xlim = c(2000, 2090), ylim = c(-0.5,0.75), expand = FALSE) +
  labs(y = "Projected change in suitability\nrelative to 1970-2000") +
  scale_x_continuous(breaks = seq(2000, 2090, 15), labels = c("2000", "", "2030", "", "2060", "", "2090")) +
  theme_bw() +
  theme(
    axis.ticks = element_line(colour = "grey30", linewidth=0.3),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    plot.margin = margin(t = 5.5, b = 5.5, r = 0, l = 5.5),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 6.5), axis.text.y = element_text(size = 6.5),
    axis.title.y = element_text(size = 7.5),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt'),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())







