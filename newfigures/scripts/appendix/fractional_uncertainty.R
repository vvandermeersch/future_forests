
cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm:gcm + sdm:ssp" = "#ddb166",
          "climate" = "#D98B65", "species" = "#6B95B2", "species:all" = "#8E85C4") 
frac_uncertainty <- saved_data_all %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  mutate(ecoregion = factor(ecoregion, levels = rev(c("Boreal", "Alpine", "Continental", "Atlantic", "Mediterranean")))) %>%
  ggplot(aes(x = year)) +
  facet_wrap(~ ecoregion, nrow = 1) +
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
                  ymax = (gcm+ssp+gcm.ssp)/tot*100, fill = "climate")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm+gcm.ssp)/tot*100, fill = "climate")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (gcm)/tot*100, fill = "climate")) +
  coord_cartesian(ylim = c(0,100), xlim = c(2000, 2090), expand = FALSE) +
  theme_bw() + scale_y_continuous(position = "left") +
  labs(y = "Fraction of total variance (%)") +
  scale_x_continuous(breaks = seq(2000, 2090, 15)) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 5.5),
    panel.spacing = unit(10, "points"),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust=1), axis.title.y = element_text(size = 8),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt'),
    strip.background = element_blank()) +
  scale_fill_manual(name = "Source of uncertainty:", values=cols, 
                    breaks = c("climate",  "sdm:gcm + sdm:ssp", "sdm", "species", "species:all", "residuals"),
                    labels = c("Climate (SSP, GCM)", "Climate - SDM", "SDM", "Species", "Species - climate/SDM", "Residuals")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 