


# Decomposition of species interaction (species:all)
cols <- c("species.ssp" = "#474262", "species.gcm" = "#8E85C4", "species.sdm" = "#d1cee7")
unc_decomp_species <- saved_data_all %>%
  dplyr::filter(year %in% c(1990:2100)) %>%
  mutate(ecoregion = factor(ecoregion, levels = rev(c("Boreal", "Alpine", "Continental", "Atlantic", "Mediterranean")))) %>%
  ggplot(aes(x = year)) +
  facet_wrap(~ ecoregion, nrow = 1) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp+species.gcm+species.sdm)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.sdm")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp+species.gcm)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.gcm")) +
  geom_ribbon(aes(ymin = 0,
                  ymax = (species.ssp)/(species.ssp+species.gcm+species.sdm)*100, fill = "species.ssp")) +
  coord_cartesian(ylim = c(0,100), xlim = c(2000, 2090), expand = FALSE) +
  theme_bw() + scale_y_continuous(position = "left") +
  labs(y = "Fraction of species\ninteraction variance (%)") +
  scale_x_continuous(breaks = seq(2000, 2090, 15)) +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 5.5),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    panel.spacing = unit(10, "points"),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust=1), axis.title.y = element_text(size = 8),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt'),
    strip.background = element_blank()) +
  scale_fill_manual(values=cols, 
                    breaks = c("species.ssp", "species.gcm", "species.sdm"),
                    labels = c("Species - SSP", "Species - GCM", "Species - SDM")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


adjusted_predictions_speciesSDM <- saved_fit_speciesSDM %>%
  mutate(ecoregion = factor(ecoregion, levels = rev(c("Boreal", "Alpine", "Continental", "Atlantic", "Mediterranean"))),
         species = factor(x, levels = c("abies_alba", "fagus_sylvatica", "quercus_petraea", 
                                             "quercus_robur", "quercus_pubescens", "quercus_ilex"))) %>%
  ggplot(aes(x = year)) +
  facet_grid(year ~ ecoregion, switch = "y") +
  geom_hline(aes(yintercept = 0), colour = "white", linewidth=0.4) +
  geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3, linetype = "dashed") +
  geom_pointrange(aes(x = species, 
                      y = predicted, ymin = conf.low, ymax = conf.high,
                      color = group),
                  size = 0.1, linewidth = 0.5) +
  
  geom_line(aes(x = species, y = predicted, group = group, color = group),
            linewidth = 0.2) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 5.5, b = 1.5, r = 5.5, l = 5.5),
    axis.title.x = element_blank(), legend.text = element_text(size = 7),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6.5, angle = 45, vjust = 1, hjust=1), axis.title.y = element_text(size = 8),
    legend.title = element_blank(), legend.key.size = unit(8, 'pt'),
    legend.box.margin = margin(t= -5, b=0, l = 0, r = 0),
    strip.background = element_blank(),
    panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("Silver fir", "Beech", "Sessile oak", "Pedunc. oak",
                              "Pubesc. oak", "Evergreen oak")) +
  scale_y_continuous(position = "right") +
  scale_color_manual(values = c(expert = "#018530", fitted = "#995D81", correlative = "#457b9d"),
                     breaks = c("correlative", "fitted", "expert"),
                     labels = c("Correlative SDM", "Fitted PEM", "Expert PEM")) +
  ylab("Predicted change in suitability")
