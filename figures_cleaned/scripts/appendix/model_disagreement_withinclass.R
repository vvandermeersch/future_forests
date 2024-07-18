
ctr <- subset(sim_fitness,1) %>% 
  project("EPSG:3035")
ctr <- ifel(is.na(ctr), 0, 1) %>% as.polygons()

epbm_agr <- ifel(subset(epbm_agreement,1) == 0, 0, NA) %>% as.polygons()
values(epbm_agr) <- "Low"
fpbm_agr <- ifel(subset(fpbm_agreement,1) == 0, 0, NA) %>% as.polygons()
values(fpbm_agr) <- "Low"
csdm_agr <- ifel(subset(csdm_agreement,1) == 0, 0, NA) %>% as.polygons()
values(csdm_agr) <- "Low"

map_disag_withinclass <- ggplot() +
  geom_spatraster(data = subset(sim_fitness,1) %>% project("EPSG:3035"),
                          maxcell = 1e+05) +
  scale_fill_gradient(low = "grey95", high = "grey95", na.value = NA, guide = 'none') +
  geom_sf_pattern(data = epbm_agr %>% project("EPSG:3035"), 
                  aes(pattern = value, pattern_fill ="expert"),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = 45,
                  pattern_density = 0.15,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 1) +
  geom_sf_pattern(data = fpbm_agr %>% project("EPSG:3035"), 
                  aes(pattern = value, pattern_fill ="fitted"),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = 0,
                  pattern_density = 0.15,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 1) +
  geom_sf_pattern(data = csdm_agr %>% project("EPSG:3035"), 
                  aes(pattern = value, pattern_fill ="correlative"),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = -45,
                  pattern_density = 0.15,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement"), guide = 'none') +
  scale_pattern_fill_manual(values = c(correlative = "#457b9d", fitted = "#995D81", expert = "#018530"), 
                            breaks = c("correlative", "fitted", "expert"),
                            labels = c("Correlative SDM", "Fitted PEM", "Expert PEM"),
                            name ="") +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 6806970)) +
  theme_void() +
  theme(
    legend.spacing.x = unit(30, 'pt'),
    legend.box.margin=margin(0,0,0,0),
    plot.margin = margin(t = 2, b = 2, 0,0),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "grey80", fill=NA, linewidth=0.7),
    panel.grid.major = element_line(color = "grey90")) +
  guides(
    pattern_fill = guide_legend(override.aes = list(fill = "white", color = "grey30"),
                           theme = theme(legend.key.height  = unit(8, "pt"),
                                         legend.key.width  = unit(8, "pt"),
                                         legend.text = element_text(size = 7))))



