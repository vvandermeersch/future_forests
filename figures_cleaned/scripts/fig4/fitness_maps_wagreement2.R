
# contour
ctr <- subset(sim_fitness,1) %>% 
  crop(ext(-10.5, 31.7, 34.6, 71.2)) %>%
  project("EPSG:3035")
ctr <- ifel(is.na(ctr), 0, 1) %>% 
  as.polygons() %>% 
  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot

# process agreement between models
sim_agreement_process2050 <- ifel(subset(sim_agreement,1) == 0, 0, NA) %>% as.polygons()
values(sim_agreement_process2050) <- "Low"
sim_agreement_process2090 <- ifel(subset(sim_agreement,2) == 0, 0, NA) %>% as.polygons()
values(sim_agreement_process2090) <- "Low"

# maps of fitness change, with model agreement (>= 80%)
fitness_map_2050 <- ggplot() +
  geom_raster(data = subset(sim_fitness,1) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% 
                project("EPSG:3035") %>% as.data.frame(xy= TRUE), aes(x = x, y = y, fill = `2050`)) +
  scale_fill_gradient2(
    low = "#a63716",
    mid = "#eef0f1",
    high = "#15607a",
    na.value = "transparent", 
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5), 
    labels = c("-1", "-0.5", "0", "0.5", "1")
  ) +
  geom_sf_pattern(data = sim_agreement_process2050 %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035") %>% 
                    simplifyGeom(tolerance=10000, preserveTopology=TRUE, makeValid=FALSE), 
                  aes(pattern = value),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_fill = "grey50",
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 4) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement")) + 
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 6306970)) +
  theme(
    legend.spacing.x = unit(30, 'pt'),
    legend.box.margin=margin(0,0,0,0),
    plot.margin = margin(t = 2, b = 2, 0,0),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = panel_color, fill=NA, linewidth=1),
    panel.grid.major = element_line(color = "grey90")) +
  guides(
    fill = guide_colorbar(order = 1,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(6, "pt"),
                                        legend.key.width  = unit(150, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 2,
                           theme = theme(legend.key.height  = unit(8, "pt"),
                                         legend.text = element_text(size = 7))))

fitness_map_2090 <- ggplot() +
  geom_raster(data = subset(sim_fitness,2) %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% 
                project("EPSG:3035") %>% as.data.frame(xy= TRUE), aes(x = x, y = y, fill = `2090`)) +
  scale_fill_gradient2(
    low = "#a63716",
    mid = "#eef0f1",
    high = "#15607a",
    na.value = "transparent", 
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25), 
    labels = c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
  ) +
  geom_sf_pattern(data = sim_agreement_process2090 %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035") %>% 
                    simplifyGeom(tolerance=10000, preserveTopology=TRUE, makeValid=FALSE), 
                  aes(pattern = value),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_fill = "grey50", 
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 4) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement")) + 
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 6306970)) +
  theme(
    legend.spacing.x = unit(20, 'pt'), 
    legend.box.margin=margin(0,0,0,0),
    plot.margin = margin(t = 2, b = 2, 0,0),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = panel_color, fill=NA, linewidth=1),
    panel.grid.major = element_line(color = "grey90")) +
  guides(
    fill = guide_colorbar(order = 1,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(5, "pt"),
                                        legend.key.width  = unit(100, "pt"),
                                        legend.text = element_text(size = 6, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 2,
                           theme = theme(legend.key.height  = unit(6, "pt"),
                                         legend.text = element_text(size = 6))))

# 
# fitness_maps <- plot_grid(
#   plot_grid(fitness_map_2030 + theme(legend.position = 'none'), 
#             fitness_map_2060 + theme(legend.position = 'none'), 
#             fitness_map_2090 + theme(legend.position = 'none'), nrow = 1),
#   get_plot_component(fitness_map_2030, 'guide-box-bottom', return_all = TRUE), 
#   ncol = 1, rel_heights = c(1,0.1)
# )
