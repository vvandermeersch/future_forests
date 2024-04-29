
davos <- color("davos")
distribution_map_2030 <- ggplot() +
  geom_spatraster(data = subset(sim_distribution,1)) +
  scale_fill_gradientn(
    colours = rev(davos(9))[1:8],
    na.value = "transparent", limits = c(0, max(breakseq)),
    breaks = breakseq) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-13.05, 40.05)) +
  theme(
    legend.spacing.x = unit(30, 'pt'),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "grey30", fill=NA, size=0.4)) +
  guides(
    fill = guide_colorbar(order = 1,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(6, "pt"),
                                        legend.key.width  = unit(130, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 2,
                           theme = theme(legend.key.height  = unit(8, "pt"),
                                         legend.text = element_text(size = 7)))) +
  labs(title = "2020-2040")

distribution_map_2060 <- ggplot() +
  geom_spatraster(data = subset(sim_distribution,2)) +
  scale_fill_gradientn(
    colours = rev(davos(9))[1:8],
    na.value = "transparent", limits = c(0, max(breakseq)),
    breaks = breakseq) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-13.05, 40.05)) +
  theme(
    legend.spacing.x = unit(30, 'pt'),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "grey30", fill=NA, size=0.4)) +
  guides(
    fill = guide_colorbar(order = 1,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.3,
                          theme = theme(legend.key.height  = unit(6, "pt"),
                                        legend.key.width  = unit(130, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 2,
                           theme = theme(legend.key.height  = unit(8, "pt"),
                                         legend.text = element_text(size = 7)))) +
  labs(title = "2050-2070")

distribution_map_2090 <- ggplot() +
  geom_spatraster(data = subset(sim_distribution,3)) +
  scale_fill_gradientn(
    colours = rev(davos(9))[1:8],
    na.value = "transparent", limits = c(0, max(breakseq)),
    breaks = breakseq) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-13.05, 40.05)) +
  theme(
    legend.spacing.x = unit(30, 'pt'),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "grey30", fill=NA, size=0.4)) +
  guides(
    fill = guide_colorbar(order = 1,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.3,
                          theme = theme(legend.key.height  = unit(6, "pt"),
                                        legend.key.width  = unit(130, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 2,
                           theme = theme(legend.key.height  = unit(8, "pt"),
                                         legend.text = element_text(size = 7)))) +
  labs(title = "2080-2100")

distribution_maps <- plot_grid(
  plot_grid(distribution_map_2030 + theme(legend.position = 'none'), 
            distribution_map_2060 + theme(legend.position = 'none'), 
            distribution_map_2090 + theme(legend.position = 'none'), nrow = 1),
  get_plot_component(distribution_map_2030, 'guide-box-bottom', return_all = TRUE), 
  ncol = 1, rel_heights = c(1,0.15)
)