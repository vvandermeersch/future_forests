
decsuit <- subset(sim_dist_2090, 3) %>% 
  as.polygons()
names(decsuit) <- "dec"

dist_map_lessmore <- ggplot() +
  geom_spatraster(data = subset(sim_dist_2090, 1)) +
  scale_fill_gradient(low = "grey98", high = NA, na.value = NA, guide="none") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = subset(sim_dist_2090, 2)) +
  scale_fill_manual(
    name = "",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    values = c(`-1`="#a63716", NA, `0`= "#c7cdd1", NA, `1`="#15607a"),  
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable"),
    na.value = NA) +
  geom_sf_pattern(data = decsuit, 
                  aes(pattern = dec, pattern_fill = dec),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.01,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "stripe", NA, `1` = "stripe", NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  scale_pattern_fill_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "#a63716", NA, `1` = "#15607a", NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  guides(
    pattern = guide_legend(override.aes = list(fill = NA))) +
  #scale_x_continuous(limits = c(2431478, 6800000)) +
  #scale_y_continuous(limits = c(1288120, 5571562)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    panel.border = element_rect(colour = "grey80", fill=NA, linewidth=0.7),
    panel.grid.major = element_line(color = "grey90"))
