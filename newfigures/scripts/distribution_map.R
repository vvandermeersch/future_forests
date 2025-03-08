
decsuit <- subset(sim_dist_2090, 3) %>%
  mutate(`2090` = factor(`2090`, levels = c(-2,-1,0,1,2))) 

decsuit[1:3] <- c(1,3,5)

decsuit <- decsuit %>%
  as.polygons() %>% 
  simplifyGeom(tolerance=10000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot

names(decsuit) <- "dec"

ctr <- subset(sim_dist_2090, 1)
ctr <- ifel(is.na(ctr), 0, 1) %>% 
  as.polygons() %>% 
  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot

dist_map_lessmore <- ggplot() +
  geom_spatraster(data = subset(sim_dist_2090, 1),
                  maxcell = 2e+05) +
  scale_fill_gradient(low = "grey98", high = NA, na.value = NA, guide="none") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = subset(sim_dist_2090, 2),
                  maxcell = 2e+05) +
  scale_fill_manual(
    name = "Suitability",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    values = c(`-1`="#a63716", NA, `0`= "#c7cdd1", NA, `1`="#15607a"),  
    labels = c("Extinction", "Decreasing", "No change", "Increasing", "New area"),
    na.value = NA, guide = 'none') +
  ggnewscale::new_scale_fill() +
  geom_sf_pattern(data = decsuit, 
                  aes(pattern = dec, pattern_fill = dec, fill = dec),
                  color = NA, 
                  pattern_colour = NA,
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 0.7) +
  scale_pattern_manual(
    name = "Projected suitability",
    breaks = c(-2, -1, 0, 1, 2),
    values = c(`-2` = "none", `-1` = "stripe", `0` = "none", `1` = "stripe", `2` = "none"),
    labels = c("Extinction", "Decreasing", "No change", "Increasing", "New area"),
    drop = FALSE) +
  scale_pattern_fill_manual(
    name = "Projected suitability",
    breaks = c(-2, -1, 0, 1, 2),
    values = c(`-2` = "#a63716", `-1` = "#a63716", `0` = "#a63716", `1` = "#15607a", `2` = "#15607a"),
    labels = c("Extinction", "Decreasing", "No change", "Increasing", "New area"),
    drop = FALSE) +
  scale_fill_manual(
    name = "Projected suitability",
    breaks = c(-2, -1, 0, 1, 2),
    values = c(`-2` = "#a63716", `-1` = "#c7cdd1", `0` = "#c7cdd1", `1` = "#c7cdd1", `2` = "#15607a"),
    labels = c("Extinction", "Decreasing", "No change", "Increasing", "New area"),
    drop = FALSE) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 6306970)) +
  theme(
    legend.key.height  = unit(15, "pt"),
    legend.key.width = unit(10, "pt"),
    legend.frame = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.position = "right", 
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90"))

