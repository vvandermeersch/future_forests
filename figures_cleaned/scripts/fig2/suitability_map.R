
# Model agreement on sign of fitness change
sign <- lapply(c("correlative","expert","fitted"), function(m){
  
  
  sim_r <- simulations %>% 
    dplyr::filter(method == m) %>%
    group_by(x,y) %>%
    summarise(suit = mean(mean)) %>%
    rast(crs = "EPSG:4326") %>% 
    project("EPSG:3035")
  
  sign <- ifel(sim_r < 0, -1, 1)
  
  return(sign)
  
}) %>% rast()
neg <- ifel(sign < 0, 1, 0) %>% sum()
neg <- ifel(neg < 3, 0, 1)
pos <- ifel(sign > 0, 1, 0) %>% sum()
pos <- ifel(pos < 3, 0, 1)
agreement <- pos + neg

sim_agreement <- ifel(agreement == 0, 0, NA) %>% 
  as.polygons()  %>% 
  simplifyGeom(tolerance=10000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot
values(sim_agreement) <- "Low"

suit_r <- simulations %>%
  group_by(x,y) %>%
  summarise(suit = mean(mean)) %>%
  rast(crs = "EPSG:4326") %>% 
  project("EPSG:3035")

map_suitability <- ggplot() +
  geom_spatraster(data = suit_r, maxcell = 1e+05) +
  geom_sf_pattern(data = sim_agreement, 
                  aes(pattern = value),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_fill = "grey50",
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement")) +
  geom_spatvector(data = ctr %>%  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE), 
                  color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5561560)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 7018580)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom", legend.box="vertical",
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    plot.margin = margin(t=5.5,b=0,l=0,r=0),
    strip.text = element_text(size = 8, margin = margin(t=0,b=5,l=0,r=0)),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90")) +
  scale_fill_gradient2(
    low = "#a63716",
    mid = "#eef0f1",
    high = "#15607a",
    na.value = "transparent", 
    limits = c(-0.45, 0.45),
    breaks = seq(-0.4, 0.4, 0.2), 
    labels = c("-0.4", "-0.2", "0", "0.2", "0.4")
  ) +
  guides(
    fill = guide_colorbar(order = 2,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(5, "pt"),
                                        legend.key.width  = unit(120, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 1,
                           theme = theme(legend.key.height  = unit(6, "pt"),
                                         legend.text = element_text(size = 7))))
